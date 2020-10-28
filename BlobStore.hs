{-# LANGUAGE TupleSections #-}

module BlobStore
  ( BlobDirStore
  , BlobMapStore
  , blobName
  , blobNameLength
  , BlobStore
  , getBlob
  , listBlobs
  , newBlobDir
  , newBlobMap
  , Password(Pass)
  , StagedBlobHandle(..)
  , stageBlob
  , writeNamePrefixedBlob
  , writeTrustedBlob
  , writeUntrustedBlob
  ) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (bytestringDigest, hmacSha256)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import System.Directory
  ( createDirectoryIfMissing
  , listDirectory
  , removeFile
  , renameFile
  )
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.IO.Temp (openBinaryTempFile)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Instances.ByteString ()

blobNameLength :: Int
blobNameLength = 32

newtype Password =
  Pass BS.ByteString
  deriving (Show)

-- HMAC specifies that keys less than the key length are null-padded up
-- to the key length (longer keys are hashed).  This means that keys that
-- are both less than the key length and differ only by trailing \NULs are
-- equivalent HMAC keys.  We must surface this minor quirk here so that when
-- we ask quickcheck for two different passwords, they actually differ.
instance Eq Password where
  (Pass a) == (Pass b)
    | BS.length a <= blobNameLength && BS.length b <= blobNameLength =
      nullPad a == nullPad b
    | otherwise = a == b
    where
      nullPad s = BS.append s (BS.replicate (blobNameLength - BS.length s) 0)

instance Arbitrary Password where
  arbitrary = Pass <$> arbitrary

newtype ExtantBlobName =
  ExtantBlob BS.ByteString
  deriving (Eq, Ord)

blobName :: Password -> BL.ByteString -> BS.ByteString
blobName (Pass password) blob =
  BL.toStrict $ bytestringDigest $ hmacSha256 (BL.fromStrict password) blob

-- |Lazily write a Lazy ByteString
tee :: Handle -> BL.ByteString -> IO BL.ByteString
tee h stream =
  BL.fromChunks <$> mapM (\x -> BS.hPut h x >> pure x) (BL.toChunks stream)

teeToTempFile ::
     FilePath -> String -> BL.ByteString -> IO (BL.ByteString, FilePath)
teeToTempFile dir template stream =
  bracket
    (openBinaryTempFile dir template)
    (hClose . snd)
    (\(tmpname, tmpfile) -> (, tmpname) <$> tee tmpfile stream)

data StagedBlobHandle =
  StagedBlobHandle
    { blobData :: BL.ByteString
    , commit :: BS.ByteString -> IO ExtantBlobName
    , abort :: IO ()
    }

class BlobStore a where
  stageBlob :: a -> BL.ByteString -> IO StagedBlobHandle
  listBlobs :: a -> IO [ExtantBlobName]
  getBlob :: a -> ExtantBlobName -> IO BL.ByteString

writeNamePrefixedBlob ::
     BlobStore bs
  => bs
  -> Password
  -> BL.ByteString
  -> IO (Maybe ExtantBlobName)
writeNamePrefixedBlob bs password stream =
  let (name, blob) = strictPrefixSplitAt blobNameLength stream
   in writeUntrustedBlob bs password name blob
  where
    strictPrefixSplitAt ::
         Integral a => a -> BL.ByteString -> (BS.ByteString, BL.ByteString)
    -- |Like splitAt, but the prefix is strict
    strictPrefixSplitAt i str =
      let tmp = BL.splitAt (fromIntegral i) str
       in (BL.toStrict (fst tmp), snd tmp)

newtype BlobMapStore =
  BlobMap (IORef (Map.Map ExtantBlobName BL.ByteString))

newBlobMap :: IO BlobMapStore
newBlobMap = BlobMap <$> newIORef Map.empty

instance BlobStore BlobMapStore where
  stageBlob (BlobMap rm) blob =
    pure
      StagedBlobHandle
        { blobData = blob
        , commit =
            \name ->
              let ename = ExtantBlob name
               in do atomicModifyIORef' rm ((, ()) . Map.insert ename blob)
                     pure ename
        , abort = pure ()
        }
  listBlobs (BlobMap rm) = Map.keys <$> readIORef rm
  getBlob (BlobMap rm) name = do
    m <- readIORef rm
    pure $ m Map.! name

newtype BlobDirStore =
  BlobDir FilePath

newBlobDir :: FilePath -> IO BlobDirStore
newBlobDir path = do
  createDirectoryIfMissing True (path </> "incoming")
  pure $ BlobDir path

instance BlobStore BlobDirStore where
  stageBlob bd blob = do
    (teeWrappedBlob, tmpPath) <- teeToTempFile (d </> "incoming") "new" blob
    pure
      StagedBlobHandle
        { blobData = teeWrappedBlob
        , commit =
            \name -> do
              renameFile tmpPath (blobFileName bd name)
              pure (ExtantBlob name)
        , abort = removeFile tmpPath
        }
    where
      (BlobDir d) = bd
  listBlobs (BlobDir d) =
    fmap ExtantBlob . mapMaybe unBlobFileName <$> listDirectory d
  getBlob bd (ExtantBlob name) = BL.readFile (blobFileName bd name)

blobFileName :: BlobDirStore -> BS.ByteString -> FilePath
blobFileName (BlobDir d) blobname = d </> toString (Base64.encode blobname)

unBlobFileName :: FilePath -> Maybe BS.ByteString
unBlobFileName relpath =
  case Base64.decode $ fromString relpath of
    Left _ -> Nothing
    Right n ->
      if BS.length n == blobNameLength
        then Just n
        else Nothing

writeTrustedBlob ::
     BlobStore bs => bs -> Password -> BL.ByteString -> IO ExtantBlobName
writeTrustedBlob bs password blob = do
  staged <- stageBlob bs blob
  commit staged (blobName password (blobData staged))

writeUntrustedBlob ::
     BlobStore bs
  => bs
  -> Password
  -> BS.ByteString
  -> BL.ByteString
  -> IO (Maybe ExtantBlobName)
writeUntrustedBlob bs password expectedHash blob = do
  staged <- stageBlob bs blob
  if expectedHash == blobName password (blobData staged)
    then Just <$> commit staged expectedHash
    else abort staged >> pure Nothing

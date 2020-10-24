{-# LANGUAGE TupleSections #-}

module Bejnarkli
  ( abort
  , blobData
  , blobName
  , blobNameLength
  , commit
  , getBlob
  , listBlobs
  , newUnverifiedBlobDir
  , newUnverifiedBlobMap
  , someFunc
  , stageBlob
  , UnverifiedBlobStore
  , writeNamePrefixedBlob
  ) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (bytestringDigest, hmacSha256)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
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

blobNameLength :: Int
blobNameLength = 32

newtype ExtantBlobName =
  ExtantBlob BS.ByteString
  deriving (Eq, Ord)

blobName :: BS.ByteString -> BL.ByteString -> BS.ByteString
blobName password blob =
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
    { blobData :: IO BL.ByteString
    , commit :: BS.ByteString -> IO ExtantBlobName
    , abort :: IO ()
    }

class UnverifiedBlobStore a where
  stageBlob :: a -> BL.ByteString -> IO StagedBlobHandle
  listBlobs :: a -> IO [ExtantBlobName]
  getBlob :: a -> ExtantBlobName -> IO BL.ByteString

writeNamePrefixedBlob ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> IO ExtantBlobName
writeNamePrefixedBlob ubs stream =
  let (name, blob) = strictPrefixSplitAt blobNameLength stream
   in do sbh <- stageBlob ubs blob
         commit sbh name
  where
    strictPrefixSplitAt ::
         Integral a => a -> BL.ByteString -> (BS.ByteString, BL.ByteString)
    -- |Like splitAt, but the prefix is strict
    strictPrefixSplitAt i str =
      let tmp = BL.splitAt (fromIntegral i) str
       in (BL.toStrict (fst tmp), snd tmp)

newtype BlobMapStore =
  BlobMap (IORef (Map.Map ExtantBlobName BL.ByteString))

newUnverifiedBlobMap :: IO BlobMapStore
newUnverifiedBlobMap = BlobMap <$> newIORef Map.empty

instance UnverifiedBlobStore BlobMapStore where
  stageBlob (BlobMap rm) blob =
    pure
      StagedBlobHandle
        { blobData = pure blob
        , commit =
            \name ->
              let ename = ExtantBlob name
               in do modifyIORef' rm (Map.insert ename blob)
                     pure ename
        , abort = pure ()
        }
  listBlobs (BlobMap rm) = Map.keys <$> readIORef rm
  getBlob (BlobMap rm) name = do
    m <- readIORef rm
    pure $ m Map.! name

newtype BlobDirStore =
  BlobDir FilePath

newUnverifiedBlobDir :: FilePath -> IO BlobDirStore
newUnverifiedBlobDir path = do
  createDirectoryIfMissing True (path </> "incoming")
  pure $ BlobDir path

instance UnverifiedBlobStore BlobDirStore where
  stageBlob bd blob = do
    (teeWrappedBlob, tmpPath) <- teeToTempFile (d </> "incoming") "new" blob
    pure
      StagedBlobHandle
        { blobData = pure teeWrappedBlob
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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-# LANGUAGE TupleSections #-}

module BlobStore
  ( BlobDirStore
  , BlobMapStore
  , blobName
  , blobNameLength
  , BlobStore
  , ExtantBlobName(..)
  , getBlob
  , listBlobs
  , newBlobDir
  , newBlobMap
  , Password(Pass)
  , sinkBlob
  , sinkNamePrefixedBlob
  , sinkTrustedBlob
  , sinkUntrustedBlob
  , StagedBlobHandle(..)
  ) where

import Conduit
  ( ConduitT
  , MonadResource
  , ZipConduit(ZipConduit)
  , await
  , getZipConduit
  , liftIO
  , sourceFile
  )
import Control.Monad.Trans.Resource (ResourceT)
import qualified Crypto.Hash.Algorithms
import Crypto.MAC.HMAC (HMAC)
import Crypto.MAC.HMAC.Conduit (sinkHMAC)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString, toString)
import Data.Conduit (bracketP)
import Data.Conduit.Combinators (sinkHandle, sinkLazy, sourceLazy, takeExactlyE)
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
import System.IO (hClose)
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

blobName ::
     MonadResource m => Password -> ConduitT BS.ByteString o m BS.ByteString
blobName (Pass password) = do
  digest <- sinkHMAC password
  pure $ BA.convert (digest :: HMAC Crypto.Hash.Algorithms.SHA256)

data StagedBlobHandle =
  StagedBlobHandle
    { commit :: BS.ByteString -> IO ExtantBlobName
    , abort :: IO ()
    }

class BlobStore a where
  sinkBlob ::
       MonadResource m => a -> ConduitT BS.ByteString o m StagedBlobHandle
  listBlobs :: a -> IO [ExtantBlobName]
  getBlob :: a -> ExtantBlobName -> ConduitT i BS.ByteString (ResourceT IO) ()

sinkNamePrefixedBlob ::
     (BlobStore bs, MonadResource m)
  => bs
  -> Password
  -> ConduitT BS.ByteString o m (Maybe ExtantBlobName)
sinkNamePrefixedBlob bs password = do
  name <- takeExactlyE blobNameLength await
  case name of
    Just aname
      | BS.length aname == blobNameLength -> sinkUntrustedBlob bs password aname
    _ -> pure Nothing

newtype BlobMapStore =
  BlobMap (IORef (Map.Map ExtantBlobName BL.ByteString))

newBlobMap :: IO BlobMapStore
newBlobMap = BlobMap <$> newIORef Map.empty

instance BlobStore BlobMapStore where
  sinkBlob (BlobMap rm) = do
    blob <- sinkLazy
    pure
      StagedBlobHandle
        { commit =
            \name ->
              let ename = ExtantBlob name
               in do atomicModifyIORef' rm ((, ()) . Map.insert ename blob)
                     pure ename
        , abort = pure ()
        }
  listBlobs (BlobMap rm) = Map.keys <$> readIORef rm
  getBlob (BlobMap rm) name = do
    m <- liftIO $ readIORef rm
    sourceLazy $ m Map.! name

newtype BlobDirStore =
  BlobDir FilePath

newBlobDir :: FilePath -> IO BlobDirStore
newBlobDir path = do
  createDirectoryIfMissing True (path </> "incoming")
  pure $ BlobDir path

instance BlobStore BlobDirStore where
  sinkBlob bd =
    bracketP
      (openBinaryTempFile (d </> "incoming") "new")
      (hClose . snd)
      (\(tmpPath, tmpHandle) -> do
         sinkHandle tmpHandle
         pure
           StagedBlobHandle
             { commit =
                 \name -> do
                   renameFile tmpPath (blobFileName bd name)
                   pure (ExtantBlob name)
             , abort = removeFile tmpPath
             })
    where
      (BlobDir d) = bd
  listBlobs (BlobDir d) =
    fmap ExtantBlob . mapMaybe unBlobFileName <$> listDirectory d
  getBlob bd (ExtantBlob name) = sourceFile (blobFileName bd name)

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

sinkTrustedBlob ::
     (BlobStore bs, MonadResource m)
  => bs
  -> Password
  -> ConduitT BS.ByteString o m ExtantBlobName
sinkTrustedBlob bs password = do
  (staged, name) <-
    getZipConduit
      (ZipConduit ((,) <$> sinkBlob bs) <*> ZipConduit (blobName password))
  liftIO $ commit staged name

sinkUntrustedBlob ::
     (BlobStore bs, MonadResource m)
  => bs
  -> Password
  -> BS.ByteString
  -> ConduitT BS.ByteString o m (Maybe ExtantBlobName)
sinkUntrustedBlob bs password expectedHash = do
  (staged, name) <-
    getZipConduit
      (ZipConduit ((,) <$> sinkBlob bs) <*> ZipConduit (blobName password))
  liftIO $
    if expectedHash == name
      then Just <$> commit staged expectedHash
      else abort staged >> pure Nothing

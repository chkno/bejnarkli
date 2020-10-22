module Bejnarkli
  ( abort
  , blobName
  , blobNameLength
  , commit
  , getBlob
  , newUnverifiedBlobDir
  , newUnverifiedBlobMap
  , someFunc
  , stageBlob
  , UnverifiedBlobStore
  , writeNamePrefixedBlob
  ) where

import qualified Crypto.Hash.Algorithms
import Crypto.Hash.IO (hashDigestSize)
import Crypto.MAC.HMAC as HMAC
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString, toString)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import System.Directory (createDirectoryIfMissing, listDirectory, renameFile)
import System.FilePath ((</>))
import System.IO (hSetBinaryMode)
import System.IO.SafeWrite (withOutputFile)
import System.IO.Temp (openBinaryTempFile)

type BlobHMACAlgorithm = Crypto.Hash.Algorithms.SHA256

blobHMACAlgorithm = Crypto.Hash.Algorithms.SHA256

blobNameLength = hashDigestSize blobHMACAlgorithm

newtype ExtantBlobName =
  ExtantBlob BS.ByteString
  deriving (Eq, Ord)

blobName :: BS.ByteString -> BL.ByteString -> BS.ByteString
blobName password blob =
  let ctx = HMAC.initialize password :: HMAC.Context BlobHMACAlgorithm
   in BA.convert $ HMAC.finalize $ HMAC.updates ctx $ BL.toChunks blob

data StagedBlobHandle =
  StagedBlobHandle
    { commit :: IO ExtantBlobName
    , abort :: IO ()
    }

class UnverifiedBlobStore a where
  stageBlob :: a -> BS.ByteString -> BL.ByteString -> StagedBlobHandle
  listBlobs :: a -> IO [ExtantBlobName]
  getBlob :: a -> ExtantBlobName -> IO BL.ByteString

writeNamePrefixedBlob ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> IO ExtantBlobName
writeNamePrefixedBlob ubs stream =
  commit (uncurry (stageBlob ubs) $ strictPrefixSplitAt blobNameLength stream)
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
  stageBlob (BlobMap rm) name blob =
    StagedBlobHandle
      { commit =
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
  createDirectoryIfMissing True path
  pure $ BlobDir path

instance UnverifiedBlobStore BlobDirStore where
  stageBlob bd name blob =
    StagedBlobHandle
      { commit =
          withOutputFile
            (blobFileName bd name)
            (\tmpfile -> do
               hSetBinaryMode tmpfile True
               BL.hPut tmpfile blob
               pure $ ExtantBlob name)
      , abort = pure ()
      }
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

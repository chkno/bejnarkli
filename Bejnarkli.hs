module Bejnarkli
  ( blobNameLength
  , BlobStore
  , getBlob
  , newBlobMap
  , newBlobDir
  , someFunc
  , writeBlob
  , writeNamePrefixedBlob
  ) where

import Control.Error.Util (hush)
import Control.Exception (bracket)
import qualified Crypto.Hash.Algorithms
import Crypto.Hash.IO (hashDigestSize)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString, toString)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import System.Directory (createDirectoryIfMissing, listDirectory, renameFile)
import System.FilePath
import System.IO (hClose)
import System.IO.Temp (openBinaryTempFile)

blobHMACAlgorithm = Crypto.Hash.Algorithms.SHA256

blobNameLength = hashDigestSize blobHMACAlgorithm

newtype ExtantBlobName =
  ExtantBlob BS.ByteString
  deriving (Eq, Ord)

class BlobStore a where
  writeBlob :: a -> BS.ByteString -> BL.ByteString -> IO ExtantBlobName
  listBlobs :: a -> IO [ExtantBlobName]
  getBlob :: a -> ExtantBlobName -> IO BL.ByteString
  writeNamePrefixedBlob :: a -> BL.ByteString -> IO ExtantBlobName
  writeNamePrefixedBlob bs stream =
    uncurry (writeBlob bs) $ strictPrefixSplitAt blobNameLength stream
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
  writeBlob (BlobMap rm) name blob =
    let ename = ExtantBlob name
     in do modifyIORef' rm (Map.insert ename blob)
           pure ename
  listBlobs (BlobMap rm) = Map.keys <$> readIORef rm
  getBlob (BlobMap rm) name = do
    m <- readIORef rm
    pure $ m Map.! name

newtype BlobDirStore =
  BlobDir FilePath

newBlobDir :: FilePath -> IO BlobDirStore
newBlobDir path = do
  createDirectoryIfMissing True $ path </> "incoming"
  pure $ BlobDir path

instance BlobStore BlobDirStore where
  writeBlob bd name blob =
    bracket
      (openBinaryTempFile (d </> "incoming") "new")
      (hClose . snd)
      (\(tmpname, tmpfile) -> do
         BL.hPut tmpfile blob
         renameFile tmpname (blobFileName bd name)
         pure $ ExtantBlob name)
    where
      (BlobDir d) = bd
  listBlobs (BlobDir d) =
    fmap ExtantBlob . mapMaybe unBlobFileName <$> listDirectory d
  getBlob bd (ExtantBlob name) = BL.readFile (blobFileName bd name)

blobFileName :: BlobDirStore -> BS.ByteString -> FilePath
blobFileName (BlobDir d) blobname = d </> toString (Base64.encode blobname)

unBlobFileName :: FilePath -> Maybe BS.ByteString
unBlobFileName relpath = hush $ Base64.decode $ fromString relpath

someFunc :: IO ()
someFunc = putStrLn "someFunc"

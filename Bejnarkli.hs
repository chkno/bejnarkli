module Bejnarkli
  ( blobName
  , blobNameLength
  , BlobStore
  , getBlob
  , newBlobMap
  , newBlobDir
  , someFunc
  , writeBlob
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
  createDirectoryIfMissing True path
  pure $ BlobDir path

instance BlobStore BlobDirStore where
  writeBlob bd name blob =
    withOutputFile
      (blobFileName bd name)
      (\tmpfile -> do
         hSetBinaryMode tmpfile True
         BL.hPut tmpfile blob
         pure $ ExtantBlob name)
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

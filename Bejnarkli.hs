module Bejnarkli
  ( BlobStore
  , getBlob
  , newBlobMap
  , someFunc
  , writeBlob
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as Map
import System.Directory
import System.FilePath

data ExtantBlobName = ExtantBlob String deriving (Eq, Ord)

class BlobStore a where
  writeBlob    :: a -> String -> BL.ByteString -> IO ExtantBlobName
  listBlobs    :: a -> IO [ExtantBlobName]
  getBlob      :: a -> ExtantBlobName -> IO BL.ByteString


data BlobMapStore = BlobMap (IORef (Map.Map ExtantBlobName BL.ByteString))
newBlobMap :: IO BlobMapStore
newBlobMap = BlobMap <$> newIORef Map.empty
instance BlobStore BlobMapStore where
  writeBlob (BlobMap rm) name blob  = let
      ename = ExtantBlob name
      in pure $ seq (modifyIORef' rm (Map.insert ename blob)) ename
  listBlobs (BlobMap rm)            = Map.keys <$> readIORef rm
  getBlob   (BlobMap rm) name       = do
    m <- readIORef rm
    pure $ m Map.! name



data BlobDirStore = BlobDir FilePath
newBlobDir :: FilePath -> IO BlobDirStore
newBlobDir path = seq (createDirectoryIfMissing True path) (pure $ BlobDir path)
instance BlobStore BlobDirStore where
  writeBlob (BlobDir d) name blob         = pure $ seq (BL.writeFile (d </> name) blob) (ExtantBlob name)
  listBlobs (BlobDir d)                   = fmap ExtantBlob <$> listDirectory d
  getBlob   (BlobDir d) (ExtantBlob name) = BL.readFile (d </> name)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

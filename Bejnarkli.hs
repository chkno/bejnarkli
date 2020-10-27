module Bejnarkli
  ( bejnarkliServer
  , someFunc
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (fromString)

import BlobStore (BlobStore, Password, writeNamePrefixedBlob)

bejnarkliServer ::
     BlobStore blobstore
  => blobstore
  -> Password
  -> BL.ByteString
  -> IO BL.ByteString
bejnarkliServer bs password stream = do
  result <- writeNamePrefixedBlob bs password stream
  case result of
    Just _ -> pure $ fromString "y"
    Nothing -> pure $ fromString "n"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

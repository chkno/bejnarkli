module Bejnarkli
  ( bejnarkliServer
  , protocolVersion
  , someFunc
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (ord)
import Data.Word (Word8)

import BlobStore (BlobStore, Password, writeNamePrefixedBlob)

protocolVersion :: Word8
protocolVersion = fromIntegral $ ord 'B'

bejnarkliServer ::
     BlobStore blobstore
  => blobstore
  -> Password
  -> BL.ByteString
  -> IO BL.ByteString
bejnarkliServer bs password wireStream =
  let (version, blobStream) = BL.splitAt 1 wireStream
   in case BL.unpack version of
        [wireVersion]
          | wireVersion == protocolVersion -> do
            result <- writeNamePrefixedBlob bs password blobStream
            case result of
              Just _ -> pure $ fromString "y"
              Nothing -> pure $ fromString "n"
        _ -> pure $ fromString "n"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

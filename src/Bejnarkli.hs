module Bejnarkli
  ( bejnarkliServer
  , bejnarkliClient
  , defaultPort
  , protocolVersion
  ) where

import Conduit
  ( ConduitT
  , MonadResource
  , Void
  , ZipConduit(ZipConduit)
  , (.|)
  , await
  , getZipConduit
  , yield
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U8S
import Data.Char (ord)
import Data.Conduit.Combinators (headE)
import Data.Word (Word8)

import BlobStore (BlobStore, Password, sinkNamePrefixedBlob)

protocolVersion :: Word8
protocolVersion = fromIntegral $ ord 'B'

defaultPort :: Int
defaultPort = 8934

bejnarkliServer
  :: (BlobStore blobstore, MonadResource m)
  => blobstore
  -> Password
  -> ConduitT BS.ByteString BS.ByteString m ()
bejnarkliServer bs password = do
  wireVersion <- headE
  if wireVersion == Just protocolVersion
    then do
      result <- sinkNamePrefixedBlob bs password
      case result of
        Just _ -> yield $ U8S.fromString "y"
        Nothing -> yield $ U8S.fromString "n"
    else yield $ U8S.fromString "n"

idC :: Monad m => ConduitT i i m ()
idC = await >>= maybe (return ()) (\a -> yield a >> idC)

prependSource :: Monad m => i -> ConduitT i i m ()
prependSource prefix =
  getZipConduit (ZipConduit (yield prefix) *> ZipConduit idC)

parseResponse :: Monad m => ConduitT BS.ByteString Void m Bool
parseResponse = (== Just (U8S.fromString "y")) <$> await

bejnarkliClient
  :: Monad m
  => ConduitT BS.ByteString BS.ByteString m ()
  -> BS.ByteString
  -> ConduitT BS.ByteString Void m Bool
bejnarkliClient transport blobHash
  = prependSource (BS.append (BS.pack [protocolVersion]) blobHash)
 .| transport
 .| parseResponse

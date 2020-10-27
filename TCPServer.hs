{-# LANGUAGE LambdaCase #-}

module TCPServer
  ( tCPServer
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Network.Simple.TCP
  ( HostPreference(HostAny)
  , Socket
  , recv
  , sendLazy
  , serve
  )

import Bejnarkli (bejnarkliServer)
import BlobStore (BlobStore)

drainSocket :: Socket -> IO BL.ByteString
drainSocket socket = BL.fromChunks <$> drain
  where
    drain :: IO [BS.ByteString]
    drain =
      recv socket defaultChunkSize >>= \case
        Just chunk -> (chunk :) <$> drain
        Nothing -> pure []

tCPServer :: BlobStore bs => String -> bs -> BS.ByteString -> IO ()
tCPServer port bs password =
  serve
    HostAny
    port
    (\(socket, _) -> do
       request <- drainSocket socket
       response <- bejnarkliServer bs password request
       sendLazy socket response)

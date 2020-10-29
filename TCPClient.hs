module TCPClient
  ( tCPClient
  ) where

import qualified Data.ByteString.Lazy as BL
import Network.Simple.TCP (connect, recv, sendLazy)
import Network.Socket (ShutdownCmd(ShutdownSend), shutdown)

tCPClient :: String -> String -> BL.ByteString -> IO BL.ByteString
tCPClient port host request =
  connect
    host
    port
    (\(socket, _) -> do
       sendLazy socket request
       shutdown socket ShutdownSend
       response <- recv socket 1
       case response of
         Just s -> pure $ BL.fromStrict s
         Nothing -> pure BL.empty)

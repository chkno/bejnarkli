module TCPServer
  ( tCPServer
  ) where

import Conduit ((.|), runConduitRes)
import Data.Conduit.Network
  ( HostPreference
  , appSink
  , appSource
  , runTCPServer
  , serverSettings
  )
import Data.String (fromString)
import Bejnarkli (bejnarkliServer)
import BlobStore (BlobStore, Password)

tCPServer :: BlobStore bs => Int -> bs -> Password -> IO ()
tCPServer port bs password =
  runTCPServer
    (serverSettings port (fromString "*" :: HostPreference))
    (\appdata -> runConduitRes
     $ appSource appdata .| bejnarkliServer bs password .| appSink appdata)

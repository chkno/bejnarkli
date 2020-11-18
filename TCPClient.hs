module TCPClient
  ( retryingTCPClient
  ) where

import Conduit (ConduitT, (.|), liftIO, runConduitRes)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U8S
import Data.Conduit.Network
  ( AppData
  , appSink
  , appSource
  , clientSettings
  , runTCPClient
  )
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Streaming.Network (appRawSocket)
import Network.Socket (ShutdownCmd(ShutdownSend), shutdown)
import Network.URI
  ( parseURI
  , uriAuthority
  , uriFragment
  , uriPath
  , uriPort
  , uriQuery
  , uriRegName
  , uriScheme
  , uriUserInfo
  )
import System.FilePath ((</>))
import Text.Read (readMaybe)

import Bejnarkli (bejnarkliClient)
import BlobStore (BlobStore, ExtantBlobName(ExtantBlob), getBlob)
import PersistentOnce (once)
import RetryQueue (mapChanWithBackoff)

-- We use parseURI rather than just splitting on : because IPv6 literals
parsePeerName :: Int -> String -> (Int, String)
parsePeerName defaultPort name =
  case parseURI ("bejnarkli://" ++ name) of
    Just uri
      | uriScheme uri == "bejnarkli:" &&
          uriPath uri == "" && uriQuery uri == "" && uriFragment uri == "" ->
        case uriAuthority uri of
          Just auth
            | uriUserInfo auth == "" ->
              case stripPrefix ":" (uriPort auth) of
                Just portString ->
                  case readMaybe portString :: Maybe Int of
                    Just port -> (port, uriRegName auth)
                    _ -> (defaultPort, name)
                _ -> (defaultPort, name)
          _ -> (defaultPort, name)
    _ -> (defaultPort, name)

tCPClient ::
     Int
  -> String
  -> BS.ByteString
  -> ConduitT () BS.ByteString (ResourceT IO) ()
  -> IO Bool
tCPClient defaultPort hostString blobHash blobData =
  let (port, host) = parsePeerName defaultPort hostString
   in runTCPClient (clientSettings port (U8S.fromString host)) app
  where
    app :: AppData -> IO Bool
    app appdata =
      runConduitRes $
      blobData .|
      bejnarkliClient
        (do appSink appdata
            liftIO (shutdown (fromJust (appRawSocket appdata)) ShutdownSend)
            appSource appdata)
        blobHash

-- Maybe make these flags?
retryIncrement :: Float
retryIncrement = 1.5

retryMinDelay :: Float
retryMinDelay = 0.1

retryMaxDelay :: Float
retryMaxDelay = 600

retryingTCPClient ::
     BlobStore bs
  => FilePath
  -> Int
  -> String
  -> IO ((bs, ExtantBlobName) -> IO ())
retryingTCPClient dataDir defaultPort hostString = do
  chan <- liftIO newChan
  _ <-
    liftIO $
    mapChanWithBackoff
      retryIncrement
      retryMinDelay
      retryMaxDelay
      attemptSend
      chan
  pure $ writeChan chan
  where
    attemptSend :: BlobStore bs => (bs, ExtantBlobName) -> IO Bool
    attemptSend (bs, ename@(ExtantBlob hash)) =
      once
        (dataDir </> ".once")
        (BS.concat [hash, separator, U8S.fromString hostString]) $
      tCPClient defaultPort hostString hash (getBlob bs ename)
    separator :: BS.ByteString
    separator = U8S.fromString " "

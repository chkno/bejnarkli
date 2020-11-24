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
import RetryQueue
  ( RetryParams(RetryParams)
  , increment
  , maxDelay
  , minDelay
  , retryQueue
  )

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
retryParams :: RetryParams
retryParams = RetryParams {increment = 1.5, minDelay = 0.1, maxDelay = 600}

retrying :: (a -> IO Bool) -> IO (a -> IO ())
retrying action = do
  chan <- liftIO newChan
  _ <- liftIO $ retryQueue retryParams action chan
  pure $ writeChan chan

retryingTCPClient ::
     BlobStore bs
  => FilePath
  -> Int
  -> String
  -> IO ((bs, ExtantBlobName) -> IO ())
retryingTCPClient dataDir defaultPort hostString =
  retrying $ attemptSend dataDir defaultPort hostString

attemptSend ::
     BlobStore bs
  => FilePath
  -> Int
  -> String
  -> (bs, ExtantBlobName)
  -> IO Bool
attemptSend dataDir defaultPort hostString (bs, ename@(ExtantBlob hash)) =
  once
    (dataDir </> ".once")
    (BS.concat [hash, separator, U8S.fromString hostString]) $
  tCPClient defaultPort hostString hash (getBlob bs ename)

separator :: BS.ByteString
separator = U8S.fromString " "

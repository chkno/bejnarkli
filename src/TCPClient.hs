module TCPClient
  ( asyncRetryingTCPClient
  , retryingTCPClient
  ) where

import Conduit (ConduitT, (.|), liftIO, runConduitRes)
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
import Retry
  ( RetryParams(RetryParams)
  , increment
  , maxDelay
  , minDelay
  , retryQueue
  , retryWithDelay
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

-- |Try to send this blob to this host
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

-- |Keep trying to send this blob to this host until successful
retryingTCPClient ::
     Int
  -> String
  -> BS.ByteString
  -> ConduitT () BS.ByteString (ResourceT IO) ()
  -> IO ()
retryingTCPClient defaultPort hostString blobHash blobData =
  retryWithDelay retryParams $
  tCPClient defaultPort hostString blobHash blobData

-- |This produces an 'enqueue' function.  Blobs enqueued are sent one at a time
-- and share the exponential backoff retry-state.
--
-- Failed transfers are retried until they succeed or until the process exits.
-- Successful transfers are logged in dataDir, and prevent this blob ever
-- being sent to this host ever again, making it safe to re-enqueue blobs
-- in separate runs without consuming network resources.
asyncRetryingTCPClient ::
     BlobStore bs
  => FilePath
  -> Int
  -> String
  -> IO ((bs, ExtantBlobName) -> IO ())
asyncRetryingTCPClient dataDir defaultPort hostString =
  retryQueue retryParams $ attemptSend dataDir defaultPort hostString

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

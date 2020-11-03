module Bejnarkli
  ( bejnarkliServer
  , bejnarkliClient
  , protocolVersion
  , someFunc
  ) where

import Conduit (ConduitT, MonadResource, yield)
import Control.Concurrent.Chan (newChan, writeChan)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as U8L
import qualified Data.ByteString.UTF8 as U8S
import Data.Char (ord)
import Data.Conduit.Combinators (headE)
import Data.List (stripPrefix)
import Data.Word (Word8)
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
import Text.Read (readMaybe)

import BlobStore (BlobStore, Password, sinkNamePrefixedBlob)
import Queue (mapChanWithBackoff)
import TCPClient (tCPClient)

protocolVersion :: Word8
protocolVersion = fromIntegral $ ord 'B'

bejnarkliServer ::
     (BlobStore blobstore, MonadResource m)
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

-- We use parseURI rather than just splitting on : because IPv6 literals
parsePeerName :: Int -> String -> (String, Int)
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
                    Just port -> (uriRegName auth, port)
                    _ -> (name, defaultPort)
                _ -> (name, defaultPort)
          _ -> (name, defaultPort)
    _ -> (name, defaultPort)

-- Maybe make these flags?
retryIncrement :: Float
retryIncrement = 1.5

retryMinDelay :: Float
retryMinDelay = 0.1

retryMaxDelay :: Float
retryMaxDelay = 600

bejnarkliClient :: Int -> String -> IO (BL.ByteString -> IO ())
bejnarkliClient defaultPort hostString =
  let (host, port) = parsePeerName defaultPort hostString
   in do chan <- newChan
         _ <-
           mapChanWithBackoff
             retryIncrement
             retryMinDelay
             retryMaxDelay
             (\stream -> do
                response <- tCPClient (show port) host stream
                pure $ response == U8L.fromString "y")
             chan
         pure $ writeChan chan

someFunc :: IO ()
someFunc = putStrLn "someFunc"

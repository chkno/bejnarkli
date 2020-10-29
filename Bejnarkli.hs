module Bejnarkli
  ( bejnarkliServer
  , bejnarkliClient
  , protocolVersion
  , someFunc
  ) where

import Control.Concurrent.Chan (newChan, writeChan)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (ord)
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

import BlobStore (BlobStore, Password, writeNamePrefixedBlob)
import Queue (mapChanWithBackoff)
import TCPClient (tCPClient)

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

-- We use parseURI rather than just splitting on : because IPv6 literals
parsePeerName :: String -> String -> (String, String)
parsePeerName defaultPort name =
  case parseURI ("bejnarkli://" ++ name) of
    Just uri
      | uriScheme uri == "bejnarkli:" &&
          uriPath uri == "" && uriQuery uri == "" && uriFragment uri == "" ->
        case uriAuthority uri of
          Just auth
            | uriUserInfo auth == "" -> (uriRegName auth, uriPort auth)
          _ -> (name, defaultPort)
    _ -> (name, defaultPort)

-- Maybe make these flags?
retryIncrement :: Float
retryIncrement = 1.5

retryMinDelay :: Float
retryMinDelay = 0.1

retryMaxDelay :: Float
retryMaxDelay = 600

bejnarkliClient :: String -> String -> IO (BL.ByteString -> IO ())
bejnarkliClient defaultPort hostString =
  let (host, port) = parsePeerName defaultPort hostString
   in do chan <- newChan
         _ <-
           mapChanWithBackoff
             retryIncrement
             retryMinDelay
             retryMaxDelay
             (\stream -> do
                response <- tCPClient port host stream
                pure $ response == fromString "y")
             chan
         pure $ writeChan chan

someFunc :: IO ()
someFunc = putStrLn "someFunc"

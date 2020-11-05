module Main
  ( main
  ) where

import BlobStore (Password(Pass), newBlobDir)
import Data.ByteString.UTF8 (fromString)
import Options.Applicative
  ( Parser
  , (<**>)
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , many
  , option
  , short
  , showDefault
  , strOption
  , value
  )
import Options.Applicative.Types (ParserInfo)
import ReplicatingBlobStore (ReplicatingBlobStore(ReplicatingBlobStore))

import TCPClient (retryingTCPClient)
import TCPServer (tCPServer)

data Args =
  Args
    { blobdir :: String
    , password :: String
    , peers :: [String]
    , port :: Int
    }

parser :: Parser Args
parser =
  Args <$>
  strOption
    (long "blobdir" <>
     short 'd' <>
     value "blobs" <> showDefault <> help "Where to store the blobs") <*>
  strOption (long "password" <> short 'p' <> help "Transfer password") <*>
  many
    (strOption
       (long "peer" <>
        help
          "Addresses of other servers to forward to.  Repeat for multiple peers.")) <*>
  option
    auto
    (long "port" <>
     value 8934 <>
     showDefault <>
     help
       "TCP port.  This is both the port on which this instance will listen and the default port for connecting to peers with unspecified ports.")

parserInfo :: ParserInfo Args
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "bejnarkli - Transfer blobs around")

main :: IO ()
main = do
  args <- execParser parserInfo
  localBS <- newBlobDir (blobdir args)
  peerClients <- mapM (retryingTCPClient (port args)) (peers args)
  let replicatingBS = ReplicatingBlobStore peerClients localBS
   in tCPServer (port args) replicatingBS (Pass (fromString (password args)))

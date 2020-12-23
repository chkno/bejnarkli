module Main
  ( main
  ) where

import qualified Data.ByteString as BS
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
import Bejnarkli (defaultPort)
import BlobStore (Password(Pass), newBlobDir)
import ReplicatingBlobStore (ReplicatingBlobStore(ReplicatingBlobStore))
import Retransmit (retransmit)
import TCPClient (asyncRetryingTCPClient)
import TCPServer (tCPServer)

data Args =
  Args
  { blobdir :: String
  , passwordFile :: String
  , peers :: [String]
  , port :: Int
  }

parser :: Parser Args
parser =
  Args
  <$> strOption
    (long "blobdir"
     <> short 'd'
     <> value "blobs"
     <> showDefault
     <> help "Where to store the blobs")
  <*> strOption
    (long "passwordfile"
     <> short 'p'
     <> help "File containing the transfer password")
  <*> many
    (strOption
       (long "peer"
        <> help
          "Addresses of other servers to forward to.  Repeat for multiple peers."))
  <*> option
    auto
    (long "port"
     <> value defaultPort
     <> showDefault
     <> help
       "TCP port.  This is both the port on which this instance will listen and the default port for connecting to peers with unspecified ports.")

parserInfo :: ParserInfo Args
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "bejnarkli - Transfer blobs around")

main :: IO ()
main = do
  args <- execParser parserInfo
  password <- Pass <$> BS.readFile (passwordFile args)
  localBS <- newBlobDir $ blobdir args
  peerClients <- traverse
    (asyncRetryingTCPClient (blobdir args) (port args))
    (peers args)
  _ <- retransmit localBS peerClients
  let replicatingBS = ReplicatingBlobStore peerClients localBS
    in tCPServer (port args) replicatingBS password

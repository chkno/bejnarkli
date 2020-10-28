module Main
  ( main
  ) where

import BlobStore (Password(Pass), newBlobDir)
import Data.ByteString.UTF8 (fromString)
import Options.Applicative
  ( Parser
  , (<**>)
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , many
  , short
  , showDefault
  , strOption
  , value
  )
import Options.Applicative.Types (ParserInfo)
import ReplicatingBlobStore (ReplicatingBlobStore(ReplicatingBlobStore))

import Bejnarkli (bejnarkliClient)
import TCPServer (tCPServer)

data Args =
  Args
    { blobdir :: String
    , password :: String
    , peers :: [String]
    , port :: String
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
    (strOption (long "peers" <> help "Hostnames of other servers to forward to")) <*>
  strOption
    (long "port" <>
     value "8934" <> showDefault <> help "TCP port on which to listen")

parserInfo :: ParserInfo Args
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "bejnarkli - Transfer blobs around")

main :: IO ()
main = do
  args <- execParser parserInfo
  localBS <- newBlobDir (blobdir args)
  peerClients <- mapM (bejnarkliClient (port args)) (peers args)
  let replicatingBS = ReplicatingBlobStore peerClients localBS
   in tCPServer (port args) replicatingBS (Pass (fromString (password args)))

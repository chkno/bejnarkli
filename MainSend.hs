module Main
  ( main
  ) where

import Conduit ((.|), runConduitRes, sourceFile)
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
  , metavar
  , short
  , strArgument
  , strOption
  )
import Options.Applicative.Types (ParserInfo)

import Bejnarkli (defaultPort)
import BlobStore (Password(Pass), blobName)
import TCPClient (retryingTCPClient)

data Args =
  Args
    { password :: String
    , file :: FilePath
    , server :: String
    }

parser :: Parser Args
parser =
  Args <$> strOption (long "password" <> short 'p' <> help "Transfer password") <*>
  strArgument (metavar "FILE" <> help "File containing the blob to be sent") <*>
  strArgument (metavar "SERVER" <> help "Server to send the file to")

parserInfo :: ParserInfo Args
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "bejnarkli-send - Send a blob to a bejnarkli server")

main :: IO ()
main = do
  args <- execParser parserInfo
  hash <-
    runConduitRes $
    sourceFile (file args) .| blobName (Pass (fromString (password args)))
  retryingTCPClient defaultPort (server args) hash (sourceFile (file args))

module Main
  ( main
  ) where

import Conduit ((.|), runConduitRes, sourceFile)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromMaybe)
import Options.Applicative
  ( Parser
  , ReadM
  , (<**>)
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , optional
  , readerError
  , short
  , some
  , strArgument
  , strOption
  )
import Options.Applicative.Types (ParserInfo)

import Async (atLeast_)
import Bejnarkli (defaultPort)
import BlobStore (Password(Pass), blobName)
import TCPClient (retryingTCPClient)

data Args =
  Args
    { durability :: Maybe Int
    , password :: String
    , file :: FilePath
    , servers :: [String]
    }

validDurability :: ReadM Int
validDurability = do
  i <- auto
  if i > 0
    then pure i
    else readerError "Durability must be greater than 0"

parser :: Parser Args
parser =
  Args <$>
  optional
    (option
       validDurability
       (long "durability" <>
        short 'n' <> help "Exit after successfully sending to this many servers")) <*>
  strOption (long "password" <> short 'p' <> help "Transfer password") <*>
  strArgument (metavar "FILE" <> help "File containing the blob to be sent") <*>
  some (strArgument (metavar "SERVERS..." <> help "Server to send the file to"))

parserInfo :: ParserInfo Args
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "bejnarkli-send - Send a blob to bejnarkli servers")

main :: IO ()
main = do
  args <- execParser parserInfo
  hash <-
    runConduitRes $
    sourceFile (file args) .| blobName (Pass (fromString (password args)))
  atLeast_ (fromMaybe (length (servers args)) (durability args)) $
    map
      (\server ->
         retryingTCPClient defaultPort server hash (sourceFile (file args)))
      (servers args)

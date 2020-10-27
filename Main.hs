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
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )
import Options.Applicative.Types (ParserInfo)

import TCPServer (tCPServer)

data Args =
  Args
    { blobdir :: String
    , password :: String
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
  strOption
    (long "port" <>
     value "8934" <> showDefault <> help "TCP port on which to listen")

parserInfo :: ParserInfo Args
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <>
     progDesc "Transfer blobs around" <>
     header "bejnarkli - store authenticated blobs received from the network")

main :: IO ()
main = do
  args <- execParser parserInfo
  bs <- newBlobDir (blobdir args)
  tCPServer (port args) bs (Pass (fromString (password args)))

module Main
  ( main
  ) where

import BlobStore (newBlobDir)
import Data.ByteString.UTF8 (fromString)

import TCPServer (tCPServer)

main :: IO ()
main = do
  bs <- newBlobDir "teh-blobs"
  tCPServer "8934" bs (fromString "secret")

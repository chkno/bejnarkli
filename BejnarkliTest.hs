module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as U8L
import qualified Data.ByteString.UTF8 as U8S
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli (bejnarkliServer)
import BlobStore (blobName, getBlob, listBlobs, newBlobMap)

password = U8S.fromString "test secret"

prop_ServerWritesBlob :: BL.ByteString -> Property
prop_ServerWritesBlob b =
  let name = blobName password b
      stream = BL.append (BL.fromStrict name) b
   in monadicIO $ do
        bs <- run newBlobMap
        result <- run $ bejnarkliServer bs password stream
        assert $ result == U8L.fromString "y"
        storedBlobs <- run $ listBlobs bs
        storedBlob <- run $ getBlob bs $ head storedBlobs
        assert $ storedBlob == b

tests :: IO [Result]
tests = mapM quickCheckResult [prop_ServerWritesBlob]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

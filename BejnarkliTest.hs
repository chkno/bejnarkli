module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as U8L
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, (==>), isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli (bejnarkliServer)
import BlobStore (Password, blobName, getBlob, listBlobs, newBlobMap)

prop_ServerWritesBlob :: Password -> BL.ByteString -> Property
prop_ServerWritesBlob password b =
  let name = blobName password b
      stream = BL.append (BL.fromStrict name) b
   in monadicIO $ do
        bs <- run newBlobMap
        result <- run $ bejnarkliServer bs password stream
        assert $ result == U8L.fromString "y"
        storedBlobs <- run $ listBlobs bs
        storedBlob <- run $ getBlob bs $ head storedBlobs
        assert $ storedBlob == b

prop_ServerRejectsBadPassword ::
     Password -> Password -> BL.ByteString -> Property
prop_ServerRejectsBadPassword pass1 pass2 b =
  (pass1 /= pass2) ==>
  let name = blobName pass1 b
      stream = BL.append (BL.fromStrict name) b
   in monadicIO $ do
        bs <- run newBlobMap
        result <- run $ bejnarkliServer bs pass2 stream
        assert $ result == U8L.fromString "n"
        storedBlobs <- run $ listBlobs bs
        assert $ null storedBlobs

tests :: IO [Result]
tests =
  sequence
    [ quickCheckResult prop_ServerWritesBlob
    , quickCheckResult prop_ServerRejectsBadPassword
    ]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

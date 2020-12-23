module Main
  ( main
  ) where

import Conduit ((.|), await, runConduitRes, yield)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as U8S
import Data.Conduit.Combinators (sinkLazy, sourceLazy)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, (==>), isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Bejnarkli (bejnarkliServer)
import BlobStore (Password, blobName, getBlob, listBlobs, newBlobMap)

prop_ServerWritesBlob :: Password -> BL.ByteString -> Property
prop_ServerWritesBlob password b = monadicIO $ do
  name <- run $ runConduitRes $ sourceLazy b .| blobName password
  bs <- run newBlobMap
  result <- run
    $ runConduitRes
    $ (yield (BS.pack [66]) *> yield name *> sourceLazy b)
    .| bejnarkliServer bs password
    .| await
  assert $ result == Just (U8S.fromString "y")
  storedBlobs <- run $ listBlobs bs
  storedBlob <- run $ runConduitRes $ getBlob bs (head storedBlobs) .| sinkLazy
  assert $ storedBlob == b

prop_ServerRejectsBadPassword
  :: Password -> Password -> BL.ByteString -> Property
prop_ServerRejectsBadPassword pass1 pass2 b =
  (pass1 /= pass2) ==> monadicIO $ do
    name <- run $ runConduitRes $ sourceLazy b .| blobName pass1
    bs <- run newBlobMap
    result <- run
      $ runConduitRes
      $ (yield (BS.pack [66]) *> yield name *> sourceLazy b)
      .| bejnarkliServer bs pass2
      .| await
    assert $ result == Just (U8S.fromString "n")
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

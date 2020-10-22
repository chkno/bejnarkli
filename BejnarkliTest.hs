module Main where

import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromJust)
import System.IO.Temp (withSystemTempDirectory)
import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, (==>), quickCheck)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli

password = fromString "test secret"

prop_BlobStoreWriteRead ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> Property
prop_BlobStoreWriteRead ubs b =
  monadicIO $ do
    ename <- run $ commit $ stageBlob ubs (blobName password b) b
    ret <- run $ getBlob ubs ename
    assert $ ret == b

prop_BlobStoreWritePrefixedRead ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedRead ubs b =
  let stream = BL.append (BL.fromStrict $ blobName password b) b
   in monadicIO $ do
        ename <- run $ writeNamePrefixedBlob ubs stream
        ret <- run $ getBlob ubs ename
        assert $ ret == b

main :: IO ()
main = withSystemTempDirectory "bej" $ tests >=> TF.defaultMain

tests :: FilePath -> IO [Test]
tests tmpdir = do
  m <- newUnverifiedBlobMap
  d <- newUnverifiedBlobDir tmpdir
  pure
    [ testProperty "Map Write Read" (prop_BlobStoreWriteRead m)
    , testProperty "Map Write Prefixed Read" (prop_BlobStoreWritePrefixedRead m)
    , testProperty "Dir Write Read" (prop_BlobStoreWriteRead d)
    , testProperty "Dir Write Prefixed Read" (prop_BlobStoreWritePrefixedRead d)
    ]

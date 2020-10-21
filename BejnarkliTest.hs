module Main where

import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO.Temp (withSystemTempDirectory)
import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, (==>), quickCheck)
import qualified Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli

prop_BlobStoreWriteRead ::
     BlobStore bs => bs -> BS.ByteString -> BL.ByteString -> Property
prop_BlobStoreWriteRead bs n b =
  BS.length n > 0 ==> monadicIO $ do
    ename <- run $ writeBlob bs n b
    ret <- run $ getBlob bs ename
    assert $ ret == b

prop_BlobStoreWritePrefixedRead ::
     BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedRead bs stream =
  BL.length stream > fromIntegral blobNameLength ==> monadicIO $ do
    ename <- run $ writeNamePrefixedBlob bs stream
    ret <- run $ getBlob bs ename
    assert $ ret == BL.drop (fromIntegral blobNameLength) stream

main :: IO ()
main = withSystemTempDirectory "bej" $ tests >=> TF.defaultMain

tests :: FilePath -> IO [Test]
tests tmpdir = do
  m <- newBlobMap
  d <- newBlobDir tmpdir
  pure
    [ testProperty "Map Write Read" (prop_BlobStoreWriteRead m)
    , testProperty "Map Write Prefixed Read" (prop_BlobStoreWritePrefixedRead m)
    , testProperty "Dir Write Read" (prop_BlobStoreWriteRead d)
    , testProperty "Dir Write Prefixed Read" (prop_BlobStoreWritePrefixedRead d)
    ]

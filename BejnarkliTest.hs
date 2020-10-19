module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, quickCheck)
import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli

prop_BlobStoreWriteRead :: BlobStore bs => bs -> BS.ByteString -> BL.ByteString -> Property
prop_BlobStoreWriteRead bs n b = monadicIO $ do
  ename <- run $ writeBlob bs n b
  ret <- run $ getBlob bs ename
  assert $ ret == b

main :: IO ()
main = tests >>= TF.defaultMain

tests :: IO [Test]
tests = do
  m <- newBlobMap
  pure [ testProperty "Map Write Read" (prop_BlobStoreWriteRead m) ]

module Main where

import Test.QuickCheck (Property, quickCheck)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli

prop_BlobStoreWriteRead :: BlobStore bs => bs -> String -> String -> Property
prop_BlobStoreWriteRead bs n b = monadicIO $ do
  ename <- run $ writeBlob bs n b
  ret <- run $ getBlob bs ename
  assert $ ret == b

main :: IO ()
main = do
  newBlobMap >>= quickCheck <$> prop_BlobStoreWriteRead

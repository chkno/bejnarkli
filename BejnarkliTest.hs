module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Property, Result, isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli
  ( UnverifiedBlobStore
  , blobName
  , getBlob
  , newUnverifiedBlobDir
  , newUnverifiedBlobMap
  , writeNamePrefixedBlob
  , writeTrustedBlob
  )

password = fromString "test secret"

prop_BlobStoreWriteRead ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> Property
prop_BlobStoreWriteRead ubs b =
  monadicIO $ do
    ename <- run $ writeTrustedBlob ubs password b
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

tests :: IO [Result]
tests =
  withSystemTempDirectory
    "bej"
    (\tmpdir -> do
       m <- newUnverifiedBlobMap
       d <- newUnverifiedBlobDir tmpdir
       mapM
         quickCheckResult
         [ prop_BlobStoreWriteRead m
         , prop_BlobStoreWritePrefixedRead m
         , prop_BlobStoreWriteRead d
         , prop_BlobStoreWritePrefixedRead d
         ])

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

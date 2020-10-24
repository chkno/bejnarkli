module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as U8L
import qualified Data.ByteString.UTF8 as U8S
import Data.Maybe (fromJust, isNothing)
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
  , writeUntrustedBlob
  )

password = U8S.fromString "test secret"

prop_BlobStoreWriteReadTrusted ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> Property
prop_BlobStoreWriteReadTrusted ubs b =
  monadicIO $ do
    ename <- run $ writeTrustedBlob ubs password b
    ret <- run $ getBlob ubs ename
    assert $ ret == b

prop_BlobStoreWriteReadCorrectHash ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> Property
prop_BlobStoreWriteReadCorrectHash ubs b =
  monadicIO $ do
    ename <- run $ writeUntrustedBlob ubs password (blobName password b) b
    ret <- run $ getBlob ubs (fromJust ename)
    assert $ ret == b

prop_BlobStoreWriteWrongHash ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> Property
prop_BlobStoreWriteWrongHash ubs b =
  monadicIO $
  let wrongHash = blobName password (BL.append b (U8L.fromString "different"))
   in do ename <- run $ writeUntrustedBlob ubs password wrongHash b
         assert $ isNothing ename

prop_BlobStoreWriteWrongPassword ::
     UnverifiedBlobStore ubs => ubs -> BL.ByteString -> Property
prop_BlobStoreWriteWrongPassword ubs b =
  monadicIO $
  let wrongHash = blobName (U8S.fromString "wrong password") b
   in do ename <- run $ writeUntrustedBlob ubs password wrongHash b
         assert $ isNothing ename

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
         [ prop_BlobStoreWriteReadTrusted m
         , prop_BlobStoreWriteReadCorrectHash m
         , prop_BlobStoreWriteWrongHash m
         , prop_BlobStoreWriteWrongPassword m
         , prop_BlobStoreWritePrefixedRead m
         , prop_BlobStoreWriteReadTrusted d
         , prop_BlobStoreWriteReadCorrectHash d
         , prop_BlobStoreWriteWrongHash d
         , prop_BlobStoreWriteWrongPassword d
         , prop_BlobStoreWritePrefixedRead d
         ])

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

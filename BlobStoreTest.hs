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

import BlobStore
  ( BlobStore
  , blobName
  , getBlob
  , newBlobDir
  , newBlobMap
  , writeNamePrefixedBlob
  , writeTrustedBlob
  , writeUntrustedBlob
  )

password = U8S.fromString "test secret"

prop_BlobStoreWriteReadTrusted ::
     BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWriteReadTrusted bs b =
  monadicIO $ do
    ename <- run $ writeTrustedBlob bs password b
    ret <- run $ getBlob bs ename
    assert $ ret == b

prop_BlobStoreWriteReadCorrectHash ::
     BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWriteReadCorrectHash bs b =
  monadicIO $ do
    ename <- run $ writeUntrustedBlob bs password (blobName password b) b
    ret <- run $ getBlob bs (fromJust ename)
    assert $ ret == b

prop_BlobStoreWriteWrongHash :: BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWriteWrongHash bs b =
  monadicIO $
  let wrongHash = blobName password (BL.append b (U8L.fromString "different"))
   in do ename <- run $ writeUntrustedBlob bs password wrongHash b
         assert $ isNothing ename

prop_BlobStoreWriteWrongPassword ::
     BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWriteWrongPassword bs b =
  monadicIO $
  let wrongHash = blobName (U8S.fromString "wrong password") b
   in do ename <- run $ writeUntrustedBlob bs password wrongHash b
         assert $ isNothing ename

prop_BlobStoreWritePrefixedRead ::
     BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedRead bs b =
  let stream = BL.append (BL.fromStrict $ blobName password b) b
   in monadicIO $ do
        ename <- run $ writeNamePrefixedBlob bs password stream
        ret <- run $ getBlob bs (fromJust ename)
        assert $ ret == b

prop_BlobStoreWritePrefixedWrongHash ::
     BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedWrongHash bs b =
  monadicIO $
  let wrongHash = blobName password (BL.append b (U8L.fromString "different"))
      stream = BL.append (BL.fromStrict wrongHash) b
   in do ename <- run $ writeNamePrefixedBlob bs password stream
         assert $ isNothing ename

prop_BlobStoreWritePrefixedWrongPassword ::
     BlobStore bs => bs -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedWrongPassword bs b =
  monadicIO $
  let wrongHash = blobName (U8S.fromString "wrong password") b
      stream = BL.append (BL.fromStrict wrongHash) b
   in do ename <- run $ writeNamePrefixedBlob bs password stream
         assert $ isNothing ename

tests :: IO [Result]
tests =
  withSystemTempDirectory
    "bej"
    (\tmpdir -> do
       m <- newBlobMap
       d <- newBlobDir tmpdir
       sequence
         [ quickCheckResult $ prop_BlobStoreWriteReadTrusted m
         , quickCheckResult $ prop_BlobStoreWriteReadCorrectHash m
         , quickCheckResult $ prop_BlobStoreWriteWrongHash m
         , quickCheckResult $ prop_BlobStoreWriteWrongPassword m
         , quickCheckResult $ prop_BlobStoreWritePrefixedRead m
         , quickCheckResult $ prop_BlobStoreWritePrefixedWrongHash m
         , quickCheckResult $ prop_BlobStoreWritePrefixedWrongPassword m
         , quickCheckResult $ prop_BlobStoreWriteReadTrusted d
         , quickCheckResult $ prop_BlobStoreWriteReadCorrectHash d
         , quickCheckResult $ prop_BlobStoreWriteWrongHash d
         , quickCheckResult $ prop_BlobStoreWriteWrongPassword d
         , quickCheckResult $ prop_BlobStoreWritePrefixedRead d
         , quickCheckResult $ prop_BlobStoreWritePrefixedWrongHash d
         , quickCheckResult $ prop_BlobStoreWritePrefixedWrongPassword d
         ])

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust, isNothing)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Property, Result, (==>), isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import BlobStore
  ( BlobDirStore
  , BlobMapStore
  , BlobStore
  , Password
  , blobName
  , getBlob
  , newBlobDir
  , newBlobMap
  , writeNamePrefixedBlob
  , writeTrustedBlob
  , writeUntrustedBlob
  )

prop_BlobStoreWriteReadTrusted ::
     BlobStore bs => bs -> Password -> BL.ByteString -> Property
prop_BlobStoreWriteReadTrusted bs password b =
  monadicIO $ do
    ename <- run $ writeTrustedBlob bs password b
    ret <- run $ getBlob bs ename
    assert $ ret == b

prop_BlobStoreWriteReadCorrectHash ::
     BlobStore bs => bs -> Password -> BL.ByteString -> Property
prop_BlobStoreWriteReadCorrectHash bs password b =
  monadicIO $ do
    ename <- run $ writeUntrustedBlob bs password (blobName password b) b
    ret <- run $ getBlob bs (fromJust ename)
    assert $ ret == b

prop_BlobStoreWriteWrongHash ::
     BlobStore bs
  => bs
  -> Password
  -> BL.ByteString
  -> BL.ByteString
  -> Property
prop_BlobStoreWriteWrongHash bs password blob1 blob2 =
  (blob1 /= blob2) ==> monadicIO $ do
    ename <-
      run $ writeUntrustedBlob bs password (blobName password blob2) blob1
    assert $ isNothing ename

prop_BlobStoreWriteWrongPassword ::
     BlobStore bs => bs -> Password -> Password -> BL.ByteString -> Property
prop_BlobStoreWriteWrongPassword bs pass1 pass2 b =
  (pass1 /= pass2) ==> monadicIO $
  let wrongHash = blobName pass1 b
   in do ename <- run $ writeUntrustedBlob bs pass2 wrongHash b
         assert $ isNothing ename

prop_BlobStoreWritePrefixedRead ::
     BlobStore bs => bs -> Password -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedRead bs password b =
  let stream = BL.append (BL.fromStrict $ blobName password b) b
   in monadicIO $ do
        ename <- run $ writeNamePrefixedBlob bs password stream
        ret <- run $ getBlob bs (fromJust ename)
        assert $ ret == b

prop_BlobStoreWritePrefixedWrongHash ::
     BlobStore bs
  => bs
  -> Password
  -> BL.ByteString
  -> BL.ByteString
  -> Property
prop_BlobStoreWritePrefixedWrongHash bs password blob1 blob2 =
  (blob1 /= blob2) ==> monadicIO $
  let stream = BL.append (BL.fromStrict (blobName password blob2)) blob1
   in do ename <- run $ writeNamePrefixedBlob bs password stream
         assert $ isNothing ename

prop_BlobStoreWritePrefixedWrongPassword ::
     BlobStore bs => bs -> Password -> Password -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedWrongPassword bs pass1 pass2 b =
  (pass1 /= pass2) ==> monadicIO $
  let wrongHash = blobName pass2 b
      stream = BL.append (BL.fromStrict wrongHash) b
   in do ename <- run $ writeNamePrefixedBlob bs pass1 stream
         assert $ isNothing ename

class BlobStore bs =>
      TestedBlobStore bs
  where
  testBlobStore :: bs -> [IO Result]
  testBlobStore bs =
    [ quickCheckResult $ prop_BlobStoreWriteReadTrusted bs
    , quickCheckResult $ prop_BlobStoreWriteReadCorrectHash bs
    , quickCheckResult $ prop_BlobStoreWriteWrongHash bs
    , quickCheckResult $ prop_BlobStoreWriteWrongPassword bs
    , quickCheckResult $ prop_BlobStoreWritePrefixedRead bs
    , quickCheckResult $ prop_BlobStoreWritePrefixedWrongHash bs
    , quickCheckResult $ prop_BlobStoreWritePrefixedWrongPassword bs
    ]

instance TestedBlobStore BlobMapStore

instance TestedBlobStore BlobDirStore

tests :: IO [Result]
tests =
  withSystemTempDirectory
    "bej"
    (\tmpdir -> do
       m <- newBlobMap
       d <- newBlobDir tmpdir
       sequence $ testBlobStore m ++ testBlobStore d)

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

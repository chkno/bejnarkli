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
  ( BlobStore
  , blobName
  , getBlob
  , newBlobDir
  , newBlobMap
  , someFunc
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
       mapM
         quickCheckResult
         [ prop_BlobStoreWriteReadTrusted m
         , prop_BlobStoreWriteReadCorrectHash m
         , prop_BlobStoreWriteWrongHash m
         , prop_BlobStoreWriteWrongPassword m
         , prop_BlobStoreWritePrefixedRead m
         , prop_BlobStoreWritePrefixedWrongHash m
         , prop_BlobStoreWritePrefixedWrongPassword m
         , prop_BlobStoreWriteReadTrusted d
         , prop_BlobStoreWriteReadCorrectHash d
         , prop_BlobStoreWriteWrongHash d
         , prop_BlobStoreWriteWrongPassword d
         , prop_BlobStoreWritePrefixedRead d
         , prop_BlobStoreWritePrefixedWrongHash d
         , prop_BlobStoreWritePrefixedWrongPassword d
         ])

main :: IO ()
main = do
  someFunc
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

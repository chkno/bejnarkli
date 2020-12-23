module Main
  ( main
  ) where

import Conduit ((.|), runConduitRes, yield)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Combinators (sinkLazy, sourceLazy)
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
  , sinkNamePrefixedBlob
  , sinkTrustedBlob
  , sinkUntrustedBlob
  )
import ReplicatingBlobStore (ReplicatingBlobStore(ReplicatingBlobStore))

prop_BlobStoreWriteReadTrusted
  :: BlobStore bs => bs -> Password -> BL.ByteString -> Property
prop_BlobStoreWriteReadTrusted bs password b = monadicIO $ do
  ename <- run $ runConduitRes $ sourceLazy b .| sinkTrustedBlob bs password
  ret <- run $ runConduitRes $ getBlob bs ename .| sinkLazy
  assert $ ret == b

prop_BlobStoreWriteReadCorrectHash
  :: BlobStore bs => bs -> Password -> BL.ByteString -> Property
prop_BlobStoreWriteReadCorrectHash bs password b = monadicIO $ do
  name <- run $ runConduitRes $ sourceLazy b .| blobName password
  ename
    <- run $ runConduitRes $ sourceLazy b .| sinkUntrustedBlob bs password name
  ret <- run $ runConduitRes $ getBlob bs (fromJust ename) .| sinkLazy
  assert $ ret == b

prop_BlobStoreWriteWrongHash
  :: BlobStore bs
  => bs
  -> Password
  -> BL.ByteString
  -> BL.ByteString
  -> Property
prop_BlobStoreWriteWrongHash bs password blob1 blob2 =
  (blob1 /= blob2) ==> monadicIO $ do
    name <- run $ runConduitRes $ sourceLazy blob1 .| blobName password
    ename <- run
      $ runConduitRes
      $ sourceLazy blob2 .| sinkUntrustedBlob bs password name
    assert $ isNothing ename

prop_BlobStoreWriteWrongPassword
  :: BlobStore bs => bs -> Password -> Password -> BL.ByteString -> Property
prop_BlobStoreWriteWrongPassword bs pass1 pass2 b =
  (pass1 /= pass2) ==> monadicIO $ do
    wrongHash <- run $ runConduitRes $ sourceLazy b .| blobName pass1
    ename <- run
      $ runConduitRes
      $ sourceLazy b .| sinkUntrustedBlob bs pass2 wrongHash
    assert $ isNothing ename

prop_BlobStoreWritePrefixedRead
  :: BlobStore bs => bs -> Password -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedRead bs password b = monadicIO $ do
  name <- run $ runConduitRes $ sourceLazy b .| blobName password
  ename <- run
    $ runConduitRes
    $ (yield name *> sourceLazy b) .| sinkNamePrefixedBlob bs password
  ret <- run $ runConduitRes $ getBlob bs (fromJust ename) .| sinkLazy
  assert $ ret == b

prop_BlobStoreWritePrefixedWrongHash
  :: BlobStore bs
  => bs
  -> Password
  -> BL.ByteString
  -> BL.ByteString
  -> Property
prop_BlobStoreWritePrefixedWrongHash bs password blob1 blob2 =
  (blob1 /= blob2) ==> monadicIO $ do
    wrongHash <- run $ runConduitRes $ sourceLazy blob1 .| blobName password
    ename <- run
      $ runConduitRes
      $ (yield wrongHash *> sourceLazy blob2)
      .| sinkNamePrefixedBlob bs password
    assert $ isNothing ename

prop_BlobStoreWritePrefixedWrongPassword
  :: BlobStore bs => bs -> Password -> Password -> BL.ByteString -> Property
prop_BlobStoreWritePrefixedWrongPassword bs pass1 pass2 b =
  (pass1 /= pass2) ==> monadicIO $ do
    wrongHash <- run $ runConduitRes $ sourceLazy b .| blobName pass1
    ename <- run
      $ runConduitRes
      $ (yield wrongHash *> sourceLazy b) .| sinkNamePrefixedBlob bs pass2
    assert $ isNothing ename

class BlobStore bs => TestedBlobStore bs where
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

instance BlobStore bs => TestedBlobStore (ReplicatingBlobStore bs)

tests :: IO [Result]
tests =
  withSystemTempDirectory
    "bej"
    (\tmpdir -> do
       m <- newBlobMap
       d <- newBlobDir tmpdir
       r <- ReplicatingBlobStore [] <$> newBlobMap
       sequence $ testBlobStore m ++ testBlobStore d ++ testBlobStore r)

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

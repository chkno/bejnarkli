module Main
  ( main
  ) where

import Conduit ((.|), runConduitRes)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Combinators (sinkLazy, sourceLazy)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli (bejnarkliClient, bejnarkliServer)
import BlobStore
  ( BlobStore
  , ExtantBlobName(ExtantBlob)
  , Password
  , getBlob
  , newBlobMap
  , sinkTrustedBlob
  )
import ReplicatingBlobStore (ReplicatingBlobStore(ReplicatingBlobStore))

localBejnarkliClient ::
     BlobStore bs => bs -> Password -> (bs, ExtantBlobName) -> IO ()
localBejnarkliClient remoteBS password (localBS, ename@(ExtantBlob name)) = do
  response <-
    runConduitRes $
    getBlob localBS ename .|
    bejnarkliClient (bejnarkliServer remoteBS password) name
  if response
    then pure ()
    else error "Unexpected transfer failure"

prop_Replicates :: Password -> BL.ByteString -> Property
prop_Replicates password b =
  monadicIO $ do
    localBS <- run newBlobMap
    remoteBS <- run newBlobMap
    let replicatedBS =
          ReplicatingBlobStore [localBejnarkliClient remoteBS password] localBS
     in do ename <-
             run $
             runConduitRes $
             sourceLazy b .| sinkTrustedBlob replicatedBS password
           locallyStored <-
             run $ runConduitRes $ getBlob localBS ename .| sinkLazy
           remotelyStored <-
             run $ runConduitRes $ getBlob remoteBS ename .| sinkLazy
           assert $ locallyStored == b
           assert $ remotelyStored == b

tests :: IO [Result]
tests = sequence [quickCheckResult prop_Replicates]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

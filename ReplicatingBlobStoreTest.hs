module Main
  ( main
  ) where

import Conduit ((.|), await, runConduitRes)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import Data.Conduit.Combinators (sourceLazy)
import Data.Maybe (fromJust)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli (bejnarkliServer)
import BlobStore (BlobStore, Password, getBlob, newBlobMap, sinkTrustedBlob)
import ReplicatingBlobStore (ReplicatingBlobStore(ReplicatingBlobStore))

nonTCPBejnarkliClient ::
     BlobStore bs => bs -> Password -> BL.ByteString -> IO ()
nonTCPBejnarkliClient bs password blob = do
  response <-
    runConduitRes $ sourceLazy blob .| bejnarkliServer bs password .| await
  if fromJust response == fromString "y"
    then pure ()
    else error "Unexpected transfer failure"

prop_Replicates :: Password -> BL.ByteString -> Property
prop_Replicates password b =
  monadicIO $ do
    localBS <- run newBlobMap
    remoteBS <- run newBlobMap
    let remoteServer = nonTCPBejnarkliClient remoteBS password
        replicatedBS = ReplicatingBlobStore [remoteServer] localBS
     in do ename <-
             run $
             runConduitRes $
             sourceLazy b .| sinkTrustedBlob replicatedBS password
           locallyStored <- run $ getBlob localBS ename
           remotelyStored <- run $ getBlob remoteBS ename
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

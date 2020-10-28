module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BL
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, isSuccess, quickCheckResult)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Bejnarkli (bejnarkliServer)
import BlobStore (Password, getBlob, newBlobMap, writeTrustedBlob)
import ReplicatingBlobStore (ReplicatingBlobStore(ReplicatingBlobStore))

prop_Replicates :: Password -> BL.ByteString -> Property
prop_Replicates password b =
  monadicIO $ do
    localBS <- run newBlobMap
    remoteBS <- run newBlobMap
    let remoteServer = bejnarkliServer remoteBS password
        replicatedBS = ReplicatingBlobStore [remoteServer] localBS
     in do ename <- run $ writeTrustedBlob replicatedBS password b
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

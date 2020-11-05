module Main
  ( main
  ) where

import Conduit (MonadUnliftIO, (.|), liftIO, runConduitRes)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Combinators (sourceLazy)
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
     (BlobStore bs, MonadUnliftIO m)
  => bs
  -> Password
  -> (bs, ExtantBlobName)
  -> m ()
localBejnarkliClient remoteBS password (localBS, ename) = do
  blob <- liftIO $ getBlob localBS ename
  response <-
    runConduitRes $
    sourceLazy blob .| bejnarkliClient (bejnarkliServer remoteBS password) name
  if response
    then pure ()
    else error "Unexpected transfer failure"
  where
    (ExtantBlob name) = ename

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

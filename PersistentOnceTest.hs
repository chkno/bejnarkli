{-# LANGUAGE TupleSections #-}

module Main
  ( main
  ) where

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck
  ( NonNegative(NonNegative)
  , Property
  , Result
  , isSuccess
  , quickCheckResult
  )
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (monadicIO, run)

import PersistentOnce (once)

correct :: Int -> Int -> Bool
correct 0 count = count == 0
correct _ count = count == 1

prop_Once :: BS.ByteString -> NonNegative Int -> Property
prop_Once name (NonNegative n) =
  monadicIO $ run $ withSystemTempDirectory "bej" aux
  where
    aux :: FilePath -> IO Bool
    aux tmpdir = do
      count <- newIORef 0
      replicateM_ n (once (tmpdir </> "once-db") name $ inc count)
      finalCount <- readIORef count
      pure $ correct n finalCount
    inc :: IORef Int -> IO ()
    inc count = atomicModifyIORef' count ((, ()) . (+ 1))

prop_OnceConcurrently :: BS.ByteString -> NonNegative Int -> Property
prop_OnceConcurrently name (NonNegative n) =
  monadicIO $ run $ withSystemTempDirectory "bej" aux
  where
    aux :: FilePath -> IO Bool
    aux tmpdir = do
      count <- newIORef 0
      withPool (n + 1) $ \pool ->
        parallel_ pool $
        replicate n (once (tmpdir </> "once-db") name $ inc count)
      finalCount <- readIORef count
      pure $ correct n finalCount
    inc :: IORef Int -> IO ()
    inc count = atomicModifyIORef' count ((, ()) . (+ 1))

tests :: IO [Result]
tests =
  sequence [quickCheckResult prop_Once, quickCheckResult prop_OnceConcurrently]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

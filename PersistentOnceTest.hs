{-# LANGUAGE TupleSections #-}

module Main
  ( main
  ) where

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)
import Control.Exception (Exception, throwIO, try)
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

inc :: IORef Int -> IO ()
inc count = atomicModifyIORef' count ((, ()) . (+ 1))

prop_Once :: BS.ByteString -> NonNegative Int -> Property
prop_Once name (NonNegative n) =
  monadicIO $ run $ withSystemTempDirectory "bej" aux
  where
    aux :: FilePath -> IO Bool
    aux tmpdir = do
      count <- newIORef 0
      replicateM_ n (once (tmpdir </> "once-db") name $ inc count >> pure True)
      finalCount <- readIORef count
      pure $ correct n finalCount

prop_OnceConcurrently :: BS.ByteString -> NonNegative Int -> Property
prop_OnceConcurrently name (NonNegative n) =
  monadicIO $ run $ withSystemTempDirectory "bej" aux
  where
    aux :: FilePath -> IO Bool
    aux tmpdir = do
      count <- newIORef 0
      withPool (n + 1) $ \pool ->
        parallel_ pool $
        replicate n (once (tmpdir </> "once-db") name $ inc count >> pure True)
      finalCount <- readIORef count
      pure $ correct n finalCount

data SimulatedFailure =
  SimulatedFailure
  deriving (Show)

instance Exception SimulatedFailure

prop_OnceConcurrentlyFlakily :: BS.ByteString -> NonNegative Int -> Property
prop_OnceConcurrentlyFlakily name (NonNegative n) =
  monadicIO $ run $ withSystemTempDirectory "bej" aux
  where
    aux :: FilePath -> IO Bool
    aux tmpdir = do
      count <- newIORef 0
      withPool (n + 1) $ \pool ->
        parallel_ pool $
        map
          (\i ->
             try (once (tmpdir </> "once-db") name $ flakyInc i count) :: IO (Either SimulatedFailure Bool))
          [0 .. (n - 1)]
      finalCount <- readIORef count
      pure $ correct n finalCount
    flakyInc :: Int -> IORef Int -> IO Bool
    flakyInc i count
      | i `mod` 3 == 0 = inc count >> pure True
      | i `mod` 3 == 1 = pure False
      | otherwise = throwIO SimulatedFailure

tests :: IO [Result]
tests =
  sequence
    [ quickCheckResult prop_Once
    , quickCheckResult prop_OnceConcurrently
    , quickCheckResult prop_OnceConcurrentlyFlakily
    ]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

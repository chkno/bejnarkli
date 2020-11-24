module Main
  ( main
  ) where

import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, (==>), isSuccess, quickCheckResult)
import Test.QuickCheck.Monadic (monadicIO, run)

import Control.Concurrent.Chan (newChan, writeChan)
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , takeMVar
  )

import RetryQueue (RetryParams(RetryParams), retryQueue)

prop_retryQueueDoesStuff :: Int -> Property
prop_retryQueueDoesStuff attemptsNeeded =
  (0 < attemptsNeeded && attemptsNeeded < 100) ==> monadicIO $ do
    chan <- run newChan
    counter <- run (newMVar 0 :: IO (MVar Int))
    done <- run newEmptyMVar
    _ <-
      run $
      retryQueue
        (RetryParams 0 0 0)
        (\_ -> do
           val <- modifyMVar counter (\v -> pure (v + 1, v))
           if val > attemptsNeeded
             then do
               putMVar done ()
               pure True
             else pure False)
        chan
    run $ writeChan chan ()
    run $ takeMVar done

tests :: IO [Result]
tests = sequence [quickCheckResult prop_retryQueueDoesStuff]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

module Main
  ( main
  ) where

import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck (Property, Result, (==>), isSuccess, quickCheckResult)
import Test.QuickCheck.Monadic (monadicIO, run)

import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , takeMVar
  )

import Retry (RetryParams(RetryParams), retryQueue, retryWithDelay)

prop_retryWithDelayDoesStuff :: Int -> Property
prop_retryWithDelayDoesStuff attemptsNeeded =
  (0 < attemptsNeeded && attemptsNeeded < 100) ==> monadicIO $ do
    counter <- run (newMVar 0 :: IO (MVar Int))
    done <- run newEmptyMVar
    run $
      retryWithDelay
        (RetryParams 0 0 0)
        (do val <- modifyMVar counter (\v -> pure (v + 1, v))
            if val > attemptsNeeded
              then do
                putMVar done ()
                pure True
              else pure False)
    run $ takeMVar done

prop_retryQueueDoesStuff :: Int -> Property
prop_retryQueueDoesStuff attemptsNeeded =
  (0 < attemptsNeeded && attemptsNeeded < 100) ==> monadicIO $ do
    counter <- run (newMVar 0 :: IO (MVar Int))
    done <- run newEmptyMVar
    queue <-
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
    run $ queue ()
    run $ takeMVar done

tests :: IO [Result]
tests =
  sequence
    [ quickCheckResult prop_retryWithDelayDoesStuff
    , quickCheckResult prop_retryQueueDoesStuff
    ]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

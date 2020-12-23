{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Retry
  ( increment
  , minDelay
  , maxDelay
  , RetryParams(RetryParams)
  , retryQueue
  , retryWithDelay
  ) where

import Data.Functor (($>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (try)
import System.Random (getStdRandom, randomR)

-- This is growing closer to Control.Retry from the "retry" package.
-- Some of this should probably be replaced with that.
data RetryParams = RetryParams
  { increment :: Float, -- Multiplier for how much longer to wait on each consecutive failure.  Eg: 1.5
    minDelay :: Float,
    maxDelay :: Float
  }

-- How long to wait
nextBackoff :: RetryParams -> Bool -> Float -> Float
nextBackoff params True _ = minDelay params
nextBackoff params False prevBackoff =
  max (minDelay params) (min (maxDelay params) (prevBackoff * increment params))

retryWithDelay :: RetryParams -> IO Bool -> IO ()
retryWithDelay params action = process $ minDelay params

  where
    process :: Float -> IO ()
    process prevBackoff = (== Right True) <$> try @IOError action >>= \case
      True -> pure ()
      False -> do
        let backoff = nextBackoff params False prevBackoff
        delay <- getStdRandom (randomR (0, 2 * backoff))
        threadDelay $ round $ 100000 * delay
        process backoff

-- | Apply f to items passed to the returned enqueue function.
-- f returns a bool indicating success.  When f is unsuccessful:
--   * The item is re-enqueued to be attempted again later
--   * Subsequent calls of f are delayed by exponential back-off with jitter
--     until successful again.
--
-- Use one retryQueue per failure domain (eg: one per remote network host)
retryQueue :: forall a. RetryParams -> (a -> IO Bool) -> IO (a -> IO ())
retryQueue params f = do
  chan <- newChan
  forkIO (process (minDelay params) chan) $> writeChan chan -- TODO: Allow thread clean-up

  where
    process :: Float -> Chan a -> IO ()
    process prevBackoff chan = do
      item <- readChan chan
      (== Right True) <$> try @IOError (f item) >>= \case
        True -> process (minDelay params) chan
        False -> do
          let backoff = nextBackoff params False prevBackoff
          delay <- getStdRandom (randomR (0, 2 * backoff))
          threadDelay $ round $ 100000 * delay
          writeChan chan item
          process backoff chan

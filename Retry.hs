module Retry
  ( RetryParams(RetryParams)
  , increment
  , minDelay
  , maxDelay
  , retryQueue
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Exception (try)
import System.Random (getStdRandom, randomR)

-- This is growing closer to Control.Retry from the "retry" package.
-- Some of this should probably be replaced with that.
data RetryParams =
  RetryParams
    { increment :: Float -- Multiplier for how much longer to wait on each consecutive failure.  Eg: 1.5
    , minDelay :: Float
    , maxDelay :: Float
    }

-- How long to wait
nextBackoff :: RetryParams -> Bool -> Float -> Float
nextBackoff params True _ = minDelay params
nextBackoff params False prevBackoff =
  max (minDelay params) (min (maxDelay params) (prevBackoff * increment params))

-- |Apply f to items written to the channel.  f returns a bool indicating success.
-- When f is unsuccessful:
--   * The item is re-inserted into the channel to be attempted again later
--   * Subsequent calls of f are delayed by exponential back-off with jitter
--     until successful again.
retryQueue :: RetryParams -> (a -> IO Bool) -> Chan a -> IO ThreadId
retryQueue params f chan = forkIO $ process (minDelay params)
  where
    process :: Float -> IO ()
    process prevBackoff = do
      item <- readChan chan
      success <- (== Right True) <$> (try $ f item :: IO (Either IOError Bool))
      let backoff = nextBackoff params success prevBackoff
       in if success
            then process (minDelay params)
            else do
              delay <- getStdRandom (randomR (0, 2 * backoff))
              threadDelay $ round $ 100000 * delay
              writeChan chan item
              process backoff

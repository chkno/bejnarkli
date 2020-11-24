module RetryQueue
  ( retryQueue
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Exception (try)
import System.Random (getStdRandom, randomR)

-- |Apply f to items written to the channel.  f returns a bool indicating success.
-- When f is unsuccessful:
--   * The item is re-inserted into the channel to be attempted again later
--   * Subsequent calls of f are delayed by exponential back-off with jitter
--     until successful again.
retryQueue :: Float -> Float -> Float -> (a -> IO Bool) -> Chan a -> IO ThreadId
retryQueue increment minDelay maxDelay f chan = forkIO $ process minDelay
  where
    process :: Float -> IO ()
    process prevBackoff = do
      item <- readChan chan
      success <- try $ f item :: IO (Either IOError Bool)
      let backoff = max minDelay (min maxDelay (prevBackoff * increment))
       in if success == Right True
            then process minDelay
            else do
              delay <- getStdRandom (randomR (0, 2 * backoff))
              threadDelay $ round $ 100000 * delay
              writeChan chan item
              process backoff

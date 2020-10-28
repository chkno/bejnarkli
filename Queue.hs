{-# LANGUAGE ScopedTypeVariables #-}

module Queue
  ( mapChanWithBackoff
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, getChanContents, writeChan)

-- |Apply f to items written to the channel.  f returns a bool indicating success.
-- When f is unsuccessful:
--   * The item is re-inserted into the channel to be attempted again later
--   * Subsequent calls of f are delayed by exponential back-off until successful.
mapChanWithBackoff ::
     forall a.
     Float
  -> Float
  -> Float
  -> (a -> IO Bool)
  -> Chan a
  -> IO ThreadId
mapChanWithBackoff increment minDelay maxDelay f chan =
  forkIO $ getChanContents chan >>= process 0.0
  where
    process :: Float -> [a] -> IO ()
    process prevBackoff (item:next) = do
      success <- f item
      let backoff = max minDelay (min maxDelay (prevBackoff * increment))
       in if success
            then process 0.0 next
            else do
              threadDelay $ round $ 1000000 * backoff
              writeChan chan item
              process backoff next
    process _ [] = error "getChanContents unexpectedly ended"

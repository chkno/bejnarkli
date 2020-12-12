{-# LANGUAGE TupleSections #-}

module Retransmit
  ( retransmit
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Data.Foldable (traverse_)

import BlobStore (BlobStore, ExtantBlobName, listBlobs)

retransmit
  :: BlobStore bs
  => bs -> [(bs, ExtantBlobName) -> IO ()] -> IO [ThreadId]
retransmit bs = traverse (\peer -> forkIO $ listBlobs bs >>= traverse_ (peer . (bs, )))

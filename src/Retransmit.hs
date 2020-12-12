{-# LANGUAGE TupleSections #-}

module Retransmit
  ( retransmit
  ) where

import Control.Concurrent (ThreadId, forkIO)

import BlobStore (BlobStore, ExtantBlobName, listBlobs)

retransmit
  :: BlobStore bs
  => bs -> [(bs, ExtantBlobName) -> IO ()] -> IO [ThreadId]
retransmit bs = mapM (\peer -> forkIO $ listBlobs bs >>= mapM_ (peer . (bs, )))

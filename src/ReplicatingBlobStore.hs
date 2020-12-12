module ReplicatingBlobStore
  ( ReplicatingBlobStore(..)
  ) where

import Data.Foldable (traverse_)

import BlobStore
  ( BlobStore
  , ExtantBlobName
  , StagedBlobHandle(StagedBlobHandle, abort, commit)
  , getBlob
  , listBlobs
  , sinkBlob
  )

data ReplicatingBlobStore bs =
  ReplicatingBlobStore [(bs, ExtantBlobName) -> IO ()] bs

instance BlobStore bs => BlobStore (ReplicatingBlobStore bs) where
  listBlobs (ReplicatingBlobStore _ bs) = listBlobs bs
  getBlob (ReplicatingBlobStore _ bs) = getBlob bs
  sinkBlob (ReplicatingBlobStore remotes bs) = do
    handle <- sinkBlob bs
    pure
      StagedBlobHandle
        { abort = abort handle
        , commit =
            \name -> do
              ename <- commit handle name
              traverse_ (\r -> r (bs, ename)) remotes
              pure ename
        }

module ReplicatingBlobStore
  ( ReplicatingBlobStore(..)
  ) where

import qualified Data.ByteString.Lazy as BL

import BlobStore
  ( BlobStore
  , StagedBlobHandle(StagedBlobHandle, abort, blobData, commit)
  , getBlob
  , listBlobs
  , stageBlob
  )

data ReplicatingBlobStore bs =
  ReplicatingBlobStore [BL.ByteString -> IO BL.ByteString] bs

instance BlobStore bs => BlobStore (ReplicatingBlobStore bs) where
  listBlobs (ReplicatingBlobStore _ bs) = listBlobs bs
  getBlob (ReplicatingBlobStore _ bs) = getBlob bs
  stageBlob (ReplicatingBlobStore _ bs) blob = do
    handle <- stageBlob bs blob
    pure
      StagedBlobHandle
        { blobData = blobData handle
        , abort = abort handle
        , commit = commit handle
        }

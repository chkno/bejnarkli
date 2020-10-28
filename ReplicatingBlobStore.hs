module ReplicatingBlobStore
  ( ReplicatingBlobStore(..)
  ) where

import qualified Data.ByteString.Lazy as BL

import Bejnarkli (protocolVersion)
import BlobStore
  ( BlobStore
  , ExtantBlobName(ExtantBlob)
  , StagedBlobHandle(StagedBlobHandle, abort, blobData, commit)
  , getBlob
  , listBlobs
  , stageBlob
  )

sendBlobFromStore ::
     BlobStore bs
  => bs
  -> ExtantBlobName
  -> (BL.ByteString -> IO BL.ByteString)
  -> IO ()
sendBlobFromStore bs name remoteServer = do
  blob <- getBlob bs name
  _ <-
    remoteServer $
    BL.concat [BL.pack [protocolVersion], BL.fromStrict rawname, blob]
  pure ()
  where
    (ExtantBlob rawname) = name

data ReplicatingBlobStore bs =
  ReplicatingBlobStore [BL.ByteString -> IO BL.ByteString] bs

instance BlobStore bs => BlobStore (ReplicatingBlobStore bs) where
  listBlobs (ReplicatingBlobStore _ bs) = listBlobs bs
  getBlob (ReplicatingBlobStore _ bs) = getBlob bs
  stageBlob (ReplicatingBlobStore remotes bs) blob = do
    handle <- stageBlob bs blob
    pure
      StagedBlobHandle
        { blobData = blobData handle
        , abort = abort handle
        , commit =
            \name -> do
              ename <- commit handle name
              mapM_ (sendBlobFromStore bs ename) remotes
              pure ename
        }

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Like once from the once package, but
--   * Store state persistently on disk, so it's once-ever, not once-per-execution
--   * Caller specifies the name of the action directly, rather than it being
--     automatically derived by hashing.
module PersistentOnce
  ( once
  ) where

import qualified Data.ByteString as BS
import Database.SQLite.Simple
  ( Connection
  , Only(Only)
  , execute
  , execute_
  , query
  , withConnection
  )

-- Perform action only once
once :: FilePath -> BS.ByteString -> IO () -> IO ()
once databaseFile name action =
  withConnection databaseFile checkDone >>= \case
    True -> pure ()
    False -> do
      action
      withConnection databaseFile markDone
    -- SQLite is pretty heavy-weight for this, but it
    --   * is well-supported
    --   * is unlikely to become abandonware
    --   * is well-tested
    --   * uses O(1) memory
    --   * uses O(log n) disk i/o
  where
    checkDone :: Connection -> IO Bool
    checkDone conn = do
      execute_ conn "CREATE TABLE IF NOT EXISTS once (seen BLOB PRIMARY KEY)"
      count <-
        query conn "SELECT COUNT(*) FROM once WHERE seen == (?)" (Only name) :: IO [[Int]]
      pure $ count == [[1]]
    markDone :: Connection -> IO ()
    markDone conn =
      execute conn "INSERT INTO once (seen) VALUES (?)" (Only name)

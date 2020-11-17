{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Like once from the once package, but
--   * Store state persistently on disk, so it's once-ever, not once-per-execution
--   * Caller specifies the name of the action directly, rather than it being
--     automatically derived by hashing.
module PersistentOnce
  ( once
  ) where

import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , takeMVar
  )
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Database.SQLite.Simple
  ( Connection
  , Only(Only)
  , execute
  , execute_
  , query
  , withConnection
  )
import System.IO.Unsafe (unsafePerformIO)

import Queue (Queue, dequeue, enqueue, newQueue)

-- Perform action only once
once :: FilePath -> BS.ByteString -> IO Bool -> IO Bool
once databaseFile name action =
  oneAtATime name $ onceDisk databaseFile name action

inFlight :: MVar (Map.Map BS.ByteString (Queue (MVar ())))
{-# NOINLINE inFlight #-}
inFlight = unsafePerformIO $ newMVar Map.empty

oneAtATime :: BS.ByteString -> IO Bool -> IO Bool
oneAtATime name action = do
  waitForMyTurn
  ret <- action
  wakeupNextContender -- TODO: bracket
  pure ret
  where
    waitForMyTurn :: IO ()
    waitForMyTurn = do
      myTurn <- newEmptyMVar
      needToWait <-
        modifyMVar inFlight $ \inFlightMap ->
          let waitlist = Map.lookup name inFlightMap
              waitlist' = maybe newQueue (enqueue myTurn) waitlist
           in pure (Map.insert name waitlist' inFlightMap, isJust waitlist)
      if needToWait
        then takeMVar myTurn
        else pure ()
    wakeupNextContender :: IO ()
    wakeupNextContender = do
      nextContender <-
        modifyMVar inFlight $ \inFlightMap ->
          let (next, waitlist') = dequeue (inFlightMap Map.! name)
           in let inFlightMapEntry =
                    if isNothing next
                      then Nothing
                      else Just waitlist'
               in pure
                    (Map.update (const inFlightMapEntry) name inFlightMap, next)
      maybe (pure ()) (`putMVar` ()) nextContender

onceDisk :: FilePath -> BS.ByteString -> IO Bool -> IO Bool
onceDisk databaseFile name action =
  withConnection databaseFile checkDone >>= \case
    True -> pure True
    False -> do
      ret <- action
      if ret
        then withConnection databaseFile markDone
        else pure ()
      pure ret
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

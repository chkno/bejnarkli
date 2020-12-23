module Async
  ( atLeast
  , atLeast_
  ) where

import Control.Concurrent.Async (Async, async, cancel, waitAny, waitAnyCancel)
import Data.Foldable (traverse_)
import Data.Functor (($>))

-- Like `filter (not p)`, but always remove exactly one element
removeOne :: (a -> Bool) -> [a] -> Maybe [a]
removeOne _ [] = Nothing
removeOne p (x:xs)
  | p x = Just xs
  | otherwise = (x :) <$> removeOne p xs

waitAtLeast :: Int -> [Async a] -> IO [a]
waitAtLeast 0 actions = traverse_ cancel actions $> []
waitAtLeast 1 actions = waitAnyCancel actions >>= \(_, result) -> pure [result]
waitAtLeast n actions = do
  (action, result) <- waitAny actions
  case removeOne (== action) actions of
    Just remainingActions -> (result :)
      <$> waitAtLeast (n - 1) remainingActions
    Nothing -> error
      "Async Eq doesn't work like I assumed.  Change this to keep its own indicies"

-- Like `sequence`, but finish when n actions finish.  The remaining actions are cancelled.
-- Like `race` from async, but for more than two actions
atLeast :: Int -> [IO a] -> IO [a]
atLeast n actions = traverse async actions >>= waitAtLeast n

-- Like `sequence_`, but finish when n actions finish.  The remaining actions are cancelled.
-- Like `race` from async, but for more than two actions
atLeast_ :: Int -> [IO a] -> IO ()
atLeast_ n actions = atLeast n actions $> ()

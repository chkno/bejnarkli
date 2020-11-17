module Main
  ( main
  ) where

import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Test.QuickCheck
  ( InfiniteList(InfiniteList)
  , Result
  , isSuccess
  , quickCheckResult
  )

import Queue (Queue, dequeue, enqueue, newQueue)

prop_Queue :: [Int] -> InfiniteList Bool -> Bool
prop_Queue allItems (InfiniteList popPattern _) =
  aux newQueue allItems popPattern == allItems
  where
    aux :: Queue Int -> [Int] -> [Bool] -> [Int]
    aux queue (x:xs) (False:pops) = aux (enqueue queue x) xs pops
    aux queue (x:xs) (True:pops) =
      let (queue', popped) = dequeue queue
       in case popped of
            Just pop -> pop : aux (enqueue queue' x) xs pops
            Nothing -> aux (enqueue queue' x) xs pops
    aux queue [] _ = drain queue
    aux _ _ [] = error "Pop schedule was supposed to be infinite"
    drain :: Queue Int -> [Int]
    drain queue =
      case dequeue queue of
        (queue', Just x) -> x : drain queue'
        (_, Nothing) -> []

tests :: IO [Result]
tests = sequence [quickCheckResult prop_Queue]

main :: IO ()
main = do
  allPassed <- and . fmap isSuccess <$> tests
  exitWith
    (if allPassed
       then ExitSuccess
       else ExitFailure 1)

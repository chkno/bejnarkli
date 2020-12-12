{-# LANGUAGE LambdaCase #-}

module Queue
  ( enqueue
  , newQueue
  , Queue()
  , dequeue
  ) where

-- Why is this not in the standard library?
data Queue a = Q [a] [a]

newQueue :: Queue a
newQueue = Q [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Q ins outs) = Q (x : ins) outs

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue = \case
  q@(Q [] []) -> (Nothing, q)
  Q ins (out:outs) -> (Just out, Q ins outs)
  Q ins [] -> dequeue (Q [] (reverse ins))

module Queue
  ( enqueue
  , newQueue
  , Queue()
  , dequeue
  ) where

-- Why is this not in the standard library?
data Queue a =
  Q [a] [a]

newQueue :: Queue a
newQueue = Q [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Q ins outs) = Q (x : ins) outs

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Q [] []) = (Nothing, q)
dequeue (Q ins (out:outs)) = (Just out, Q ins outs)
dequeue (Q ins []) = dequeue (Q [] (reverse ins))

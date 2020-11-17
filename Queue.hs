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

enqueue :: Queue a -> a -> Queue a
enqueue (Q ins outs) x = Q (x : ins) outs

dequeue :: Queue a -> (Queue a, Maybe a)
dequeue q@(Q [] []) = (q, Nothing)
dequeue (Q ins (out:outs)) = (Q ins outs, Just out)
dequeue (Q ins []) = dequeue (Q [] (reverse ins))

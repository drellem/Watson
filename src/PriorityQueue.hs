-- Implements a priority queue of fixed size, so when it is full, inserting a new item removes an old one
module PriorityQueue(HeapQueue,size,order,full,insert,newHeapQueue,threshold) where

import qualified Data.Heap as H

data HeapQueue a = HeapQueue Int (H.MaxPrioHeap Double a)

size :: HeapQueue a -> Int
size (HeapQueue i _) = i

order :: HeapQueue a -> Int
order (HeapQueue _ h) = H.size h

newHeapQueue :: Int -> [(Double,a)] -> HeapQueue a
newHeapQueue i l = HeapQueue i (H.fromList l)

full :: HeapQueue a -> Bool
full h = order h >= size h

overflow :: H.MaxPrioHeap Double a -> Int -> Int
overflow h len  =
  let delta =  H.size h - len in
    if delta > 0 then delta else 0

threshold :: HeapQueue a -> Double
threshold (HeapQueue i h) = case H.viewHead h of
  Just x -> fst x
  Nothing -> 0
  
insert :: HeapQueue a -> (Double,a) -> HeapQueue a
insert (HeapQueue len h) (i,e) =
  let max = case H.viewHead h of
        Just x -> fst x
        Nothing -> 0
      in
    if full (HeapQueue len h) && max < i then HeapQueue len h
    else let temp2 = H.insert (i,e) h  in
      HeapQueue len (H.drop (overflow temp2 len) temp2)



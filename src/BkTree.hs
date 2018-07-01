module BkTree where

-- import Data.PQueue.Min
import qualified SparseArray as S
import Metric
import qualified PriorityQueue as Q
import qualified Data.List as L

data BkTree a = Leaf a | Node a (S.HeapArray (BkTree a))

insert :: (Metric a) => BkTree a -> a -> BkTree a
insert (Leaf e) f = Node e (S.fromElem ((ceiling (dist e f)), Leaf f))
insert (Node e r) f =
  let d = ceiling $ dist e f
      tlist = S.getRange r (d,d) in
    case tlist of
      [] -> (Node e (r S.//[(d,Leaf f)]))
      (x:xs) -> (Node e (r S.//[(d, insert (snd x) f)]))



nearestNeighbors :: (Metric a) => BkTree a -> Int -> a -> Q.HeapQueue a
nearestNeighbors t len s = nearestNeighborsAcc t s (Q.newHeapQueue len [])

-- Fold where each intermediate result is used
--ifoldl :: S.HeapArray a -> (b->(Int,a)->

nearestNeighborsAcc (Leaf e) s acc = acc `Q.insert` (dist e s, e)
nearestNeighborsAcc (Node a r) s acc =
  let (ret,_,_,_) =
        if Q.full acc then L.foldl' (appendResults) (acc, Nothing, s, di) (S.getRange r (largest 0 (m-di), m+di))
        else L.foldl' (appendResults) (acc, Nothing, s, di) (S.toList r)
          where m = floor (Q.threshold acc)
                di = ceiling (dist a s)
  in
    ret
largest a b = if a > b then a else b

-- TODO: Should the Maybe Int be a Maybe Double? Also should refactor and put this under nearestNeighborsAcc
appendResults :: (Metric a) => (Q.HeapQueue a,Maybe Int, a, Int) -> (Int,BkTree a)->(Q.HeapQueue a,Maybe Int, a, Int)
appendResults (acc, max, s, di) (i, t) = case max of
  Nothing -> let acc2 = nearestNeighborsAcc t s acc in
    if Q.full acc2 then (acc2, Just (floor (Q.threshold acc2)), s, di) else (acc2, Nothing, s, di)
  Just maxVal -> if (largest 0 (maxVal-di))> i || maxVal + di < i then (acc, Just maxVal,s, di)
                 else let acc2 = nearestNeighborsAcc t s acc
                          maxVal2 = Q.threshold acc2 in
                        (acc2, Just (floor maxVal2), s, di)
                          

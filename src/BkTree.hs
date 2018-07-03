-- BkTree that indexes metric spaces for searching. Normally BkTrees are for discrete
-- metrics, so w/ discretize by using floor/ceiling. This is probably not the correct
-- data structure going forward, because its efficiency will vary greatly based on the
-- implemenation of the metric space (e.g. scale of distance)
module BkTree(BkTree,insert,nearestNeighbors, fromList, getMaxDistance, fromElem,(//)) where

-- import Data.PQueue.Min
import qualified SparseArray as S
import Metric
import qualified PriorityQueue as Q
import qualified Data.List as L

data BkTree a = Leaf a | Node a (S.HeapArray (BkTree a)) deriving (Show)


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
    Q.insert ret (dist a s, a)
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
                          
fromList :: (Metric a) => [a] -> Maybe (BkTree a)
fromList [] = Nothing
fromList (x:xs) = Just (L.foldl' (insert) (Leaf x) xs)

fromElem :: (Metric a) => a -> BkTree a
fromElem x = Leaf x

-- Returns all items within a given distance from the search term
getMaxDistance :: (Metric a) => BkTree a -> Double -> a -> [(Double,a)]
getMaxDistance (Leaf a) max s =  if dist a s <= max then [(dist a s, a)] else []
getMaxDistance (Node a r) max s =
  let d = dist a s
      di = ceiling (dist a s)
      m = floor max
      ret = L.foldl' (appendItems) [] (S.getRange r (largest 0 (m-di), m+di))
      appendItems l i = l ++ (getMaxDistance (snd i) max s) 
  in
    if d < max then (d,a):ret else ret

a // b = L.foldl' (insert) a b

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}



module Lib
    ( distance, Metric, diff, dist, IntArray, BkTree(BKNil, BKLeaf, BKNode), search, searchAcc, searchWithMax, PriorityQueue(Queue), point, contents, empty
    ) where

import qualified Data.Text as T
import Data.Array
import Data.List
import Control.Lens


type IntArray a = Array Int a

-- Properties: associative, reflexive, >= 0, a =b iff dist a b = 0

class Eq a => Metric a where
  dist :: a -> a -> Double

instance Eq a =>  Metric (IntArray a) where
  dist = diff

instance (Metric a, Metric b) => Metric (a, b) where
  dist (a,b) (c,d) = sqrt $ (dist a c) ^ 2 + (dist b d) ^ 2


-- data MetricTree a = Node a a (MetricTree a) (MetricTree a) | Leaf a | Nil


data BkTree a = BKNode a (IntArray (BkTree a)) | BKLeaf a | BKNil deriving Eq

data PriorityQueue a = Queue {
  _contents :: IntArray (a,Double),
  _point    :: Int,
  _empty    :: Bool
}

makeLenses ''PriorityQueue


distance :: T.Text -> T.Text -> Double
distance s t = fromIntegral $ c ! (m,n)
  where c  = listArray ((0,0),(m,n)) [compute (i,j) | i <- [0..m], j <- [0..n]]
        compute (0,j) = j
        compute (i,0) = i
        compute (i,j)
          | (s `T.index` ( i-1)) == (t `T.index` (j-1)) = c ! ((i-1,j-1))
          | otherwise = 1 + (minimum $ map (c !) [(i-1,j), (i, j-1), (i-1, j-1)])
        m = T.length s
        n = T.length t




instance Metric T.Text where
  dist = distance



-- Views are an idea I probably won't end up using
-- data View a b v = Construct (a->v) (b->v)



-- Extends the idea of Levenshtein distance 
diff :: (Eq a) => IntArray a -> IntArray a -> Double
diff s t  =  fromIntegral $ c ! (m,n)
  where c = listArray ((0,0),(m,n)) [compute (i,j) | i <- [0..m], j <- [0..n]]
        compute (0,j) = j
        compute (i,0) = i
        compute (i, j)
          | (s ! (i-1)) == (t ! (j-1)) = c ! ((i-1,j-1))
          | otherwise = 1 + (minimum $ map (c !) [(i-1, j), (i, j-1), (i-1, j-1)])
        m = length s
        n = length



-- Functions for BkTree
insert :: (Metric a) => BkTree a -> a -> BkTree a
insert (BKLeaf l) m = BKNode l (array (0, d) [(d,BKLeaf m)])
  where d = ceiling (dist l m)

insert (BKNode n c) m = BKNode n (c//[(d, Lib.insert (c!d) m)])
  where d = ceiling (dist n m)

insert BKNil m = BKLeaf m

buildBkTree :: (Metric a) => [a] -> BkTree a
buildBkTree l = foldl' (Lib.insert) (BKNil) l






-- PriorityQueue functions
threshold :: PriorityQueue a -> Double
threshold q = if view empty q then 0 else snd ((view contents q)!((view point q)-1))

full :: PriorityQueue a -> Bool
full = length (view contents q) == point


addElem :: IntArray (a, Double) -> (a, Double) -> IntArray (a, Double)
addElem r e =
  isearch r e 0 (length r)


-- Binary search with range specified
isearch r e i j =
          if i == j then if snd (r!i) < snd e then r else
            insertAt r i e else 
            let k = floor (i + j)/2 in
              if snd (r!k) < snd e then isearch r e k j else isearch r e i k

              
append :: PriorityQueue a -> (a,Double) -> PriorityQueue a
append q (s,d) =
    if d <= threshold q then Queue { _contents = (addElem (view contents q) (s,d)), _point = (view point q)+1, _empty = False }
    else q


-- Can replace this with a proper merge to optimize, right now complexity is bad
(++) ::  PriorityQueue a -> PriorityQueue a -> PriorityQueue a
a ++ b = listArray (0,(length r)-1) (take r (merge (toList a) (toList b)))

merge :: [a,Double] -> [a,Double] -> [a,Double]
merge s [] = s
merge [] t = t
merge ((x,xd):xs) ((y,yd):ys) =
  if xd < yd then (x,xd):(merge xs ((y,yd):ys)) else (y,yd):(merge ((x,xd):xs) ys )

insertAt :: IntArray a -> Int -> a -> IntArray a
insertAt r i e = listArray (0, length r) [if m == i then e else if m < i then r!m else r!(m+1) | m<- [0..(length r) - 1]]

toList :: IntArray a -> [a]
toList r = map (r!) [0..length r - 1]

split :: (a->Bool) -> [a] -> ([a],[a])
split f [] = ([],[])
split f (x:xs) =
  let (l, m) = split f xs in
    if f x then (x:l, m) else (l, x:m)


createQueue :: IntArray (a, Double) -> PriorityQueue a
createQueue r = Queue { _contents = r, _point = length r, _empty = length r == 0}

emptyQueue :: Int -> PriorityQueue a
emptyQueue len = Queue { _contents = array (0,len), _point = 0, empty = True }

search :: (Metric a) =>  a -> BkTree a -> Int -> PriorityQueue a

search s docs len =
  searchAcc s docs (emptyQueue len)


searchAcc :: (Metric a) => a -> BkTree a  -> PriorityQueue a -> PriorityQueue a

searchAcc s BKNil acc = emptyQueue len

searchAcc s (BKLeaf d) acc =
  append acc (d, dist s d)


searchAcc s (BKNode n c) acc =
    if full acc2 then searchWithMAx s (BKNode n c) acc thresh
    else foldl' (++) (acc2) (map (\i -> searchAcc s (c!i) acc2) [(largest 0 (dii - thresh))..dii + thresh])
    where di = dist s d
          dii = ceiling di
          acc2 = if full acc && di <= threshold acc then append acc (n, di) else acc
          thresh = threshold acc2


searchWithMax :: (Metric a) => a -> BkTree a   -> PriorityQueue a -> Double -> PriorityQueue a
searchWithMax _ BKNIL _ _  = emptyQueue len
searchWithMax s (BKLeaf d) acc max =
  if di > max  then acc
  else append acc (d, di)
       where di = dist s d
searchWithMax s (BKNode n c) acc max =
  foldl' (++) (acc2) (map (\i -> searchWithMax s (c!i) acc2 (smallest thresh max)) [(largest 0 (dii - thresh))..dii + thresh])
  where di = dist s d
        dii = ceiling di
        acc2 = if di <= max then append acc (n, di) else acc
        thresh = threshold acc2

smallest a b = if a < b then a else b
  
largest a b = if a < b then b else a

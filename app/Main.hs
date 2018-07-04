{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Lib
import Metric 
import qualified Data.Text as T
import qualified BkTree as B
import qualified SparseArray as S
import qualified PriorityQueue as Q

a :: (T.Text, T.Text)
a = ("This is my initial search string", "Some semantic information")

b :: (T.Text, T.Text)
b = ("Candidate file", "Some semantic information")

d :: (T.Text, T.Text)
d = ("Another string", "With semantic stuff around")

e :: (T.Text, T.Text)
e = ("Again another", "Here we go")

c :: B.BkTree (T.Text, T.Text)
c = B.fromElem a B.// (b:d:e:[])

mylist :: [Double]
mylist = [3.8, 7.5, 8.6, 4.2, 4.0]

dTree :: B.BkTree Double
dTree = B.fromElem 2.4 B.//mylist

s :: S.HeapArray (Double)
s = S.fromElem  (1,3.7 :: Double) S.// ([(1,3.8), (2, 7.5), (5, 8.6), (3, 4.2), (6, 4.0)] :: [(Int,Double)])

  
tree :: Maybe(B.BkTree (Double))
tree = B.fromList mylist

--main :: IO ()
--main = putStrLn $ show $ Q.toList $ (B.nearestNeighbors dTree 3 2)

biglist = [1..10000] :: [Int]
bigtree = B.fromElem 4 B.//biglist

-- main = putStrLn $ show $ qsort $ Q.toList $ (B.nearestNeighbors bigtree 20 546)
-- main = putStrLn $ show $ B.toJSON dTree
-- main = putStrLn $ show $ B.getMaxDistance bigtree 14.3 546
main = putStrLn $ show $ B.getMaxDistance bigtree 5.0 0

split :: (a->Bool)->[a]->([a],[a])
split _ [] = ([],[])
split f (x:xs) = if f x then (x:a, b) else (a, x:b)
  where (a,b) = split f xs
  
qsort :: (Ord a) => [(a,b)] -> [(a,b)]
qsort [] = []
qsort ((x,y):xs) =
  let (a,b) = split (\y -> fst y < x) xs in
    qsort a ++ [(x,y)] ++ qsort b

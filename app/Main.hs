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
mylist = [3.8, 7.5, 8.6, 4,2, 4.0]

s :: S.HeapArray (Double)
s = S.fromElem  (1,3.7 :: Double) S.// ([(1,3.8), (2, 7.5), (5, 8.6), (3, 4.2), (6, 4.0)] :: [(Int,Double)])

instance Metric Double where
  dist a b = if a-b < 0 then b-a else a-b
  
tree :: Maybe(B.BkTree (Double))
tree = B.fromList mylist

main :: IO ()
main = putStrLn $ show $ Q.toList $ (B.nearestNeighbors c 3 (("str", "info is really aroudn fund") :: (T.Text,T.Text)))


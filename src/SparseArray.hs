{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- Needs to implement sparse arrays in a manner that quickly get the indices within a given range
-- Planned implemenation is to use a heap
-- This will be used to implement the children of a BkTree node, i.e. all of the elements of a given distance from the parent
module SparseArray(SparseArray, (//), getRange, fromElem, HeapArray, toList) where

import qualified Data.List as L
import Data.Aeson
import GHC.Generics

class SparseArray c a where
  -- Updates the array with a list of key-value pairs
  (//) :: c -> [(Int,a)] -> c
  -- Gets all key-value pairs with keys in a given range
  getRange :: c -> (Int,Int) -> [(Int,a)]
  fromElem :: (Int,a) -> c

-- These rules:
-- val of node is <= val of children
-- child w/ smallest val comes first in the branch
-- no two elements can have the same index
-- indices range from 0..



instance SparseArray (HeapArray a) a where
  c // [] = c
  c // l = L.foldl' (insert) (c) l

  getRange = rangeQuery

  fromElem x = Leaf (fst x) (snd x)
  
data HeapArray a = Leaf Int a | Branch Int a (HeapArray a) (HeapArray a) | HalfBranch Int a Int a deriving (Show, Generic, ToJSON, FromJSON)

smallest a b = if a < b then a else b

val :: HeapArray a -> Int
val (Leaf i _) = i
val (Branch i _ _ _) = i
val (HalfBranch i _ _ _) = i


makeHalfBranch :: Int -> a -> Int -> a -> HeapArray a
makeHalfBranch i e j f =
  if i < j then HalfBranch i e j f
  else HalfBranch j f i e

insert :: HeapArray a -> (Int,a) -> HeapArray a
insert (Leaf i e) (j,f) = if i == j then Leaf i f else makeHalfBranch i e j f
insert (Branch i n s t) (j,f) =
  if j == i then Branch i f s t else
    if j < i then
      if i <= val s then Branch j f (insert s (i,n)) t
      else Branch j f s (insert t (i,n))
    else
      if j <= val s then Branch i n (insert s (j,f)) t
      else Branch i n s (insert t (j,f))
insert (HalfBranch i n ci ce) (j,f) =
  if j == i then HalfBranch i f ci ce
  else if j < i then
    if i < ci then Branch j f (Leaf i n) (Leaf ci ce) else Branch j f (Leaf ci ce) (Leaf i n)
  else
    if j == ci then HalfBranch i n j f
    else if j< ci then Branch i n (Leaf j f) (Leaf ci ce) else Branch i n (Leaf ci ce) (Leaf j f)


rangeQuery :: HeapArray a->(Int,Int) -> [(Int,a)]

rangeQuery (Leaf i e) (j,k) = if j<= i && i<=k then [(i,e)] else []

rangeQuery (HalfBranch i a j b) (l,u) =
  if i > u then []
  else  let rem = (if j <= u then [(j,b)] else []) in
    if i < l then rem else (i,a):rem
    
rangeQuery (Branch i n s t) (l,u) =
  if i > u then []
  else let rem = (rangeQuery s (l,u)) ++ (rangeQuery t (l,u)) in
           if i < l then rem else (i,n):rem

toList :: HeapArray a -> [(Int,a)]
toList (Leaf i e) = [(i,e)]
toList (HalfBranch i a j b) = (i,a):[(j,b)]
toList (Branch i n s t) = (i,n):(toList s) ++ (toList t)
                              



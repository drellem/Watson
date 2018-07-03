
{-# LANGUAGE FlexibleInstances #-}

module Metric where

import Data.Array
import qualified Data.Text as T

-- Todo add some quickcheck properties

class Eq a => Metric a where
  dist :: a -> a -> Double

instance (Metric a, Metric b) => Metric (a, b) where
  dist (a,b) (c,d) = sqrt $ (dist a c) ^ 2 + (dist b d) ^ 2

instance Metric T.Text where
  dist = distance

instance Metric Double where
  dist x y = if x -y > 0 then x-y else y-x

instance Metric Int where
  dist x y = fromIntegral $
    if x -y > 0 then x-y else y-x

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





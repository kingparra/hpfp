module Lib (sumTo) where

sumTo :: (Eq a, Num a) => a -> a
sumTo n = case signum n of
  0    -> 0
  1    -> n + sumTo (n-1)
  (-1) -> n + sumTo (n+1)

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lib (sumTo) where

sumTo :: (Eq a, Num a) => a -> a
sumTo n = case signum n of
  (-1) -> n + sumTo (n+1)
  1    -> n + sumTo (n-1)
  0    -> 0
  _    -> error "sumTo received an invalid input"

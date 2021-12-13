{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Lib (sumTo) where

-- Because GHC doesn't know that signum can only return 1, -1, or
-- 0, it assumes that any number representable by Int may be the
-- result, so the fallback case is necessary to suppress our
-- warning.
sumTo :: (Eq a, Num a) => a -> a
sumTo n = case signum n of
  (-1) -> n + sumTo (n+1)
  1    -> n + sumTo (n-1)
  0    -> 0
  _    -> error "sumTo received an invalid input"

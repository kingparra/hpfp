module Lib (mult) where

mult :: Integral a => a -> a -> a
mult x y
  -- moved out of inner function in case
  -- progressing from a negative number
  -- to a positive ends evaluation early
  | x == 0 = 0
  | y == 0 = 0
  | otherwise =
      let go n m count
            -- negative numbers only work for the first input...
            | count == abs m = n
            | otherwise      = go (n+x) m (count+1)
      in  go x y 1

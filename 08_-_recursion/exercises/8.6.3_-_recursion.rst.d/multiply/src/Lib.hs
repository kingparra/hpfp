module Lib (mult) where

mult :: Integral a => a -> a -> a
mult x y = go x y 1
  where
    go 0 _ _ = 0
    go _ 0 _ = 0
    go n 1 _ = n
    go 1 m _ = m
    go n m count
      | count == abs m  = n
      | count <  abs m  =
        case signum x of 
          -1 -> go (n-x) m (count+1)
          1  -> go (n+x) m (count+1)

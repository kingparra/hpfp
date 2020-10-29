module Lib (mult) where

mult :: Integral a => a -> a -> a
mult x y =
  let go n m count
        | n == 0 = 0
        | m == 0 = 0
        | count == abs m = n
        | otherwise      = go (n+x) m (count+1)
  in  go x y 1

module Lib (mult) where

mult :: Integral a
     => a -> a -> a
mult x y
  |        x == 0     =   0
  |        y == 0     =   0
  | signum y == 1     =   go x y 1
  | signum y == (-1)  =   negate $ go x (abs y) 1
  where
    go n m count
      | count == abs m  =  n
      | otherwise       =  go (n+x) m (count+1)

--mult :: Integral a => a -> a -> a
--mult x y =
--  if signum y == (-1)
--  then  negate . foldr (+) 0 . take (abs y) $ repeat x
--  else           foldr (+) 0 . take (abs y) $ repeat x

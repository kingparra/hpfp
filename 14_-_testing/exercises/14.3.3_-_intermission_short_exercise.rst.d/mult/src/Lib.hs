module Lib where

mult x y =
  case signum y of
    0    -> 0
    1    -> incrementBy 1 x
    (-1) -> negate (incrementBy 1 x)
  where
    incrementBy counter n =
      if counter /= abs y
      then incrementBy (counter+1) (n+x)
      else n

module Lib where
import Data.List


-- Question 1
half x = x / 2


-- Question 2
-- for any list you apply sort to this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


-- Question 3
plusAssociative x y z  =  x + (y + z) == (x + y) + z
plusCommutative x y    =  x + y == y + x

module Lib where
import Data.List (elemIndex)



-- Question 1
added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1,2,3] [4,5,6])



---- Question 2
--y :: Maybe Integer
--y =  lookup 3 $ zip [1,2,3] [4,5,6]

--z :: Maybe Integer
--z = lookup 2 $ zip [1,2,3] [4,5,6]

----tupled :: Maybe (Integer, Integer)
--tupled = (,) y z



---- Question 3
--x' :: Maybe Int
--x' = elemIndex 3 [1,2,3,4,5]

--y' :: Maybe Int
--y' = elemIndex 4 [1,2,3,4,5]

--max' :: Int -> Int -> Int
--max' = max

--maxed :: Maybe Int
--maxed = max' x' y'

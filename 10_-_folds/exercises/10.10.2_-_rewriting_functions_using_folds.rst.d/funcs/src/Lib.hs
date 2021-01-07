module Lib where
import Data.List (foldl', foldr1)

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- Question 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- Question 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x xs -> f x || xs) False

-- Question 3
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\y ys -> y == e || ys) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (== e)

-- Question 4
myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

-- using foldr
-- is this lazy?
myReverse' :: [a] -> [a]
myReverse' l = (foldr (\x r -> r . (x:)) id l) []

-- Question 5
myMap f = foldr (\x y -> f x : y) []

-- Question 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x xs -> if p x then x:xs else xs) []

-- Question 7
{-# ANN squish ("Hlint: disable Use concat" :: String) #-}
squish :: [[a]] -> [a]
squish = foldr (++) []

-- Question 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x xs -> f x ++ xs) []

-- -- Question 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Question 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy p =
  foldr1 (\x xs -> if (p x xs) == GT then x else xs)

-- Question 11
myMinimumBy p =
  foldr1 (\x xs -> if (p x xs) == LT then x else xs)

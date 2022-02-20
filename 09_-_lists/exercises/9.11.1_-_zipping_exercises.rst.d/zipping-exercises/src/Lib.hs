module Lib where


-- Question 1
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


-- Question 2
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys


-- Question 3
zip'Refactored :: [a] -> [b] -> [(a,b)]
zip'Refactored = zipWith' (,)

module Lib where


myOr :: [Bool] -> Bool
myOr = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x xs -> f x || xs) False


myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x xs -> x == e || xs) False


myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

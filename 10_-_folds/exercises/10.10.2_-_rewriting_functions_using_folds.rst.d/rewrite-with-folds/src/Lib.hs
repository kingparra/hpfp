module Lib where


-- Question 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False


-- Question 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x xs -> f x || xs) False


-- Question 3
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x xs -> x == e || xs) False


-- Question 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


-- Question 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []


-- Question 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr (\x xs -> if f x == True
                  then x : xs
                  else xs) []


-- Question 7
squish :: [[a]] -> [a]
squish = foldr (++) []


-- Question 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f


-- Question 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- Question 10
-- I could reutrn mempty if a had a constraint of Monoid, but
-- according to this type signature, I have to throw an exception
-- for empty lists.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl1 (\x y -> case f x y of { GT -> x; _ -> y })


-- Question 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl1 (\x y -> case f x y of { LT -> x; _ -> y })

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


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr (\x xs -> if f x == True
                  then x : xs
                  else xs) []


squish :: [[a]] -> [a]
squish = foldr (++) []


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- I could reutrn mempty if a had a constraint of Monoid, but according
-- to this type signature, I have to throw an exception for empty lists.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl1 (\x y -> case f x y of { GT -> x; _ -> y })


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl1 (\x y -> case f x y of { LT -> x; _ -> y })

module Lib where
import Data.List (foldl')

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- Question 1
myOr :: [Bool] -> Bool
myOr = foldl' (||) False

-- Question 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = myOr (map f l)

-- Question 3
-- This one was fun! Cons cells are replaced before the
-- ys is substituted in. So in this expression...
--
-- myElem 'f' False "abf" >
--
-- ... the ys are actually nested function calls, like so..
--
-- (\'a' (..see rhs..) -> 'a' == 'f' ||
--   (\'b' (..see rhs..) -> 'b' == 'f' ||
--     (\'f' False -> 'f' == 'f' || False)
--   )
-- )
--
-- ('a' == 'f' || 'b' == 'f' || 'f' == 'f' || False)
--
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\y ys -> y == e || ys) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (== e)

-- Question 4
myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []
-- Can I rewrite this to use foldr?
-- rev xs = (foldr (\x r -> r . (x:)) id xs) []

-- Question 5
-- squish flattens a list of lists into a list
{-# ANN squish ("Hlint: disable Use concat" :: String) #-}
squish :: [[a]] -> [a]
squish = foldr (++) []

-- Question 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f l = squish $ map' f l
  where map' f [] = []
        map' f (x:xs) = f x : map' f xs

-- Question 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Question 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "empty list"
myMaximumBy f [x] = x
myMaximumBy f l@(x:y:xs) =
  case x `f` y of
    GT -> myMaximumBy f (x:xs)
    LT -> myMaximumBy f (y:xs)
    EQ -> myMaximumBy f (y:xs)

-- Question 9
myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMaximumBy (flip compare)

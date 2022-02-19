module Lib where


-- Example 0
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs


-- Question 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs


-- Question 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny p [] = False
myAny p (x:xs) = p x || myAny p xs


-- Question 3
infix 4 `myElem`
myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) = e == x || myElem e xs


-- Question 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- Question 4, take two.
myRev :: [a] -> [a]
myRev l  = go l []
  where
    go [] new = new
    go (x:xs) new = go xs (x:new)


-- Question 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


-- Question 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f l  = (squish . map' f) l
  where
    map' g [] = []
    map' g (x:xs) = g x : map' g xs


-- Question 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- Question 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "myMaximumBy does not work on empty lists"
myMaximumBy p (e:l)
  | all (\x -> x == GT || x == EQ) (map (e `p`) l) = e
  | otherwise = myMaximumBy p l


-- Question 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy p [] = error "myMinimumBy only works on nonempty finite lists"
myMinimumBy p (e:l)
  | all (\x -> x == LT || x == EQ) (map (e `p`) l) = e
  | otherwise = myMinimumBy p l


-- Question 10
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare


myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

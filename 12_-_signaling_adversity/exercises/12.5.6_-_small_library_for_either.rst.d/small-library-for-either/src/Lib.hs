module Lib where


-- Question 1
-- Try to arrive at a solution that uses
-- foldr, even if earlier versions don't.

-- Non-foldr version
-- lefts' :: [Either a b] -> [a]
-- lefts'  ((Left x):xs) = x : lefts' xs
-- lefts' ((Right _):xs) = lefts' xs
-- lefts'             [] = []

-- Using foldr...
lefts' :: [Either a b] -> [a]
lefts' = foldr (\e xs ->
  case e of
    Left x  -> x : xs
    Right _ -> xs) []


-- Question 2
-- Same as the last one, use foldr
-- eventually.
rights' :: [Either a b] -> [b]
rights' = foldr (\e xs ->
  case e of
    Right x -> x : xs
    Left _  -> xs) []


-- Question 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' l = (lefts' l, rights' l)


-- Question 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f x = case x of
  Right b -> Just (f b)
  Left _  -> Nothing


-- Question 5
-- This is a general catamorphism for
-- Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g e = case e of
  Left a -> (f a)
  Right b -> (g b)


-- Question 6
-- Same as before, but use the either
-- function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (Just . f) x

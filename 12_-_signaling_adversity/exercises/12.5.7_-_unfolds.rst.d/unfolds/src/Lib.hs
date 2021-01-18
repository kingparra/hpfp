module Lib where


-- Question 1
iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x : iterate'' f (f x)


-- Question 2
unfoldr'' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr'' f x = case f x of
  Just (a,b) -> a : unfoldr'' f b
  Nothing -> []

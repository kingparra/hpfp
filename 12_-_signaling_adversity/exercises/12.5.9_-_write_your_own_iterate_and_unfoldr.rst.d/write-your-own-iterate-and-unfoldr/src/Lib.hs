module Lib where


-- Question 1
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


-- Question 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just (a,b) -> a : myUnfoldr f b
  Nothing    -> []


-- Question 3
-- Use myUnfoldr to implement this.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\i -> Just (i,f i)) x

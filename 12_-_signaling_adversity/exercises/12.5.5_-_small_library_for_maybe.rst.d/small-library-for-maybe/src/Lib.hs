module Lib where


-- Question 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust


-- Question 2
-- mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f m = case m of
  Just a   ->  f a
  Nothing  ->  b


-- Question 3
fromMaybe :: a -> Maybe a -> a
fromMaybe = undefined


-- Question 4
listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

maybeToList :: Maybe a -> [a]
maybeToList = undefined


-- Question 5
catMaybes :: [Maybe a] -> [a]
catMaybes = undefined


-- Question 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = undefined

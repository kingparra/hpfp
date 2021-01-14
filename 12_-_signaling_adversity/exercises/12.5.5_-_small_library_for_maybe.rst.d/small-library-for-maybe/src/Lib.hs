module Lib where


-- Question 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust


-- Question 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f m = case m of
  Just a   ->  f a
  Nothing  ->  b


-- Question 3
fromMaybe :: a -> Maybe a -> a
-- fromMaybe i Nothing = i
-- fromMaybe _ (Just x) = x
fromMaybe i m = mayybee i id m


-- Question 4
listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList m = case m of
  Just x   ->  [x]
  Nothing  ->   []


-- Question 5
catMaybes :: [Maybe a] -> [a]
catMaybes ((Just a):xs)  =  a : catMaybes xs
catMaybes  (Nothing:xs)  =      catMaybes xs
catMaybes            []  =                []


-- Question 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe l =
  if all isJust l
  then Just (catMaybes l)
  else Nothing

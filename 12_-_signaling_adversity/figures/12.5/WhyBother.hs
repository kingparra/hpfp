module WhyBother where
import Data.List

-- This code is only for use as a figure. I've put it in a project
-- here in order to play with it using ``stack ghci`` more easily.
mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n+x) xs)


niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0


mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n*x) xs)


niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1


mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = (go (xs' ++ x) xs)


niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

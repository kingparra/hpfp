{-# LANGUAGE NoImplicitPrelude #-}
module Lib where
import Prelude hiding
  ( sum , product , elem , minimum , maximum
  , null , length, foldr, foldMap )
import qualified Data.Foldable hiding
  ( sum , product , elem , minimum , maximum
  , null , length , toList , fold , foldMap )
import Data.Monoid


-- Question 1
sum :: (Foldable t, Num a) => t a -> a
sum x = getSum $ foldMap Sum x


-- Question 2
product :: (Foldable t, Num a) => t a -> a
product x = getProduct $ foldMap Product x


-- Question 3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e t = foldr (\x xs -> e == x || xs) False t


-- Question 4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum t
  | null t    = Nothing
  | otherwise = undefined -- getFirst $ foldMap (First . Just) t {- this is wrong -}


maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum t
  | null t    = Nothing
  | otherwise = undefined


-- Question 6
null :: (Foldable t) => t a -> Bool
null x = length x == 0


-- Question 7
length :: (Foldable t) => t a -> Int
length t = foldr (\x xs -> 1 + xs) 0 t


-- Question 8
toList :: (Foldable t) => t a -> [a]
toList t = foldr (\x xs -> x : xs) [] t


-- Question 9
fold :: (Foldable t, Monoid m) => t m -> m
fold x = foldMap id x


-- Question 10
-- Write this function in terms of foldr.
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f x = foldr (mappend . f) mempty x


foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr = Data.Foldable.foldr

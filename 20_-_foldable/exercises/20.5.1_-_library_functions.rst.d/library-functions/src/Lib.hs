{-# LANGUAGE NoImplicitPrelude #-}
module Lib where

-- Hide list processing functions that clash
-- with the names we're defining, but make
-- everything else available.
import Prelude hiding
  ( sum , product , elem , minimum , maximum
  , null , length, foldMap )

-- Hide all functions from Foldable other than
-- foldr. The typeclass Foldable is visisble.
import Data.Foldable hiding
  ( sum , product , elem , minimum , maximum
  , null , length , toList , fold , foldMap )

import Data.Monoid


-- Implement these in terms of foldr or
-- foldMap, then try them out with multiple
-- types that have Foldable instances.

-- Question 1
sum :: (Foldable t, Num a) => t a -> a
sum x = undefined


-- Question 2
product :: (Foldable t, Num a) => t a -> a
product x = undefined


-- Question 3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x y = undefined


-- Question 4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum t = undefined


-- Question 5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum x = undefined


-- Question 6
null :: (Foldable t) => t a -> Bool
null x = undefined


-- Question 7
length :: (Foldable t) => t a -> Int
length x = undefined


-- Question 8
toList :: (Foldable t) => t a -> [a]
toList x = undefined


-- Question 9
fold :: (Foldable t, Monoid m) => t m -> m
fold x = foldMap id x


-- Question 10
-- Write this function in terms of foldr.
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f x = foldr (mappend . f) mempty x

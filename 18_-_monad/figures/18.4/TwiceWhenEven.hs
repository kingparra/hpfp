module TwiceWhenEven where

-- This squares every element, regardless of
-- if they are even or odd, but duplicates
-- only even numbers within the list.
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else [x*x]

-- Maybe duplicateAndSquareWhenEven is a
-- better name for this function.
twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else []

-- These functions have too many
-- responsiblities, and should be split up.

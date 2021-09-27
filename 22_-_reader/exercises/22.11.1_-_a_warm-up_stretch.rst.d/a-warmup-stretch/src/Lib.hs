module Lib where
import Control.Applicative
import Data.Maybe

-- Some sample data for us to manipulate.
x = [1,2,3]
y = [4,5,6]
z = [7,8,9]


-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = undefined


-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = undefined


-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y


-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = undefined


-- Now we want the ability to make a Maybe (,) of values using
-- Applicative.

-- Have x1 make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = undefined


-- x2 should make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = undefined


-- x3 takes one input and makes a tuple of the results of two
-- applications of z' from above.
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 =  undefined


-- summed is uncurry with addition as the first argument
summed :: Num c => (c, c) -> c
summed = undefined


-- This function lifts a boolean function over two partially
-- applied functions. Use &&, >3, <8
bolt :: Integer -> Bool
bolt = undefined


-- Functions for the last part of the exercise.
sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m


s' = summed <$> ((,) <$> xs <*> ys)

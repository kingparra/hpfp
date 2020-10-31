module Lib (dividedBy', DividedResult(..)) where

data DividedResult = Result Integer | DividedByZero deriving (Eq, Show)

dividedBy' num denom = undefined

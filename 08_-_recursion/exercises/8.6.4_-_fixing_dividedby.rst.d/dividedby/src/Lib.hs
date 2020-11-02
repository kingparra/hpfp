module Lib (dividedBy', DividedResult(..)) where

data DividedResult = Result Integer | DividedByZero deriving (Eq, Show)

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' num denom
  |                       signum denom == 0      =    DividedByZero
  |    signum num == 0 && signum denom /= 0      =    Result 0
  |    signum num == 1 && signum denom == 1      =    Result (num `div` denom)
  | signum num == (-1) || signum denom == (-1)   =    Result (num `div` denom)

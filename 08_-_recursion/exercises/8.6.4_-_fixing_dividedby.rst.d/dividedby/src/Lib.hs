module Lib (dividedBy', DividedResult(..)) where

data DividedResult = Result Integer | DividedByZero deriving (Eq, Show)

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' num denom
  |                       signum denom == 0      =    DividedByZero
  |    signum num == 0 && signum denom /= 0      =    Result 0
  |    signum num == 1 && signum denom == 1      =    Result (num `divPlus` denom)
  | signum num == (-1) && signum denom == (-1)   =    Result (abs num `divPlus` abs denom)
  | signum num == (-1) || signum denom == (-1)   =    Result (num `divNeg` denom)
  where
    divPlus x y = fst $ dividedBy x y
    divNeg x y  = negate $ fst $ dividedBy (abs x) (abs y)

-- The old implementation of dividedBy, which works only for positive inputs.
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
  go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

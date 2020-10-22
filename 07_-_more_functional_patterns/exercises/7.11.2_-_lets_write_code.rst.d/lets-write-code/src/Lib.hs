module Lib where


-- 1
-- Original version.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- a) Modified to use divMod.
tensDigit' x = let (y,_) = x `divMod` 10 in y `mod` 10

-- b) Modified to return the hundreds digit instead.
hunsD x = (x `div` 100) `mod` 10


-- 2
foldBool :: a -> a -> Bool -> a
foldBool = undefined

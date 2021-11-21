-- This won't work. It never stops.
brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

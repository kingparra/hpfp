-- 8.2 Factorial!, page 278
--
module Factorial where


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-
brokenFact1 4 =
  4 * (4 - 1)
  4 * ((4 - 1) - 1)
  4 * (((4 - 1) - 1) - 1)
  4 * ((((4 - 1) - 1) - 1) - 1)
  4 * (((((4 - 1) - 1) - 1) - 1) - 1)
  -- never stops
-}

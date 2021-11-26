-- 8.2 Factorial!, Paragraph 5, page 277
-- "Let's look at some broken code to introduce the concept of a base case:" ...

-- This won't work. It never stops.
brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

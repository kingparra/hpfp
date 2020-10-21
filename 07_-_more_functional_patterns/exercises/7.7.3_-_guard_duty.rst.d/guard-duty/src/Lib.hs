module Lib where
-- 1
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

avgGradeUnconditional :: (Fractional a, Ord a) => a -> Char
avgGradeUnconditional x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

-- 2
avgGradeReordered :: (Fractional a, Ord a) => a -> Char
avgGradeReordered x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

avgGradeOrderIndependent :: (Fractional a, Ord a) => a -> Char
avgGradeOrderIndependent x
  | y >= 0.7  && y < 0.8 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 && y < 0.9 = 'B'
  | y >= 0.59 && y < 0.7 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

-- 3
{-# LANGUAGE NoMonomorphismRestriction #-}
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- 7 and 8
numbers x | x < 0 = -1
          | x == 0 = 0
          | x > 0 = 1

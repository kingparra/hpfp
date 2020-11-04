module Lib where
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "digitToWord enountered a number that is not a single digit"

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [ n `mod` 10 ]

wordNumber :: Int -> String
wordNumber n = if signum n == (-1)
  then "negative-" ++ wordNumber (abs n)
  else concat . intersperse "-" . map digitToWord $ digits n

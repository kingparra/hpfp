module Lib where
import Data.Char (isPunctuation)
-- 3.8.1 Building functions

-- Questions 1 and 2, page 83..84
a x = x ++ "!"
b x = if x == "Curry is awesome!" then "y" else "n"
c x = drop 9 x

-- Question 3, page 84
thirdLetter x = x !! 2

-- Question 4, page 85
letterIndex x = "Curry is awesome!" !! (x-1)

-- Question 5, page 85
-- rvrs = reverse . words . filter (not . isPunctuation)
rvrs = awesome ++ " " ++ is ++ " " ++ curry
  where
    x = "Curry is awesome!" :: String
    curry     = take (length "Curry") x
    isAwesome = drop (length "Curry ") x
    is        = take (length "is") isAwesome
    awesome   = take (length "awesome") $ drop (length "is ") isAwesome

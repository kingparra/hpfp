module Lib where
import Data.Char


preprocess text =
  filter isAlpha (map toLower text)


isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where cleanText = preprocess text

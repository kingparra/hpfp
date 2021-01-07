module Lib where
import Data.Char (isUpper, toLower, toUpper, ord, chr)
import Data.List (lookup)
import Data.Function ((&))

{-# ANN caesar ("Hlint: ignore Eta reduce" :: String) #-}
caesar :: Int -> String -> String
caesar shift str =
  let
    shiftCh ch =
      if   isUpper ch
      then toLower ch & shiftCh & toUpper
      else case lookup ch (zip ['a'..'z'] [1..26]) of
             Nothing        -> ch
             Just position  -> cycle ['a'..'z'] !! (position - 1 + shift)
  in map shiftCh str

{-# ANN unCaesar ("Hlint: ignore Eta reduce" :: String) #-}
unCaesar :: Int -> String -> String
unCaesar shift str = caesar (26 - shift) str

lcaseAlpha = zip (map ord ['a'..'z']) ['a'..'z']

calculateShift :: Char -> Char -> Int
calculateShift pc cc = ord pc - ord cc

vigenere :: String -> String -> String
vigenere key plaintext = undefined

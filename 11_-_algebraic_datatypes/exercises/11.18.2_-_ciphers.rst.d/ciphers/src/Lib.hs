module Lib where
import Data.Char (isUpper, toLower, toUpper, ord, chr)


{-# ANN caesar ("Hlint: ignore Eta reduce" :: String) #-}
caesar :: Int -> String -> String
caesar shift str =
  let
    shiftChr chr =
      if   isUpper chr
      then toUpper (shiftChr (toLower chr))
      else case lookup chr (zip ['a'..'z'] [1..26]) of
             Nothing        -> chr
             Just position  -> cycle ['a'..'z'] !! (position - 1 + shift)
  in map shiftChr str


{-# ANN unCaesar ("Hlint: ignore Eta reduce" :: String) #-}
unCaesar :: Int -> String -> String
unCaesar shift str = caesar (26 - shift) str


vigenere key msg =
  zipWith enc (stream key') msg'
  where
    offset x = ord x - ord 'a'
    stream xs = map offset . cycle $ xs
    enc x y = chr $ ord 'a' + ((offset y + x) `mod` alphaSize)
    enc' x y
      | y == ' '   =  ' '
      | otherwise  =  enc x y
    alphaSize = ord 'z' - ord 'a' + 1
    key' = map toLower $ key
    msg' = map toLower $ msg

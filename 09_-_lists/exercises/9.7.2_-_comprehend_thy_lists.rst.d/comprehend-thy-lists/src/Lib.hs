module Lib where

mySqr = [x^2 | x <- [1..10]]

{-# ANN one ("HLint: ignore" :: String) #-}
one = [x | x <- mySqr, x `rem` 2 == 0]

{-# ANN two ("HLint: ignore" :: String) #-}
two = [ (x, y) | x <- mySqr
               , y <- mySqr
               , x < 50
               , y > 50 ]

{-# ANN three ("HLint: ignore" :: String) #-}
three = take 5 [ (x, y) | x <- mySqr
                        , y <- mySqr
                        , x < 50
                        , y > 50 ]

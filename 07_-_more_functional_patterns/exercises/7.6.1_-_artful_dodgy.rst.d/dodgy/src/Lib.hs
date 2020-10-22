module Lib where

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

{-# ANN oneIsTwo ("HLint: ignore Redundant bracket" :: String) #-}
oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

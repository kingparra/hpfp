#!/usr/bin/env stack
{- stack script
     --compile
     --copy-bins
     --snapshot lts-22.32
     --package hspec
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec


data Cow = Cow {
    name :: String
  , age  :: Int
  , weight :: Int
  } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str


noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let 
    w = weight c
    n = name c
  in
    if n == "Bess" && w > 499
    then Nothing
    else Just c


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)


mkSphericalCow' name' age' weight' = do
  nammy   <- noEmpty name'
  agey    <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)


-- Exploding an spherical cow
-- instance Monad Maybe where
--   return x        =  Just x
--   (Just x) >>= k  =  k x
--   Nothing >>= _   =  Nothing

mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \nammy ->
      noNegative age' >>=
        \agey ->
          noNegative weight' >>=
            \weighty ->
              weightCheck (Cow nammy agey weighty)

-- TODO Write the evaulation process of mkSphericalCow'' "Bess" 5 499
-- TODO Write the evaulation process of mkSphericalCow'' "" 5 499


main = hspec $ do

  it "Returns Nothing when bess is 500 lbs or more" $ do
    weightCheck (Cow { name = "bess", age = 10, weight = 500}) `shouldBe` Nothing

  it "Returns bess when bess is under 500 lbs" $ do
    weightCheck (Cow { name = "bess", age = 10, weight = 300}) `shouldBe` Just (Cow { name = "bess", age = 10, weight = 300})


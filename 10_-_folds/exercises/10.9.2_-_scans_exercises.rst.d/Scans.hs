#!/usr/bin/env stack
-- stack script --resolver lts-18.26 --package hspec
module Scans where
import Test.Hspec


fibs = 1 : scanl (+) 1 fibs

twentyFibs = take 20 fibs

fibsUnderOneHundred = takeWhile (<100) fibs

fact n = last (scanl (*) 1 [1..n])


main = hspec $ do
  describe "fibs" $ do
    it "first ten fibs" $ do
      take 10 fibs `shouldBe` [1,1,2,3,5,8,13,21,34,55]
    it "first twenty fibs" $ do
      twentyFibs `shouldBe`
        [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
    it "fibsUnderOneHundred" $ do
      takeWhile (<100) fibs `shouldBe` [1,1,2,3,5,8,13,21,34,55,89]
    it "fact" $ do
      fact 3 `shouldBe` 6
      fact 9 `shouldBe` product [1..9]


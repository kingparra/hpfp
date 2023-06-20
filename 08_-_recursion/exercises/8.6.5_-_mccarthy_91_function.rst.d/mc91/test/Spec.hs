module Main where
import Test.Hspec
import Lib

main = hspec $ do
  describe "mc91" $ do
    it "should work for the example input" $ do
      map mc91 [95..110] `shouldBe` [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
    it "should return 91 where n > 1 && n <= 100"$ do
      map mc91 [1..100] `shouldBe` take 100 (cycle [91])

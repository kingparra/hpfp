import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "1" $ do
    it "a) tensDigit" $ do
      tensDigit' 180 `shouldBe` 8
      tensDigit' 180 `shouldBe` tensDigit 180
    it "c) hunsD" $ do
      hunsD 1800 `shouldBe` 8

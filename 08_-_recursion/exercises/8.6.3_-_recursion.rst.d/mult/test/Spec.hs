import Test.Hspec
import Lib (mult)

main = hspec $ do
  describe "mult" $ do
    it "5 5 -> 25" $ do
      mult 5 5 `shouldBe` 25
    it "(-7) 2 -> (-14)" $ do
      mult (-7) 2 `shouldBe` (-14)
    it "10 (-2) -> (-20)" $ do
      mult 10 (-2) `shouldBe` (-20)
    it "(-10) (-2) -> 20" $ do
      mult (-10) (-2) `shouldBe` 20
    it "8 (-2) -> (-16)" $ do
      mult 8 (-2) `shouldBe` (-16)
    it "8 0 -> 0" $ do
      mult 8 0 `shouldBe` 0
    it "0 8 -> 0" $ do
      mult 0 8 `shouldBe` 0

import Test.Hspec
import Lib (mult)

main = hspec $ do
  describe "mult" $ do
    it "mult 8 3 returns 24" $ do
      mult 8 3 `shouldBe` 8 * 3
    it "mult (-3) 0 returns 0" $ do
      mult (-3) 0 `shouldBe` 0
    it "mult 8 (-3) returns (-24)" $ do
      mult 8 (-3) `shouldBe` (-24)
    it "mult 8 (-2) return (-16)" $ do
      mult 8 (-2) `shouldBe` (-16)

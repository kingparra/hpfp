import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "dodgy" $ do
    it "1. dodgy 1 0 => 1" $ do
      dodgy 1 0 `shouldBe` 1
    it "2. dodgy 1 1 => 11" $ do
      dodgy 1 1 `shouldBe` 11
    it "3. dodgy 2 2 => 22" $ do
      dodgy 2 2 `shouldBe` 22
    it "4. dodgy 1 2 => 21" $ do
      dodgy 1 2 `shouldBe` 21
    it "5. dodgy 2 1 => 13" $ do
      dodgy 2 1 `shouldBe` 12
  describe "oneIsOne" $ do
    it "6. oneIsOne 1 => 11" $ do
      oneIsOne 1 `shouldBe` 11
    it "7. oneIsOne 2 => 21" $ do
      oneIsOne 2 `shouldBe` 21
    it "10. oneIsOne 3 => 31" $ do
      oneIsOne 3 `shouldBe` 31
  describe "oneIsTwo" $ do
    it "8. oneIsTwo 1 => 21" $ do
      oneIsTwo 1 `shouldBe` 21
    it "9. onwIsTwo 2 => 22" $ do
      oneIsTwo 2 `shouldBe` 22
    it "11. oneIsTwo 3 => 23" $ do
      oneIsTwo 3 `shouldBe` 23

import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "mult" $ do
    context "associativity" $ do
      it "x `mult` y always equals y `mult` x" $ do
        property (\x y ->
          (x :: Int) `mult` (y :: Int) == y `mult` x)
    context "negative numbers" $ do
      it "(-1) `mult` 0 returns 0" $ do
        (-1) `mult` 0 `shouldBe` 0
      it "(-7) 2 -> (-14)" $ do
        mult (-7) 2 `shouldBe` (-14)
      it "10 (-2) -> (-20)" $ do
        mult 10 (-2) `shouldBe` (-20)
      it "(-10) (-2) -> 20" $ do
        mult (-10) (-2) `shouldBe` 20
    context "positive numbers" $ do
      it "5 5 -> 25" $ do
        mult 5 5 `shouldBe` 25
    context "multiplication by zero" $ do
      it "8 0 -> 0" $ do
        mult 8 0 `shouldBe` 0
      it "0 8 -> 0" $ do
        mult 0 8 `shouldBe` 0

import Test.Hspec
import Lib

main = hspec $ do
  describe "natToInteger" $ do
    it "Zero ==> 0" $ do
      natToInteger Zero `shouldBe` 0
    it "(Succ Zero) ==> 1" $ do
      natToInteger (Succ Zero) `shouldBe` 1
    it "(Succ (Succ Zero)) ==> 2" $ do
      natToInteger (Succ (Succ Zero)) `shouldBe` 2
  describe "integerToNat" $ do
    it "0 ==> Just 0" $ do
      integerToNat 0 `shouldBe` Just Zero
    it "1 ==> Just (Succ Zero)" $ do
      integerToNat 1 `shouldBe` Just (Succ Zero)
    it "2 ==> Just (Succ (Succ Zero))" $ do
      integerToNat 2 `shouldBe` Just (Succ (Succ Zero))

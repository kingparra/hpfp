import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
  describe "eftBool" $ do
    it "True False ==> []" $ do
      eftBool True False `shouldBe` []
      eftBool True False `shouldBe` enumFromTo True False
    it "True True ==> [True]" $ do
      eftBool True True `shouldBe` [True]
      eftBool True True `shouldBe` enumFromTo True True
    it "False True ==> [False,True]" $ do
      eftBool False True `shouldBe` [False,True]
      eftBool False True `shouldBe` enumFromTo False True
    it "False False ==> [False]" $ do
      eftBool False False `shouldBe` [False]
      eftBool False False `shouldBe` enumFromTo False False
  describe "eftOrd" $ do
    it "LT LT ==> [LT]" $ do
      eftOrd LT LT `shouldBe` [LT]
      eftOrd LT LT `shouldBe` enumFromTo LT LT
    it "LT EQ ==> [LT,EQ]" $ do
      eftOrd LT EQ `shouldBe` [LT,EQ]
      eftOrd LT EQ `shouldBe` enumFromTo LT EQ
    it "LT GT ==> [LT,EQ,GT]" $ do
      eftOrd LT GT `shouldBe` [LT,EQ,GT]
      eftOrd LT GT `shouldBe` enumFromTo LT GT
  describe "eft" $ do
    it "eft will return the same result as enuFromTo" $
      property (\x y -> enumFromTo x y == eft x (y :: Int))

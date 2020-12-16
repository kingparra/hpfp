import Funcs
import Type
import Test.Hspec
import Control.Exception (evaluate)

main = hspec $ do
  describe "isCar" $ do
    it "will return true for cars and false for planes" $ do
      map isCar [ myCar, urCar, clownCar , doge ] `shouldBe` [True,True,True,False]
  describe "isPlane" $ do
    it "will return true for planes and false for cars" $ do
      map isPlane [ myCar, urCar, clownCar , doge ] `shouldBe` [False,False,False,True]
  describe "getManu" $ do
    it "returns the manu" $ do
      getManu (Car Mini (Price 8000)) `shouldBe` Mini
    it "throws an error for planes (since they dont have manus)" $ do
      evaluate (getManu (Plane TakeYourChancesUnited)) `shouldThrow` errorCall "this function only works on cars"
  describe "getManu'" $ do
    it "(Plane TakeYourChancesUnited) -> Nothing" $ do
      getManu' (Plane TakeYourChancesUnited) `shouldBe` Nothing

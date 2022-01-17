import Test.Hspec
import Lib

main = hspec $ do

  describe "shift" $ do
    it "zero" $ do
      shift 0 'a' `shouldBe` 'a'
    it "positive" $ do
      shift 1 'a' `shouldBe` 'b'
    it "positive from 'z'" $ do
      shift 3 'z' `shouldBe` 'c'
    it "negative" $ do
      shift (-5) 'a' `shouldBe` 'v'
    it "non alpha character" $ do
      shift 8 '.' `shouldBe` '.'
    it "uppercase character" $ do
      shift 3 'A' `shouldBe` 'D'
    it "w ==> x" $ do
      shift 1 'w' `shouldBe` 'x'
    it "sentence" $ do
      map (shift 1) "Attack at dawn." `shouldBe` "Buubdl bu ebxo."
      map (shift 2) "Attack at dawn." `shouldBe` "Cvvcem cv fcyp."

  describe "unshift" $ do
    it "1 a ==> z" $ do
      unshift 1 'a' `shouldBe` 'z'
    it "sentence" $ do
      map (unshift 9) "Attack at dawn." `shouldBe` "Rkkrtb rk urne."
      map (unshift 1) "Buubdl bu ebxo." `shouldBe` "Attack at dawn."
      map (unshift 2) "Cvvcem cv fcyp." `shouldBe` "Attack at dawn." 

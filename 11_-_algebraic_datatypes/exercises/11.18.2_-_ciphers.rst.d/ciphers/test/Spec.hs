import Test.Hspec
import Lib

main = hspec $ do
  describe "caesar" $ do
    it "can handle uppercase" $ do
      caesar 2 "middle-Outz" `shouldBe` "okffng-Qwvb"
    it "can shift 3 right" $ do
      caesar 3 "abcdefghijklmnopqrstuvwxyz" `shouldBe` "defghijklmnopqrstuvwxyzabc"
    it "can shift 1 right" $ do
      caesar 1 "hello" `shouldBe` "ifmmp"
    it "can shift 27 right, wrapping around the alphabet" $ do
      caesar 27 "hello" `shouldBe` "ifmmp"
  describe "unCaesar" $ do
    it "can unshift by 2" $ do
      unCaesar 2 "okffng-Qwvb" `shouldBe` "middle-Outz"
  describe "vigenere" $ do
    context "when provided with input that contains whatespace,\n\
            \spaces should be unaltered, and case shold match the\n\
            \plaintext" $ do
      it "\"ALLY\" \"MEET AT DAWN\" ==> \"MPPR AE OYWY\"" $ do
        vigenere "ALLY" "MEET AT DAWN" `shouldBe` "MPPR AE OYWY"
      it "\"ally\" \"The quick brown fox jumps over 13 lazy dogs.\" \
          \ ==> \"Tsp outni bczun qzv jfxns zgcr 13 wlxy ozes.\"" $
        vigenere "ally" "The quick brown fox jumps over 13 lazy dogs."
        `shouldBe` "Tsp outni bczun qzv jfxns zgcr 13 wlxy ozes."

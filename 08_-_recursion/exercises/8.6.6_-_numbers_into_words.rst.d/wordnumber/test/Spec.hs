import Test.Hspec (hspec, describe, context, it, shouldBe)
import Lib (digitToWord, digits, wordNumber)

main = hspec $ do
  describe "digitToWord" $ do
    -- wordNumber works only on Int, so we don't have to worry about
    -- decimals.
    it "converts single digits to words without surrounding dashes" $ do
      digitToWord 0 `shouldBe` "zero"
      digitToWord 1 `shouldBe` "one"
      digitToWord 2 `shouldBe` "two"
      digitToWord 3 `shouldBe` "three"
      digitToWord 4 `shouldBe` "four"
      digitToWord 5 `shouldBe` "five"
      digitToWord 6 `shouldBe` "six"
      digitToWord 7 `shouldBe` "seven"
      digitToWord 8 `shouldBe` "eight"
      digitToWord 9 `shouldBe` "nine"
  describe "wordNumber" $ do
    it "intersperses dashes for multi-digit outputs" $ do
      wordNumber 123 `shouldBe` "one-two-three"
      wordNumber 88 `shouldBe` "eight-eight"
      wordNumber 123456 `shouldBe` "one-two-three-two-four-five-four-six"
    it "prefixes 'negative' when the input is a negative number" $ do
      wordNumber (-98) `shouldBe` "negative-nine-eight"
      -- Since (-0) resolves to 0 before function application, we don't
      -- need a special case for it.

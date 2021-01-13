import Test.Hspec
import Lib

main = hspec $ do
  describe "Question 1" $ do
    context "notThe" $ do
      it "\"the\" ==> Nothing" $ do
        notThe "the" `shouldBe` Nothing
      it "\"blahtheblah\" ==> Just \"blahtheblah\"" $ do
        notThe "blahtheblah" `shouldBe` Just "blahtheblah"
      it "\"woot\" ==> Just \"woot\"" $ do
        notThe "woot" `shouldBe` Just "woot"
    context "replaceThe" $ do
      it "\"the cow loves us\" ==> \"a cow loves us\"" $ do
        replaceThe "the cow loves us" `shouldBe` "a cow loves us"
      it "\"\" ==> \"\"" $ do
        replaceThe "" `shouldBe` ""
  describe "Question 2" $ do
    context "countTheBeforeVowel" $ do
      it "\"the cow\" ==> 0" $ do
        countTheBeforeVowel "the cow" `shouldBe` 0
      it "\"the evil cow\" ==> 1" $ do
        countTheBeforeVowel "the evil cow" `shouldBe` 1
      it "\"the evil the other cow\" ==> 1" $ do
        countTheBeforeVowel "the evil the other cow" `shouldBe` 2
  describe "Question 3" $ do
    context "countVowels" $ do
      it "\"the cow\" ==> 2" $ do
        countVowels "the cow" `shouldBe` 2
      it "\"Mikolajczak\" ==> 4" $ do
        countVowels "Mikolajczak" `shouldBe` 4

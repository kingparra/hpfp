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

import Test.Hspec
import Lib

main = hspec $ do

  describe "Question 1" $ do
    it "appedCatty \"woohoo!\" ==> \"woops mrow woohoo!\"" $ do
      appedCatty "woohoo!" `shouldBe` "woops mrow woohoo!"

  describe "Question 2" $ do
    it "frappe \"1\" ==> \"1 mrow haha\"" $ do
      frappe "1" `shouldBe` "1 mrow haha"

  describe "Question 3" $ do
    it "frappe (appedCatty \"2\") ==> \"woops mrow 2 mrow haha\"" $ do
      frappe (appedCatty "2") `shouldBe` "woops mrow 2 mrow haha"

  describe "Question 4" $ do
    it "appedCatty (frappe \"blue\") ==> \"woops mrow blue mrow haha\"" $ do
      appedCatty (frappe "blue") `shouldBe` "woops mrow blue mrow haha"

  describe "Question 5" $ do
    it "cattyConny (frappe \"pink\") (cattyConny \"green\" (appedCatty \"blue\")) \
       \ ==> \
       \\"pink mrow haha mrow green mrow woops mrow blue\"" $ do
      cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
        `shouldBe` "pink mrow haha mrow green mrow woops mrow blue"

  describe "Question 6" $ do
    it "cattyConny (flippy \"Pugs\" \"are\") \"awesome\"\
       \ ==> \
       \\"are mrow Pugs mrow awesome\"" $ do
      cattyConny (flippy "Pugs" "are") "awesome" `shouldBe` "are mrow Pugs mrow awesome"

import Test.Hspec
import Lib

main = hspec $ do
  describe "Question 1" $ do
    it "tuples" $ do
      tuples `shouldBe` [  (1,1),  (1,8),  (1,27),  (1,64),  (1,125)
                        ,  (4,1),  (4,8),  (4,27),  (4,64),  (4,125)
                        ,  (9,1),  (9,8),  (9,27),  (9,64),  (9,125)
                        , (16,1), (16,8), (16,27), (16,64), (16,125)
                        , (25,1), (25,8), (25,27), (25,64), (25,125)]
  describe "Question 2" $ do
    it "tuplesLT50" $ do
      tuplesLT50 `shouldBe` [  (1,1),  (1,8),  (1,27)
                            ,  (4,1),  (4,8),  (4,27)
                            ,  (9,1),  (9,8),  (9,27)
                            , (16,1), (16,8), (16,27)
                            , (25,1), (25,8), (25,27)]
  describe "Question 3" $ do
    it "tuplesLT50Count" $ do
      tuplesLT50Count `shouldBe` 15

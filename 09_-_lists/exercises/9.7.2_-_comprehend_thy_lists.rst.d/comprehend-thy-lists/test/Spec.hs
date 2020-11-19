import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Question 1" $ do
    it "List comprehension shoule equal [4,16,36,64,100]" $ do
      one `shouldBe` [4,16,36,64,100]
  describe "Question 2" $ do
    it "This comprehension is huge" $ do
      two `shouldBe` [(1,64),(1,81),(1,100)
                     ,(4,64),(4,81),(4,100)
                     ,(9,64),(9,81),(9,100)
                     ,(16,64),(16,81),(16,100)
                     ,(25,64),(25,81),(25,100)
                     ,(36,64),(36,81),(36,100)
                     ,(49,64),(49,81),(49,100)]
  describe "Question 3" $ do
    it "List comprehension should equal [(1,64),(1,81),(1,100),(4,64),(4,81)]" $ do
      three `shouldBe` [(1,64),(1,81),(1,100),(4,64),(4,81)]

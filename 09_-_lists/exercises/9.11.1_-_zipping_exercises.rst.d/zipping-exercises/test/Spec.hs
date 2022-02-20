import Test.Hspec
import Lib
  ( zip'
  , zipWith'
  , zip'Refactored
  )

main = hspec $ do

  describe "Question 1" $ do
    it "zip' will work for sample inputs" $ do
      zip' [1,2] ['a','b']  `shouldBe` [(1,'a'),(2,'b')]
      zip' [1] ['a','b']    `shouldBe` [(1,'a')]
      zip' [1,2] ['a']      `shouldBe` [(1,'a')]
      zip' [1,2,3,4] "abdc" `shouldBe` [(1,'a'),(2,'b'),(3,'d'),(4,'c')]

  describe "Question 2" $ do
    it "zipWith'" $ do
      zipWith' (+) [1,2,3] [4,5,6] `shouldBe` [5,7,9]

  describe "Question 3" $ do
    it "zip'Refactored" $ do
      zip'Refactored [1,2] ['a','b']  `shouldBe` [(1,'a'),(2,'b')]
      zip'Refactored [1] ['a','b']    `shouldBe` [(1,'a')]
      zip'Refactored [1,2] ['a']      `shouldBe` [(1,'a')]
      zip'Refactored [1,2,3,4] "abdc" `shouldBe` [(1,'a'),(2,'b'),(3,'d'),(4,'c')]

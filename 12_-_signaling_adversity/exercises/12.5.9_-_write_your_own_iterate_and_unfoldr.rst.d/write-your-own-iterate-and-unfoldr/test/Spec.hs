import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do

  describe "Question 1" $ do
    context "myIterate" $ do
      it "(+1) 0 ==> [0..]" $ do
        take 10 (myIterate (+1) 0)
          `shouldBe` take 10 [(0 :: Int),1..]


  describe "Question 2" $ do
    context "myUnfoldr" $ do
      it "(\\b -> Just (b,b+1)) 0 ==> [0..]" $ do
        take 10 (myUnfoldr (\b -> Just (b, b+1)) 0)
          `shouldBe` [0..9]
      it "can generate [10,9..1]" $ do
        myUnfoldr (\b -> if   b == 0
                         then Nothing
                         else Just (b, b-1)
                  ) 10 `shouldBe` [10,9..1]

  describe "Question 3" $ do
    context "betterIterate" $ do
      it "(+1) 0 ==> [0..]" $ do
        take 10 (betterIterate (+1) 0)
          `shouldBe` take 10 [(0 :: Int),1..]

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do

  describe "iterate''" $ do
      it "(+1) 0 ==> [0..]" $ do
        take 10 (iterate'' (+1) 0) `shouldBe` [0..9]

  describe "unfoldr''" $ do
      it "(\\b -> Just (b,b+1)) 0 ==> [0..]" $ do
        take 10 (unfoldr'' (\b -> Just (b, b+1)) 0) `shouldBe` [0..9]
      it "can generate [10,9..1]" $ do
         unfoldr'' (\b -> if   b == 0 
                          then Nothing
                          else Just (b, b-1)
                          ) 10 `shouldBe` [10,9..1]

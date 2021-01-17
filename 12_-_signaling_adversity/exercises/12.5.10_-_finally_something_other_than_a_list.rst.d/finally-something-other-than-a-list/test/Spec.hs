import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do

  describe "Question 1" $ do
    context "" $ do
      it "" $ do
        True `shouldBe` True

  describe "Question 2" $ do

    context "treeBuild" $ do
      it "0 ==> Leaf" $ do
        treeBuild 0 `shouldBe` Leaf

      it "1 ==> Node Leaf 0 Leaf" $ do
        treeBuild 1 `shouldBe` Node Leaf 0 Leaf

      it "2 ==> Node (Node Leaf 1 Leaf)\n\
         \           0\n\
         \           (Node Leaf 1 Leaf)" $ do
        treeBuild 2 `shouldBe`
          Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)

      it "3 ==> Node (Node (Node Leaf 2 Leaf) \n\
         \                 1                  \n\
         \                 (Node Leaf 2 Leaf))\n\
         \           0                        \n\
         \           (Node (Node Leaf 2 Leaf) \n\
         \                 1                  \n\
         \                 (Node Leaf 2 Leaf))" $ do
          treeBuild 3 `shouldBe`
            Node (Node (Node Leaf 2 Leaf)
                       1
                       (Node Leaf 2 Leaf))
                 0
                 (Node (Node Leaf 2 Leaf)
                       1
                       (Node Leaf 2 Leaf))

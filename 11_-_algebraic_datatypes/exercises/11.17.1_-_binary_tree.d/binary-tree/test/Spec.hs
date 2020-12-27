import Lib
import Test.Hspec

main = hspec $ do
  describe "mapTree" $ do
    it "Will apply (+1) to all nodes" $ do
      mapTree (+1) testTree `shouldBe` mapExpected
      where
        testTree :: BinaryTree Int
        testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
        mapExpected :: BinaryTree Int
        mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do

  describe "Q1" $ do
    it "avgGradeUnconditional" $ do
      avgGradeUnconditional 90 `shouldBe` 'F'
      avgGradeUnconditional 75 `shouldBe` 'F'
      avgGradeUnconditional 60 `shouldBe` 'F'

  describe "Q2" $ do
    it "avgGradeReordered" $ do
      avgGradeReordered 90 `shouldBe` 'C'
      avgGradeReordered 75 `shouldBe` 'C'
      avgGradeReordered 60 `shouldBe` 'D'
    it "avgGradeOrderIndependent" $ do
      avgGradeOrderIndependent 90 `shouldBe` 'A'
      avgGradeOrderIndependent 80 `shouldBe` 'B'
      avgGradeOrderIndependent 70 `shouldBe` 'C'
      avgGradeOrderIndependent 59 `shouldBe` 'D'
      avgGradeOrderIndependent 58 `shouldBe` 'F'

  describe "Q3" $ do
    it "pal" $ do
      pal "racecar" `shouldBe` True

  describe "Q6" $ do
    it "numbers" $ do
      numbers (-3) `shouldBe` (-1)
      numbers 0 `shouldBe` 0
      numbers 9 `shouldBe` 1

  -- Questions 7 and 8 don't have tests since it requires
  -- checking the type signature in ghci.

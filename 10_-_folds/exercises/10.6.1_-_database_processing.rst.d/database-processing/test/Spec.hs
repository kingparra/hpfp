import Test.Hspec
import Data.Time
import Control.Exception (evaluate)
import Lib
  ( DatabaseItem(..)
  , theDatabase
  , filterDbDate
  , filterDbNumber
  , mostRecent
  , sumDb
  , avgDb
  )

main = hspec $ do
  describe "filterDbDate"$ do
    it "returns args to DbDate constructor" $ do
      filterDbDate theDatabase `shouldBe`
        [ (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
        , (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
        ]
  describe "filterDbNumber"$ do
    it "returns args to DbNumber constructor" $ do
      filterDbNumber theDatabase `shouldBe`
        ([9001] :: [Integer])
  describe "mostRecent"$ do
    it "returns most recent date in theDatabase" $ do
      mostRecent theDatabase `shouldBe`
        (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
    it "throws an error for empty lists" $ do
      evaluate (mostRecent []) `shouldThrow` anyException
  describe "sumDb" $ do
    it "sums DbNumbers in theDatabase" $ do
      sumDb theDatabase `shouldBe` 9001
  describe "avgDb" $ do
    it "averages DbNumbers in theDatabase" $ do
      avgDb theDatabase `shouldBe` 9001
    it "averages DbNumbers in [1..20]" $ do
      avgDb [DbNumber 20, DbNumber 30, DbNumber 90] 
        `shouldBe` 46.666666666666664

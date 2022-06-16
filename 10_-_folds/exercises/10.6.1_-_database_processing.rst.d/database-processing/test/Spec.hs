import Test.Hspec
import Data.Time
import Lib
  ( DatabaseItem(..)
  , theDatabase
  , filterDbDate
  , filterDbNumber
  , mostRecent
  -- , sumDb
  -- , avgDb
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


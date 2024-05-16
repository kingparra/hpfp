#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package QuickCheck --package hspec --package checkers
{-# LANGUAGE OverloadedStrings #-}
module BadMonad where

import Test.QuickCheck (Arbitrary(..)) 
import Test.QuickCheck.Checkers (quickBatch, EqProp(..), eq)
import Test.QuickCheck.Classes (functor, applicative, monad)

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a)

instance Applicative CountMe where
  pure = (CountMe 0)
  (CountMe n f) <*> (CountMe n' a) = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  (CountMe n a) >>= f = let (CountMe _ b) = f a in (CountMe (n + 1) b)

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

#!/usr/bin/env stack
-- stack --resolver lts-20.18 script --package QuickCheck --package hspec --package checkers
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Monad (join, liftM2)

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where 
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where 
  _ <*> _ = NopeDotJpg
  pure  _ = NopeDotJpg

instance Monad Nope where 
  _ >>= _ = NopeDotJpg

-- ·∾ :type (>>=) @(Nope)
-- (>>=) @(Nope) :: Nope a -> (a -> Nope b) -> Nope b
-- ·∾ NopeDotJpg >>= (\x -> NopeDotJpg)
-- NopeDotJpg
-- ·∾ NopeDotJpg >>= id
-- NopeDotJpg
-- ·∾ NopeDotJpg >>= undefined 
-- NopeDotJpg

-- Check this out!
-- ·∾ newtype Container a = Container [a] 
--    deriving (Functor, Applicative, Monad)


-- 2
data BahEither b a = PLeft a | PRight b deriving (Eq, Ord, Show)

instance Functor (BahEither b) where
  fmap f (PLeft x) = (PLeft $ f x)
  fmap f (PRight x) = (PRight x)

instance Applicative (BahEither b) where
  pure x = PLeft x
  (PLeft f)  <*> (PLeft x)  = PLeft (f x)
  _          <*> (PRight x) = PRight x
  (PRight x) <*> _          = PRight x
-- ·∾ (+) <$> (PLeft 10) <*> (PLeft 100)
-- PLeft 110

instance Monad (BahEither b) where
  return = pure
  (PLeft x) >>= f = (f x)
-- ·∾ (PLeft 4) >>= (\x -> PLeft (x+4))
-- PLeft 8



-- 3
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity x) <*> (Identity y) = Identity (x y)

instance Monad Identity where
  return = pure
  (Identity x) >>= f = (f x)
-- ·∾ (Identity 7 :: Identity Int) >>= (\x -> return (x*100 :: Int))
-- Identity 700


-- 4. Write a Monad for List
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr f z (Cons x xs) = x `f` (foldr f z xs)
  foldr f z Nil = z

lappend :: List a -> List a -> List a
lappend Nil ys = ys
lappend (Cons x xs) ys = Cons x (xs `lappend` ys)

lconcat :: List (List a) -> List a
lconcat xxs = foldr (\x y -> x `lappend` y) Nil xxs
-- ·∾ nested = Cons 
--               (Cons 1 (Cons 2 (Cons 3 Nil))) 
--               (Cons (Cons 4 (Cons 5 (Cons 6 Nil))) Nil)
-- ·∾ lconcat nested
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons mf mfs) <*> mxs = (fmap mf mxs) `lappend` (mfs <*> mxs)

instance Monad List where
  return = pure
  (>>=) mx mf = lconcat . fmap mf $ mx
-- ·∾ pure 3 >>= (\x -> Cons (x+3) Nil)
-- Cons 6 Nil

-- Question: How do I convert a List with arbitrary nesting level to a [] of
-- the same depth? Maybe with GADTs or some other extension to the language?


-- A. Write the following functions using the methods provided by Monad and Functor.
-- Using stuff like identity and composition is fine, but it has to type check with the types provided.
--
-- | j [[1,2],[],[3]] `shouldBe` [1,2,3]
-- | j (Just (Just 1)) `shouldBe` (Just 1)
-- | j (Just Nothing) `shouldBe` Nothing
-- | j Nothing `shouldBe` Nothing
j :: Monad m => m (m a) -> m a
j = join -- (\xs -> xs >>= id)


-- B.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap -- (\f xs -> xs >>= return . f)


-- C.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2


-- D.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)


-- F.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = flip traverse


-- G.
-- hint: reuse meh
flipType :: Monad m => [m a] -> m [a]
flipType = (`meh` id)


-- Monad laws
-- right identity
-- m >>= reutrn ≡ m
-- left identity
-- return x >>= f ≡ f x
-- associativity
-- (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
--

main = do
  let trigger = [(1,2,3)] :: [(Int,Int,Int)]
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


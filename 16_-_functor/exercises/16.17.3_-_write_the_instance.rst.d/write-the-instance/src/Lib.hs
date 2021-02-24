module Lib where


-- Question 1
data Quant a b = Finance | Desk a | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f Finance   = Finance
  fmap f (Desk a)  = Finance -- Does this destroy structure?
  fmap f (Bloor b) = Bloor (f b)

-- Question 2
data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K (f _)


-- -- Question 3
-- newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- newtype K' a b = K' a

-- -- This should remind you of an instance
-- -- you've written before.
-- instance Functor (Flip K' a) where
--  fmap = undefined


-- -- Question 4
-- data EvilGoateeConst a b = GoatyConst b
-- -- You thought you'd escaped the goats
-- -- by now didn't you? Nope.


-- -- Question 5
-- data LiftItOut f a = LiftItOut (f a)


-- -- Question 6
-- data Parrapa f g a = DaWrappa (f a) (g a)


-- -- Question 7
-- data IgnoreOne f g a b =
--  IgnoringSomething (f a) (g b)

-- -- Question 8
-- data Notorious g o a t =
--  Notorious (g o) (g a) (g t)


-- -- Question 9
-- data List a = Nil | Cons a (List a)


-- -- Question 10
-- data GoatLord a 
--   = NoGoat
--   | OneGoat a
--   | MoreGoats (GoatLord a)
--               (GoatLord a)
--               (GoatLord a)
-- -- A VERITABLE HYDRA OF GOATS


-- -- Question 11
-- data TalkToMe a 
--   = Halt 
--   | Print String a 
--   | Read (String -> a)

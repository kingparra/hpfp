-- 16.3 There's a whole lot of fmap goin' round
-- page 630, after the second paragraph on that page


{-# ANN Identity ("Hlint: Ignore: Use newtype instead of data") #-}
{-# ANN Identity ("Hlint: Ignore: Use newtype instead of data") #-}


data Identity a =
  Identity a deriving (Eq, Show)


data Constant a b =
  Constant b deriving (Eq, Show)


type E e = Either e
type C e = Constant e
type I   = Identity


{- "We can see how the type of ⍘fmap⍘ 
   specializes to different types here"
  
   fmap :: Functor f
        =>  (a -> b)   ->      f a    ->      f b

   fmap ::  (a -> b)   ->     [] a    ->     [] b
  
   fmap ::  (a -> b)   ->  Maybe a    ->  Maybe b
  
   fmap ::  (a -> b)   ->    E e a    ->    E e b
  
   fmap ::  (a -> b)   ->   (e,) a    ->   (e,) b
  
   fmap ::  (a -> b)   ->      I a    ->      I b
  
   fmap ::  (a -> b)   ->    C e a    ->    C e b
 -}

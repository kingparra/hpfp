Monad Cheat Sheet
*****************
Monad is its algebraic laws.
Nothing more, nothing less.


Definition
----------
::

  class Applicative m => Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b
    return ::   a -> m a
    {-# MINIMAL (>>=) #-}


Laws
----
::

           return a >>= f   ≡   f a              --  left identity
             m >>= return   ≡   m                -- right identity
  m >>= (\x -> f x >>= g)   ≡   (m >>= f) >>= g  --  associativity

Define ``fmap`` in terms of monad
---------------------------------
::

  fmap (+1) [1..3]                ≡  [(1+1),(1+2),(1+3)]
  [1..3] >>= return . (+1)        ≡  [(1+1),(1+2),(1+3)]
  [1..3] >>= (\x -> return (x+1)  ≡  [(1+1),(1+2),(1+3)]



The ``join`` function is a generalized ``concat``
-------------------------------------------------
::

  import Control.Monad (join)
  join   :: Monad m    => m (m a) ->  m a
  concat :: Foldable t => t [] a  -> [] a


Implementing ``(>>=)`` using ``fmap`` and ``join``
--------------------------------------------------
::

  import Control.Monad (join)

  m x y = join (fmap y x) 

  {- Comparing similar type signatures
  -- Generalizing -----------------------------------------------------
                 (>>=) :: Monad    m =>   m a  ->  (a ->  m b)  ->  m b
        flip concatMap :: Foldable t =>   t a  ->  (a -> [] b)  ->  [b]
                     m :: Monad    m =>   m a  ->  (a ->  m b)  ->  m b
  -- Specializing ------------------------------------------------------
  flip (concatMap @[]) ::                  [a] ->  (a -> [b])   -> [b]
            (>>=) @[]  ::                  [a] ->  (a -> [b])   -> [b]
  -}


Monadic functions and their do-block equivalents
------------------------------------------------
.. TODO Rewrite these equivalences so they're easier to read.

::

   join bss   ≡  do { bs <- bss; bs }
   as >>= bs  ≡  do { a <- as; bs a }

   fmap fab ma  =  do { a <- ma; return (fab a) }
               --  ma >>= (return . fab)
   pure a       =  do { return a }
               --  return a
   mfab <*> ma  =  do { fab <- mfab ; a <- ma ; return (fab a) }
               --  mfab >>= (\ fab -> ma >>= (return . fab)) 
               --  mfab `ap` ma

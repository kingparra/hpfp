*************************
 Chapter 21: Traversable
*************************


21.1 Traversable
----------------
In this chapter, we will:

* explain the ``Traversable`` typeclass and its canonical functions;
* explore examples of ``Traversable`` in practical use;
* tidy up some code using this typeclass;
* and, of course, write some ``Traversable`` instances.

.. Is traversable the same thing as an iterator?

.. An iterator interface presents operations to:
   * access the current element,
   * move to the next element,
   * and to test for completion.


21.2 The Traversable type class definition
------------------------------------------
First let's look at the typeclass definition
of ``Traversable``::

  class (Functor t, Foldable t) => Traversable (t :: * -> *) where

    traverse   :: Applicative f   =>   (a -> f b)  ->  t a  ->  f (t b)
    mapM       :: Monad m         =>   (a -> m b)  ->  t a  ->  m (t b)
    sequenceA  :: Applicative f   =>      t (f a)  ->  f (t a)
    sequence   :: Monad m         =>      t (m a)  ->  m (t a)
    {-# MINIMAL traverse | sequenceA #-}


  instance Traversable []
  instance Traversable Maybe
  instance Traversable (Either a)
  instance Traversable ((,) a)



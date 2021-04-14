*************************
 Chapter 21: Traversable
*************************


21.1 Traversable
----------------
.. The book says:
..
..  "Traversable allows you to transform
..   elements inside the structure like a
..   functor, producing applicative effects
..   along the way, and lift those potentially
..   multiple instances of applicative
..   structure outside of the traveresavle
..   structure."
..
.. Which sounds important, but is completely
.. impenetrable to me. What the hell does that
.. mean!?

.. Is traversable the same thing as an iterator?
.. An iterator interface presents operations to:
.. * access the current element,
.. * move to the next element,
.. * and to test for completion.

.. justsomeguy:
..   Can someone give me a dumbed-down, possibly
..   inaccurate summary of what Traversable is,
..   just to give me a sense of it? Is it an
..   iterator? A thingy that flips inner
..   structure with outer structure? Something
..   that helps me climb trees?
.. koz_ justsomeguy:
..   Traversable is 'effectful fmap'.
.. koz_ :t fmap
.. lambdabot Functor f => (a -> b) -> f a -> f b
.. koz_ :t traverse
.. lambdabot (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
.. koz_ s/is/allows/
.. * | justsomeguy slowly mulls over what an applicative effect is.
.. monochrom :
..  IO is an example.
.. monochrom :
..   If you have ["hello", "hi", "aloha"], and if
..   you want for each string there you putStrLn
..   then getLine, and you want the result list
..   to be the 3 lines you get from the 3 getLines:
.. monochrom :
..   traverse (\s -> putStrLn s >> getLine) ["hello", "hi", "aloha"]
.. monochrom :
..   THE END
.. justsomeguy :
..   Hrm, that's pretty similar to forM, which
..   I've been using for a while now without
..   understanding.
.. monochrom :
..   Just different argument orders.


In this chapter, we will:

* explain the ``Traversable`` typeclass and its canonical functions;
* explore examples of ``Traversable`` in practical use;
* tidy up some code using this typeclass;
* and, of course, write some ``Traversable`` instances.


21.2 The Traversable type class definition
------------------------------------------
First let's look at the typeclass definition
of ``Traversable``, which I've somewhat
rearranged::

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

As you can see, a minimal class definition
requires only one of either ``traverse`` or
``sequenceA``.

These two methods can be written in terms of
each other, and have the following default
definitions::

  traverse f = sequenceA . fmap f
  sequenceA = traverse id

How should we think about the intended usage
of these methods?

``traverse`` almost seems like an "effectful
``fmap``", and resembles the ``forM`` function
that I've been using for a while, now. Here's
an example someone gave me from IRC::


  ·∾ traverse (\s -> putStrLn s >> getLine) ["hello", "hi", "aloha"]
  hello
  Chris
  hi
  Bonjour!
  aloha
  Aloha!
  ["Chris","Bonjour!","Aloha!"]

  ·∾ mapM (\s -> putStrLn s >> getLine ) ["hello", "hi", "aloha"]
  hello
  Chris
  hi
  Bounjour!
  aloha
  Alohahaha
  ["Chris","Bounjour!","Alohahaha"]


21.3 sequenceA
--------------
You can see from the type signature that
``sequenceA`` is flipping two contexts or
structures::

  ·∾ :type sequenceA
  sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)

It doesn't by itself allow you to apply any
function to the ``a`` value inside the
structure; it only flips the layers of
structure around.

An experiment in GHCi::

  ·∾ :doc sequenceA
   Evaluate each action in the structure from left to right, and
   collect the results. For a version that ignores the results
   see 'Data.Foldable.sequenceA_'.

  ·∾ sequenceA [ (putStrLn "one" >> getLine)
               , (putStrLn "two" >> getLine)
               , (putStrLn "three" >> getLine)]
  one
  1
  two
  2
  three
  3
  ["1","2","3"]

Some examples from this section.

.. include:: figures/21.3/ghci_examples.txt
   :code:

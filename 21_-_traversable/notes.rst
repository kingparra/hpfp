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

.. http://dev.stephendiehl.com/hask/#foldable-traversable

.. traversable allows us to walk a data
   structure left-to-right within an
   applicative context.


.. A traversable structure has a finite number
   of elements that can be accessed in a
   linear order.

In this chapter, we will:

* explain the ``Traversable`` typeclass and its canonical functions;
* explore examples of ``Traversable`` in practical use;
* tidy up some code using this typeclass;
* and, of course, write some ``Traversable`` instances.


21.2 The Traversable type class definition
------------------------------------------
It speaks for itself, really.

.. include:: figures/21.2/typeclass_definition.txt
   :code:


21.3 sequenceA
--------------
Alright, what is ``sequenceA``, really?

You can see from the type signature that
``sequenceA`` is flipping two contexts or
structures::

  ·∾ :type sequenceA
  sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)

``sequenceA`` doesn't allow you to apply any
function to the ``a`` value inside the
structure by itself; it only flips the layers
of structure around.

An experiment in GHCi::

  ·∾ :doc sequenceA
   Evaluate each action in the structure from
   left to right, and collect the results. For
   a version that ignores the results see
   'Data.Foldable.sequenceA_'.

  ·∾ :{
   ⋮ sequenceA [ putStrLn "one" >> getLine
   ⋮           , putStrLn "two" >> getLine
   ⋮           , putStrLn "three" >> getLine ]
   ⋮ :}
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

.. topic:: What the ``fmap``?

   In the example above, this line confused me::

     ·∾ (fmap . fmap) sum Just [1,2,3]
     Just 6

   Here's what I expected it to desugar to::

     ·∾ fmap sum (fmap Just [1,2,3])
     [1,2,3]

   But as you can see, the result is ``[1,2,3]``
   rather than ``Just 6``.

   After some brute force experimentation, I
   came up with this, which produces the same
   result::

     ·∾ fmap sum (Just [1,2,3])
     Just 6

   What happened to the inner ``fmap``?


21.4 traverse
-------------
Let's look at the type of ``traverse``::

  ·∾ :type traverse
  traverse :: (Traversable t, Applicative f)
           => (a -> f b) -> t a -> f (t b)

You might notice a similarity between that and
the types of ``fmap`` and ``(flip bind)``:

.. include:: figures/21.4/fmap_bind_traverse.hs
   :code:

``traverse`` **is similar to** ``fmap``\ **,
except that it also allows you to run
applicative effects while you're rebuilding
the data structure, which also changes the
result type.**

In this usage, applicative is almost the same
as monad, except that effects cannot depend on
previous results.

Here's an example someone gave me from IRC
that illustrates this::

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

Since I've been using ``mapM`` for a while I
found this a useful comparison.

You've already seen that traverse is ``fmap``
composed with ``sequenceA``. Here are a few
examples of that:

.. include:: figures/21.4/traverse_in_more_steps.txt
   :code:

The general idea is that anytime you're using
``sequenceA . fmap f`` you can use ``traverse``
to achieve the same result in one step.



21.5 So, what's Traversable for?
--------------------------------
In a literal sense, **anytime you need to flip
two type constructors around, or map something
and then flip them around, that's probably**
``Traversable``\ **.**

::

  ·∾ f = undefined :: a -> Maybe b

  ·∾ xs = undefined :: [a]

  ·∾ :type map f xs
  map f xs :: [Maybe b]

  ·∾ :type sequenceA $ map f xs
  sequenceA $ map f xs :: Maybe [a]

It's usually better to use ``traverse``
whenever we see a ``sequence`` or
``sequenceA`` combined with a ``map``
or ``fmap``.

::

  ·∾ :type sequenceA $ map f xs
  sequenceA $ map f xs :: Maybe [a]

  ·∾ :type traverse f xs
  traverse f xs :: Maybe [b]


21.6 Morse code revisited
-------------------------
The expression ``(sequence .) . fmap`` is so
hard to read that the authors included a
translation in the book. Just don't write code
like that in the first place, it's awful.


21.7 Axing tedious code
-----------------------
.. include:: figures/21.7/Decoder.hs
   :code:


21.8 Do all the things
----------------------
Here, have some more code! Get ready for a
really long compile time if you try to run
this.

.. include:: figures/21.8/HttpStuff.hs
   :code:

Strength for understanding
^^^^^^^^^^^^^^^^^^^^^^^^^^
We can write ``Functor`` and ``Foldable`` in
terms of ``Traversable``.

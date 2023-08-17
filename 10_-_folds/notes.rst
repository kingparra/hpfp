***************************
 Chapter 10: Folding Lists
***************************

.. The epigraph at the beginning of the
   chapter is from the lecture `Crafstman or Scientist?
   <https://www.cs.utexas.edu/users/EWD/transcriptions/EWD04xx/EWD480.html>`_.

**"How do we reduce the demands made on our
quantitatively limited powers of reasoning?"**
~ Edsger W. Dijkstra


10.1 Folds
----------
.. topic::  Can I come up with a better name than "fold"?

   Recombine.     com- "with, together" + bini "two by two"

   Reconstruct.   con- "together" + struere "to pile up"

   Reassemble.   "bring or put together again, gather anew"

   Rejoin.       "unite again, unite after separation"

   Respine.

   Meld.         "to blend together, merge, unite"

.. topic:: What the heck is a catamorphism?

   Here is an explanation of what catamorhpism means, from
   `John Chandler Burnhams notes
   <https://www.johnchandlerburnham.com/projects/hpfp/10/>`_.

   Okay, so here’s the thing about the term “catamorphism”:

   “Kata” in Greek means “down”. The opposite of “kata” is
   “ana” which means “up”.

   So we have “catamorphisms” and “anamorphisms”. Remember
   that “morph” means “form”, so a “catamorphism” is a
   “down-form thing” and an “anamorphism” is an “up-form
   thing”.

   But what the heck do “up” and “down” have to do with
   “forms”?  There’s a metaphor that recurs (so to speak)
   again and again in functional programming between height
   and complexity: Things that have more structure are
   upwards and things that have less structure are downwards.
   It’s like a tall building: the more structure you have
   the higher you go.

   So an ``Integer`` is pretty simple, and is downwards of
   ``[Integer]`` or ``Maybe Integer`` or ``Map String
   Integer``.

   Functions that go “upwards” in this complexity-space,
   like from ``Integer -> [Integer]`` are, roughly speaking,
   anamorphisms.  Functions that go “downwards” are
   catamorphisms.

In this chapter, we will:

* Explain what folds are and how they work.
* Detail the evaluation process of folds.
* Walk through writing folding functions.
* Introduce scans, functions that are related to folds.


10.2 Bringing you into the fold
-------------------------------
::

  foldr :: (a -> b -> b) -> b -> [a] -> b


10.4 Fold right
---------------
::

  foldr f z l =
    case l of
      [] -> z
      (x:xs) -> f x (foldr f z xs)

  foldr (+) 0 [1,2,3] ≡ ((+) 1 ((+) 2 ((+) 3 0)))

10.4.1 How ``foldr`` evaluates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
One initially non-obvious aspect of folding is that it happens in two stages, traversal and folding.
Traversal is the stage in which the fold recurses over the spine.
Folding refers to the evaluation or reduction of the folding function applied to the values.

For ``foldr``, traversal of the spine is conditional on ``f``, and can stop early.
Except for the first cons cell, which is forced because of the pattern match ``(x:xs) -> f x (foldr f z xs)``.

Our input function, ``f``, has the type ``(a -> b -> b)``, where ``b`` represents the "rest of the fold".

.. include:: figures/10.04.01/full_recursive_call.txt
   :code:


10.5 Fold left
--------------
::

  foldl f z l =
    case l of
      [] -> z
      (x:xs) -> foldl f (f z x) xs

  foldl (+) 0 [1,2,3] ≡ ((+) ((+) ((+) 0 1) 2) 3)

``foldl`` forces traversal of the entire spine before
it returns a value.

Our input function ``f`` has the type signature
``(b -> a -> b)``, where our first argument ``b``
represents the "rest of the fold".

.. include:: exercises/10.5.2_-_understanding_folds.rst

.. include:: exercises/10.6.1_-_database_processing.rst


10.7 Folding and evaluation
---------------------------
The relationship between ``foldr`` and ``foldl`` is such that
``foldr f z xs`` ≡ ``foldl (flip f) z (reverse xs)``, but only
for finite lists.


10.9 Scans
----------
::

  scanr f z l =
    case l of
      [] -> [z]
      (x:xs) -> foldr f z l : scan f z xs

  scanl f z l =
    z : (case l of
          [] -> z
          (x:xs) -> scanl f (f z x) xs)

.. include:: exercises/10.9.2_-_scans_exercises.rst


10.10 Chapter exercises
-----------------------

.. include:: exercises/10.10.1_-_warm-up_and_review.rst

.. include:: exercises/10.10.2_-_rewriting_functions_using_folds.rst

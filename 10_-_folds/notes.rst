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


10.4 Fold right
---------------

10.4.1 How ``foldr`` evaluates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. paragraphs 1-14

.. include:: figures/10.04.01/full_recursive_call.txt
   :code:

.. paragraphs 15-x

Each recursive call to ``foldr`` is conditional on ``f``.

However, the first cons cell is always evaluated because of
the pattern match ``(x:xs) -> f x (foldr f z xs)``.

If ``f`` hits a base case, then it can stop evaluation
instead of consuming the next call to ``foldr`` as its
second argument.


10.5 Fold left
--------------
.. include:: exercises/10.5.2_-_understanding_folds.rst


10.6 How to write fold functions
--------------------------------

.. include:: exercises/10.6.1_-_database_processing.rst


10.7 Folding and evaluation
---------------------------
The relationship between ``foldr`` and ``foldl`` is such that
``foldr f z xs`` ≡ ``foldl (flip f) z (reverse xs)``, but only
for finite lists.


10.9 Scans
----------

.. include:: exercises/10.9.2_-_scans_exercises.rst


10.10 Chapter exercises
-----------------------

.. include:: exercises/10.10.1_-_warm-up_and_review.rst

.. include:: exercises/10.10.2_-_rewriting_functions_using_folds.rst

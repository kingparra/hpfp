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
.. Can I come up with a better name than "fold"?
   Reconstruct.   con- "together" + struere "to pile up".
   Rejoin.       "unite again, unite after separation".
   Reassemble.   "bring or put together again, gather anew,"
   Recombine.     com "with, together" + bini "two by two"
   Respine.

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

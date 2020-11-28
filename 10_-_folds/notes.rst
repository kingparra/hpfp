***************************
 Chapter 10: Folding lists
***************************


10.1 Folds
----------
This chapter is a thorough look at the topic of folding
lists in Haskell we will:

* explain what folds are and how they work;
* detail the evaluation process of folds;
* walk through writing folding functions;
* introduce scans, functions that are related to folds.


10.4 Fold right
---------------
::

  foldr binary_function accumulator input_list -> accumulated_value

  foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

  foldr            :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     =  z
  foldr f z (x:xs) =  f x (foldr f z xs)


10.8 Summary
------------

foldr
^^^^^
1. ``(a -> b -> b)`` ... ``b`` is the rest of the fold.
2. Associates to the right.
3. Works with infinite lists. ``foldr const 0 [1..]``
4. A good default choice.

foldl
^^^^^
1. Self-calls (tail-call) through the list, only beginning to produce vaules
   after reaching the end of the list.
2. Associates to the left.
3. Cannot be used with infinte lists.
4. Nearly useless and should almost always be replaced with ``foldl'``.


10.10 Chapter Exercises
-----------------------
.. include:: exercises/10.10.2_-_rewriting_functions_using_folds.rst

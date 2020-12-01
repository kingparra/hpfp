***************************
 Chapter 10: Folding lists
***************************


10.1 Folds
----------
Folds as a general concept are known as *catamorphisms*.
Cata- "down" or "against", -morph- "form", -ism "pertaining
to". Catamorphisms are a means of deconstructing data.

This chapter is a thorough look at the topic of folding
lists in Haskell we will:

* explain what folds are and how they work;
* detail the evaluation process of folds;
* walk through writing folding functions;
* introduce scans, functions that are related to folds.


10.4 Fold right
---------------
First let's look at the definition of ``foldr`` ::

  --                     binary     initial      output
  --                    function     value  list  value
  --                  vvvvvvvvvvvv     v     v     v
  foldr            :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     =  z
  foldr f z (x:xs) =  f x (foldr f z xs)

  foldr f z l =
    case l of
      []     -> 0
      (x:xs) -> f x (foldr f z xs)

  {-
  foldr (+) 0 [1,2,3] =
  . . .
    case [1,2,3] of
      []     -> 0
      (1:xs) -> (+) 1 (foldr (+) 0 [2,3]) -- <==
  . . .
    case [2,3] of
      []     -> 0
      (2:xs) -> (+) 1 ((+) 2 (foldr (+) 0 [3])) -- <==
  . . .
    case [3] of
      []     -> 0
      (3:xs) -> (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 []))) -- <==
  . . .
    case [] of
      []     -> 0 -- <==            v
      (x:xs) -> (+) 1 ((+) 2 ((+) 3 0))
  . . .
    (+) 1 ((+) 2 ((+) 3 0))
  . . .
    6
  -}


10.4 Fold right
---------------
::

  ·∾ xs = map show [1..5]
  ·∾ y = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs
  ·∾ y
  "(1+(2+(3+(4+(5+0)))))"

One non-obvious aspect of folding is that it happens in two stages, traversal
and folding. All folds recurse over the spine in the same direction; the
difference between left folds and right folds is in the association of the
folding function, which determines what direction the reduction proceeds in.
::

  foldr f z [1,2,3]
  1 `f` (foldr f z [2,3])
  1 `f` (2 `f` (foldr f z [3])
  1 `f` (2 `f` (z `f` (3 `f` (foldr f z []))
  1 `f` (2 `f` (z `f` (3 `f` z))

Because ``foldr`` evaluates with the same associativity that is used to build
the spine if the input function doesn't require it, ``foldr`` can avoid
evaluating not only values, but the spine as well.

::

  -- This should work on infinite lists...
  myAny :: (a -> Bool) -> [a] -> Bool
  myAny f xs =
    foldr (\x b -> f x || b) False xs

  -- foldr itself only requires the first cons cell
  ·∾ xs = [1,2,3] ++ undefined
  ·∾ foldr (\_ _ -> 9001) 0 xs
  9001

  ·∾ foldr const 0 [1,undefined]
  1


10.5 Fold left
--------------
Left folds traverse the spine in the same direction as right
folds, but their reduction process is left associative and
proceeds in the opposite direction as that of ``foldr``.

::

  foldr f z (x:xs) = f x (foldr f z xs)
  --                     ^^^^^^^^^^^^^^
  --     right arg to f is recursively parenthesized

  foldl f z (x:xs) = foldl f (f z x) xs
  --                         ^^^^^^^^
  --     left arg to f is recursively parenthesized

  ·∾ f x y = concat ["(",x,"+",y,")"]
  ·∾ foldl f "0" (map show [1..5])
  "(((((0+1)+2)+3)+4)+5)"

Note that the initial value, ``"0"``, was added to the left this time.

::

  foldl f z [1, 2, 3]
  -- f ~ (flip (:)); z ~ []
  -- (((z `f` 1) `f` 2) `f` 3)

  f = flip (:)
  ((([] `f` 1) `f` 2) `f` 3)
  (([1] `f` 2) `f` 3)
  ([2,1] `f` 3)
  [3,2,1]

10.5.1 Scans
^^^^^^^^^^^^
The relationship between scans and folds are as follows::

  last (scanl f z xs) = foldl f z xs

  head (scanr f z xs) = foldr f z xs

.. include:: exercises/10.5.2_-_understanding_folds.rst

10.5.3 Unconditional spine recursion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Because ``foldl`` must evaluate its whole spine before it starts evaluating
values in each cell, it accumulates unevaluated expressions (thunks) in memory
as it traverses the spine of the list. This is a problem for very large lists.
Use ``foldl'`` instead, which forces evaluation of each thunk as its
encountered, or ``foldr`` if possible.

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

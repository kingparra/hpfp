********************
 Chapter 10 Outline
********************
-- page 347
* Epigram
-- page 348
* 10.1 Folds
  * p1. What folds are used for.
  * p2. Learning objectives.
* 10.2 Bringing you into the fold
  * p1
    * f1. Type signatures of ``foldr`` in GHC versions 7.8 and 7.10.
  * p2
-- page 349
    * f2. The same type signatures, lined up next to each other.
  * p3
    * f3. A ghci session that defines ``listFoldr``, a version of ``foldr`` that has been specialized to the list type.
  * p4
    * f4. Comparing the evaluation of ``map`` and ``foldr``.
  * p5
-- page 350
* 10.3 Recursive patterns
  * p1.
    * f1. GHCi snippet ``sum [1,5,10]``.
  * p2.
    * f2. A implementation of ``sum``.
  * p3.
    * f3. A implementation of ``length``.
  * p4.
    * f4. Implementations of ``product`` and ``concat``.
  * p5.
-- page 351
  * p6.
* 10.4 Fold right
  * p1.
    * f1. A straightforward definition of ``foldr``.
  * p2.
  * 10.4.1 How foldr evaluates
    * p1.
      * f1. An alternate definition of ``foldr`` that does pattern matching with a case statement, in order to make the evaluation process easier to read.
    * p2.
-- page 352
      * f2. ``foldr (+) [1,2,3]``
    * p3.
      * f3.
    * p4.
      * f4.
    * p5.
      * f5.
    * p6.
      * f6.
-- page 353
    * p7.
    * p8
      * f7.
    * p9.
      * f8.
    * p10.
      * f9.
    * p11. "Into:"
      * f10.
    * p12.
-- page 354
      * f11.
    * p13.
      * f12.
    * p14.
    * p15. Trick for visualizing folds.
      * f13.
    * p16.
      * f14. GHCi session showing evaluation of ``y``.
    * p17.
    * p18.
    * p19.
      * f15.
-- page 355
    * p20.
      * f16.
    * p21.
      * f17. ``myAny``
    * p22.
      * f18. ``myAny even [1..]``
    * p23.
      * f19. ``myAny even (repeat 1)``
    * p24.
-- page 356
      * f20.
    * p25.
    * p26.
      * f21.
    * p27.
      * f22.
    * p28.
-- page 357
      * f23.
    * p29.
    * p30.
      * f24.
    * p31.
    * p32.
      * f25.
    * p33.
      * f26.
-- page 358
      * f26 continued.
    * p34.
    * p35.
      * f27.
    * p36. "Now that we've seen how foldr evaluates, w're going to look at foldl before we more on to learning how to write and use folds.
-- page 359
* 10.5 Fold left
  * p1.
  * p2.
    * f1.
  * p3.
    * f2.
  * p4.
  * p5.
-- page 360
  * p5 continued.
    * f3.
  * p6.
    * f4.
  * p7.
..  TODO Continue outlining from here on.
  * 10.5.1 Associativity and folding
  * 10.5.2 Exercises: Understanding folds
    * 1
    * 2
    * 3
    * 4
    * 5
  * 10.5.2 Unconditional spine recursion
* 10.6 How to write fold functions
  * 10.6.1 Exercises: Database processing
    * 1
    * 2
    * 3
    * 4
    * 5
* 10.7 Folding and evaluation
* 10.8 Summary
  * ``foldr``
  * ``foldl``
* 10.9 Scans
  * 10.9.1 Getting the Fibonacci number we want
* 10.10 Chapter Exercises
  * 10.10.1 Warm-up and review
    * 1
      * a
      * b
      * c
    * 2
    * 3
  * 10.10.2 Rewriting functions using folds
    * 1 ``myOr``
    * 2 ``myAny``
    * 3 ``myElem``
    * 4 ``myReverse``
    * 5 ``myMap``
    * 6 ``myFilter``
    * 7 ``squish``
    * 8 ``squishMap``
    * 9 ``squishAgain``
    * 10 ``myMaximumBy``
    * 11 ``myMinimumBy``
* 10.11 Definitions
  * fold
  * catamorphism
  * tail call
  * tail recursion
* 10.12 Follow-up resources
  * Antoni Diller. Introduction to Haskell. Unit 6.
    http://www.cantab.net/users/antoni.diller/haskell/haskell.html
  * Graham Hutton. A tutorial on the universality and expressiveness of fold.
    http://www.cs.nott.ac.uk/~gmh/fold.pdf

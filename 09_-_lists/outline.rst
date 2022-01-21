*******************
 Chapter 9 Outline
*******************

* 9.1 Lists

  * p1. What lists are used for.
  * p2. Topics of the chapter.

* 9.2 The list datatype

  * p1.

    * f1.

  * p2.
  * p3.
  * p4.

    * f2.

  * p5.
  * p6.

* 9.3 Pattern matching on lists

  * p1.

    * f1. ``myHead`` implemented with pattern matching in GHCi.

  * p2.

    * f2. ``myTail``

  * p3.

    * f3. Applying ``myHead`` and ``myTail`` to the empty list in GHCi.

  * p4.

    * f4. A definition of ``myTail`` with a base case for the empty list, ``[]``.

  * p5.

    * f5. ``:info Maybe``

  * p6.

    * f6. Definition of ``safeTail``.

* 9.4 List's syntactic sugar

  * p1.

    * f1.

  * p2.

    * f2.

  * p3.
  * p4.
  * p5.
  * p6.

* 9.5 Using ranges to construct lists

  * p1.

    * f1.

  * p2.

    * f2. Type signatures of the ``enumFrom`` family of functions.

  * p3.
  * p4. The first argument to ``enumFromTo`` must be lower than its second argument, otherwise you'll get an empty list.

    * f3.

  * 9.5.1 Exercise: ``EnumFromTo``

    * p1.

      * f1. Output of ``:info Enum``.

    * p2.

      * f2.

* 9.6 Extracting portions of lists

  * p1.
  
    * f1. The type signatures for ``take``, ``drop``, and ``splitAt``.

  * p2.
  * p3.

    * f2. ``take`` applied to various arguments in GHCi.

  * p4.

    * f3.

  * p5.

    * f4.

  * p6.
  
    * f5.

  * p7.

    * f6. Type signatures of ``takeWhile`` and ``dropWhile``.

  * p8.
  * p9. "Take the elements that are less than 3:"

    * f7.

  * p10.

    * f8.

  * p11.

    * f9.

  * p12.

    * f10.

  * p13. Next we'll look at ``dropWhile``...

    * f11.

    * 9.6.1 Exercises: Thy Fearful Symmetry
      
      * 1.

          * p1.
          * f1.

      * 2.

          * p1.
          * f1. ``PoemLines`` module
          * p2.
          * f2. Expected result of ``putStrLn``
          * p3.
          * f3. ``myLines`` function stub
          * p4.
          * f4. ``shouldEqual``, a list of strings that should be the result of ``myLines sentences``.
          * p5.
          * f5. the ``main`` module

      * 3.

        * p1.

* 9.7 List comprehensions

  * 9.7.1 Adding predicates
  * 9.7.2 Exercises: Comprehend thy lists
  * 9.7.3 List comprehensions with strings
  * 9.7.4 Exercises: Square Cube

* 9.8 Spines and non-strict evaluation
* 9.9 Transforming lists of values
* 9.10 Filtering lists of values
* 9.11 Zipping lists
* 9.12 Chapter exercises
* 9.13 Definitions
* 9.14 Follow-up resources

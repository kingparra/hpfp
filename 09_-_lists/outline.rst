*******************
 Chapter 9 Outline
*******************

* 9.1 Lists

  * p1. Lists may be used to represent a finite collection or infinite sequence of values.

  * p2. Learning objectives.

* 9.2 The list datatype

  * p1. "The list datatype is defined like this:"

    * f1. Definition of the list datatype.

  * p2. The type and data constructors of list.
  * p3. Arity of constructors. (tycon) ``[] a`` unary, ``a : [a]`` binary, (datacon) ``[]`` unary.
  * p4. "In English, one can read this as:"

    * f2. Anatomy of the list datatype declaration.

  * p5. The cons ``(:)`` data constructor is binary, takes a recursively defined argument, and represents a product relationship.
  * p6. Lists in Haskell are similar to singly-linked lists.

* 9.3 Pattern matching on lists

  * p1. We can match on the ``(:)`` and ``[]`` data constructors. Here we match on the first argument of ``(:)``.

    * f1. Definition, type query, and sample use of ``myHead`` in GHCi.

  * p2. We'll match on the second argument of ``(:)`` in ``myTail``.

    * f2. Definition, type query, and sample use of ``myTail`` in GHCi.

  * p3. Both ``myHead`` and ``myTail`` don't handle the case of an empty list, ``[]``.

    * f3. GHCi session showing that exceptions are thrown when ``myHead`` and ``myTail`` are applied to ``[]``.

  * p4.

    * f4. Definition of ``myTail`` as source code, with a base case for ``[]`` added.

  * p5. "With that addition, our function now evaluates like this:"

    * f5. GHCi session showing ``myTail`` applied to a finite list and the empty list.

  * p6. Using ``Maybe``.
  * p7. Let's try an example using ``Maybe`` with ``myTail``.

    * f6. ``:info Maybe``

  * p8. "Rewriting myTail to use Maybe is fairly straightforward:"

    * f7. Definition of ``safeTail``.

  * p9. Description of ``safeTail``. **See if you can rewrite the myHead function using Maybe.**
  * p10. Later the book will cover ``NonEmpty``, which avoids the empty list problem.

* 9.4 List's syntactic sugar

  * p1.

    * f1. GHCi session demonstrating equivalence of ``[1,2,3]++[4]`` and ``(1:2:3:[])++(4:[])``.

  * p2. ``[x,y]`` syntax saves typing.
  * p3. Cons cells and spines.
  * p4. The spine is the connective structure between nested cons cells.

* 9.5 Using ranges to construct lists

  * 9.5.1 Exercise: ``EnumFromTo``

* 9.6 Extracting portions of lists

  * p1.

    * f1. Type signatures of ``take``, ``drop``, and ``splitAt``.

  * p2.
  * p3.

    * f2. Demonstration of ``take`` in GHCi.

  * p4.

    * f3. Adding ``enumFrom`` to the demonstration.

  * p5.

    * f4. Demonstrating ``drop`` on lists created with range syntax.

  * p6.

    * f5. Demonstrating ``splitAt``.

  * p7.

    * f6. Type signatures of ``takeWhile`` and ``dropWhile``.

  * p8.
  * p9.

    * f7. Demonstrating ``takeWhile``

  * p10.

    * f8.

  * p11.

    * f9. Demonstrating ``takeWhile`` with a predicate that is never met.

  * p12. **"In the final example below, why does it only return a single a?"**

    * f10. ``takeWhile (=='a') "abracadabra"``

  * p13. Next we'll look at ``dropWhile``.

    * f11. Examples of ``dropWhlie`` applied to different arguments in GHCi.

  * 9.6.1 Exercises: Thy Fearful Symmetry

    * 1

      * p1.
      * f1.

    * 2

      * p1.
      * f1. The ``PoemLines`` module.
      * p2.
      * f2. The result that ``putStrLn sentences`` should print.
      * p3.
      * f3. Stub for the ``myLines`` function.
      * p4.
      * f4. A list named ``shouldEqual`` that ``myLines sentences`` should produce.
      * p5.
      * f5. A small test.

    * 3

      * p1.

* 9.7 List comprehensions

  * p1.
  * p2.

    * f1. A simple comprehension, ``[ x^2 | x <- [1..10]``, followed by a lot of explanatory text.

  * p3.

    * f2. ``[ x^2 | x <- [1..10]``,

  * 9.7.1 Adding predicates

    * p1.
    * p2.

      * f1.

    * p3.
    * p4.
    * p5.
    * p6.

      * f2.

    * p7.
    * p8.

      * f3. A list comprehension with a predicate, evaluated in GHCi.

    * p9. We can use multiple generators to zip two lists.

      * f4. Two list comprehensions that performs a cross product on two lists into a list of pairs,
        evaluated in GHCi.

    * p10.
    * p11.

      * f5. ``mySqr``, a comprehension of square numbers from n..10, evaluated in GHCi.

    * p12. We can use that list as a generator for another list comprehension.

      * f6.

  * 9.7.2 Exercises: Comprehend thy lists

    * p1.

      * f1.

  * 9.7.3 List comprehensions with strings

    * p1.

      * f1.

    * p2.

      * f2.

    * p3.

      * f3. An acronym generator.

    * p4.
    * p5. "All right, so we have our acro function with which we can generate acronyms from any string:"

      * f4. ``acro`` applied to different arguments in GHCi.

    * p6. **"Given the above, what do you think this function would do:"**

      * f5.

  * 9.7.4 Exercises: Square Cube

    * p1.

      * f1.

    * 1
    * 2
    * 3

* 9.8 Spines and non-strict evaluation

  * p1. The structure that connects values together in
    composite datatypes is known as the spine.

    * f1. An ASCII art representation of the list
      ``[1,2]`` as a tree of data constructors and
      their term-level arguments.

  * p2.

  * p3. Evaluation proceeds down the spine (left to
    right), but construction proceeds up the spine
    (right to left).

  * p4.

    * f2. ASCII art pointing out the spine of a list.

  * p5.

  * 9.8.1 Using GHCi's :sprint command

    * p1.
    * p2.
    * p3.
    * p4.

      * f1.

    * p5.
    * p6. "Next, we'll take one value..."

      * f2.

    * p7.
    * p8.

      * f3.

    * p9.
    * p10.

      * f4.

    * p11.

      * f5.

    * p12.

  * 9.8.2 Spines are evaluated independently of values

    -- page 320

    * p1. All expressions are evaluated to WHNF by default.
    * p2. WHNF vs NF.
    * p3. Examples of expressions, and whether they are WHNF or NF.

      * f1. ``(1, 2)``

    -- page 321

    * p4.

      * f2. ``(1, 1+1)``

    * p5.

      * f3. ``\x -> x*10``

    * p6.

      * f4. ``"Papu" ++ "chon"``

    * p7.

      * f5. ``(1, "Papu" ++ "chon")``

    * p8.

      * f6. Showing a fully evaluated list in GHCi.

    -- p9 is split between pages 321 and 322

    * p9.

    -- page 322

      * f7. A demonstration of WHNF evaluation in GHCi.

    * p10.
    * p11.

      * f8. The spine of a list that isn't spine strict
        and is awaiting something to force the evaluation.
        (The first cons cells, no arguments evaluated.)

    * p12.
    * p13.

    -- page 323

    * p14.

      * f9. Tree representation of the spine of an
        unevaluated list with two elements.

    * p15.

      * f10. GHCi ``x = [1,undefined]; length x``
        returns ``2``.

    * p16.

      * f11. Source code for a ``length`` function.

    * p17.

    -- page 324

    * p18.

      * f12. A complicated tree representation showing
        forced cons constructors, with unevaluated
        arguments.

    * p19.

      * f13. Demonstration of applying ``length`` to a
        list with ``undefined`` in the spine.

    * p20. Printing the list fails, but it gets as far
      as printing the first ``[1***``.
    * p21. It's possible to write functions that will
      force both the spine and the values.
    * p22. We'll write our own sum function for the
      sake of demonstration:

      * f14. Source code for ``mySum``.

    * p23.

    -- page 325

      * f15. The evaluation steps of ``mySum [1..5]``

    * p24.

  * 9.8.3 Exercises: Bottom madness

    * 9.8.3.1 Will it blow up?
    * 9.8.3.2 Intermission: Is it in normal form?

* 9.9 Transforming lists of values

  * 9.9.1 Exercises: More bottoms

* 9.10 Filtering lists of values

  * 9.10.1 Exercises: Filtering

* 9.11 Zipping lists

  * 9.11.1 Zipping exercises

* 9.12 Chapter exercises

  * 9.12.1 Data.Char
  * 9.12.2 Ciphers
  * 9.12.3 Writing your own standard functions

* 9.13 Definitions

  * Product type
  * Sum type
  * Cons
  * Cons cell
  * Spine

* 9.14 Follow-up resources

  * Data.List documentation for the base library.  http://hackage.haskell.org/package/base/docs/Data-List.html
  * Haskell Wiki. Ninety-Nine Haskell problems.  https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

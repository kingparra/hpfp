*******************
 Chapter 9 Outline
*******************

-- Chapter 9 spans from pages 299 to 346, for a total of 47 pages.

* 9.1 Lists

  * p1. What lists are used to represent.
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

  * p8. "Rewriting ``myTail`` to use ``Maybe`` is fairly straightforward:"

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

  * p1. Let's first look at ``take``, ``drop``, and ``splitAt``.

    * f1. Type signatures of ``take``, ``drop``, and ``splitAt``.

  * p2.
  * p3.

    * f2. ``take`` applied to finite lists

  * p4.

    * f3. ``take`` applied to an infinite list

  * p5.

    * f4. ``drop`` applied to finite lists

  * p6.

    * f5. ``splitAt`` applied to finite lists

  * p7. Next we'll look at ``takeWhile`` and ``dropWhile``

    * f6. Type signatures of ``takeWhile`` and ``dropWhile``.

  * p8. These functions will take or drop elements that meet the condition and then
    stop when it meets the first element that doesn't satisfy the predicate function.

  * p9.

    * f7. Take while the elements are less than 3

  * p10.

    * f8. Take while the elements are less than 8

  * p11.

    * f9. ``takeWhile`` produces an empty list when the first element does not
      meet the predicate.

  * p12. **"In the final example below, why does it only return a single a?"**

    * f10. ``takeWhile (=='a') "abracadabra"``

    -- (The second element evaluates to ``False`` in our predicate function, so
    ``takeWhile`` stops taking elements after the first element.)

  * p13. Now we'll look at ``dropWhile``.

    * f11. Examples of ``dropWhlie`` applied to finite lists

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

        * f5. A small test implemented as a ``main`` function.

    * 3

      * p1.

* 9.7 List comprehensions

  * p1. List comprehensions are a language construct inspired by set builder
    notation in mathematics. You use them to create new lists from an existing
    generator list, which may be filtered along the way by a guard.

  * p2.

    * f1. A simple comprehension, ``[ x^2 | x <- [1..10]]``, followed by a lot of explanatory text.

  * p3.

    * f2. ``[ x^2 | x <- [1..10]``

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

  * p1. The structure that connects elements together in composite datatypes is known as the spine.

    * f1. An ASCII art representation of the list ``[1,2]`` as a tree of data constructors and their term-level arguments.

  * p2.

  * p3. Evaluation proceeds down the spine (left to right), but construction proceeds up the spine (right to left).

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

      * f8. The spine of a list that isn't spine strict and is awaiting something to force the evaluation.  (The first cons cells, no arguments evaluated.)

    * p12.
    * p13.

    -- page 323

    * p14.

      * f9. Tree representation of the spine of an unevaluated list with two elements.
    * p15.

      * f10. GHCi ``x = [1,undefined]; length x`` returns ``2``.

    * p16.

      * f11. Source code for a ``length`` function.

    * p17.

    -- page 324

    * p18.

      * f12. A complicated tree representation showing forced cons constructors, with unevaluated arguments.

    * p19.

      * f13. Demonstration of applying ``length`` to a list with ``undefined`` in the spine.

    * p20. Printing the list fails, but it gets as far as printing the first ``[1***``.
    * p21. It's possible to write functions that will force both the spine and the values.
    * p22. We'll write our own sum function for the sake of demonstration:

      * f14. Source code for ``mySum``.

    * p23.

    -- page 325

      * f15. The evaluation steps of ``mySum [1..5]``

    * p24.

  * 9.8.3 Exercises: Bottom madness

    * 9.8.3.1 Will it blow up?
    * 9.8.3.2 Intermission: Is it in normal form?

* 9.9 Transforming lists of values

  -- page 326

  * p1. HOFs are use more often than primitive recursion to transform data.
  * p2. The ``map`` function applies a function to every element of a list. ``fmap`` does the same, but for any type that implements ``Foldable``.

  -- page 327

    * f1. Examples of using ``map`` and ``fmap`` in GHCi.

  * p3. The types of ``map`` and ``fmap`` respectively are:

    * f2.

  * p4.

    * f3.

  * p5.

    * f4. ``:t map (+1)``

  * p6.
  * p7.

    * f5.

  -- page 328

  * p8.

    * f6. ``:t fmap (+1)``

  * p9.

    * f7. The definition of ``map`` from the ``base`` package, heavily annotated. Spans pages 328 and 329.

  -- page 329

  * p10. "How do we write out what ``map f`` does?"

    * f8. ``map (+1) [1, 2, 3]``

  * p11.

    * f9. ``map (+1) (1 : (2 : (3 : [])))``

  * p12.

    * f10. Shows one step of the evaluation process for ``map (+1) [1,2,3]``.

  * p13.

    * f11.

  * p14.

    * f12.

  -- page 330

  * p15.

    * f13.

  * p16. "Finishing the reduction of the expression:"

    * f14.

  * p17. "Using the syntactic sugar of list, here's an approximation of what map is doing for us:"

    * f15.

  * p18.

    * f16.

  * p19.

    * f17.

  -- page 331

  * p20. Map is not applied to every element at once. Each element is mapped if and when its evaluation is forced.

    * f18.

  * p21.

    * f19.

  * p22.
  * p23.

  -- page 332

  * p24.

    * f20.

  * p25.

    * f21.

  * p26.

    * f22. ``Prelude> map (\x -> if x == 3 then (-x) else (x)) [1..10]``

  * p27.

-- page 332

  * 9.9.1 Exercises: More bottoms

    * 1
    * 2
    * 3

-- page 333

    * 4
    * 5

      * a
      * b
      * c

    * 6

* 9.10 Filtering lists of values

  * p1. We showed a few examples of ``filter`` earlier.

    * f1. ``filter even [1..10]``

  * p2. Filter has the following definition:

-- page 334

    * f2. The definition of filter.

  * p3. Filter takes a predicate function and a list and returns a list containing only the elements that satisfy the predicate.
  * p4. Examples of ``filter`` that we've already seen.

    * f3. ``filter (== 'a') "abracadabra"``

  * p5. The following examples does the same thing as filter even, but with a lambda as input.

    * f4.

  * p6. We covered list comprehensions as a way of filtering lists, as well. Compare the following:

    * f5. Example of filter vs a guarded list comprehension.

  * p7.
  * p8. **We recommend at this point that you try writing some filter functions of your own to get comfortable with the pattern.**

-- page 335

  * 9.10.1 Exercises: Filtering

    * 1
    * 2
    * 3

* 9.11 Zipping lists

  * p1.
  * p2.

    * f1.

  * p3.

    * f2.

  * p4.

    * f3.

  * p5.

    * f4.

  * p6. "We can use unzip to recover the lists as they were before they were zipped:"

    * f5.

  * p7. "Be aware that information can be list in this process, because zip must stop on the
    shortest list:"

    * f6.

  * p8. "We can also use zipWith to apply a function to the values of two lists in parallel:"

    * f7. zipWith

  * p9. "A brief demonstration of how ``zipWith`` works:"

    * f8.

  * 9.11.1 Zipping exercises

* 9.12 Chapter exercises

  * p1.

  * 9.12.1 Data.Char

    * p1.

      * 1
      * 2
      * 3
      * 4
      * 5
      * 6

  * 9.12.2 Ciphers

    * p1. Save these exercises in a module named ``Cipher``, since we'll be coming back to them later.
    * p2. The Caesar cipher shift by :math:`n` numbers forward in the alphabet.
    * p3. Write a basic Caesar cipher.
    * p4. Try to implement the cipher before googling a solution.
    * p5. The first lines should look like:

      * f1. A module header and import statement for ``Cipher``.

    * p6. ``ord`` and ``chr`` may be useful to you.

      * f2. A GHCi session where the types of ``chr`` and ``ord`` are queried.

    * p7. You want your shift to wrap back around the alphabet.
    * p8. Also, write an ``unCeasar`` cipher.

  * 9.12.3 Writing your own standard functions

    * p1.
    * p2.

    -- page 341

      * f1. myAnd

    * p3. "And now the fun begins:"

      * 1
      * 2
      * 3
      * 4
      * 5
      * 6
      * 7
      * 8
      * 9
      * 10

* 9.13 Definitions

  * Product type
  * Sum type
  * Cons
  * Cons cell
  * Spine

* 9.14 Follow-up resources

  * Data.List documentation for the base library.  http://hackage.haskell.org/package/base/docs/Data-List.html
  * Haskell Wiki. Ninety-Nine Haskell problems.  https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

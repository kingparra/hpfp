*******************
 Chapter 9 Summary
*******************
Short summaries of each section, as well as the overall chapter.

.. Sequence vs series
   https://byjus.com/maths/difference-between-sequence-and-series/
   https://www.purplemath.com/modules/series.htm
.. Series: the sum of the sequence where the order of elements does not matter.
.. Sequence: an enumerated collection of objects in which repetitions are allowed and order matters. Called "progression" in British English.

.. sequence (n)

   1 : a hymn in irregular meter between the gradual and Gospel in masses for special occassions (such as Easter)
   2 : a continuous or connected series: such as

      c : a succession of repetitions of a melodic phrase or harmoic pattern each in a new position
      d : a set of elements ordered so that they can be labeled with the positive integers
   3
    a : order of succession
    b : an arrangement of th tenses of successive verbs in a sentence designed to express a coherent relationship especially between main and subordinate parts

   late 14c., "hymn sung after the Hallelujah and before the Gospel,"
   from Old French sequence "answering verses" (13c.),
   from Medieval Latin sequentia "a following, a succession,"
   from Latin sequentem (nominative sequens), present participle of sequi "to follow"
   (from PIE root *sekw- (1) "to follow").
   In Church use, a partial loan-translation of Greek akolouthia, from akolouthos "following."
   General sense of "succession," also "a sequence at cards," appeared 1570s.

.. etymology of progresssion (n)

   late 14c., progressioun, "action of moving from one condition to another,"
   from Old French progression
   and directly from Latin progressionem (nominative progressio) "a going forward, advancement, growth, increase,"
   noun of action from past-participle stem of progredi "go forward,"
   from pro "forward" (see pro-) + gradi "to step, walk,"
   from gradus "a step"
   (from PIE root *ghredh- "to walk, go").
   The musical sense of "an advance from one note to another" or later one chord to another is by c. 1600. Related: Progressional.

.. succession
   succeed
   To come next after another in office or position or in possession of an estate.
   To follow after another in order.
   To pass to a person by inheritance.
   To follow in sequence and especially immediately.
   To come after as heir or sucessor.

   sub- "next to, after" + cedere "go, move"

   "to continue, endure"

.. Intersting words: plantigrade, perigrinate, perambulate, propinquity, mundivagant, celerity.
.. What is a top-level declaration? Something that starts with "type", "data", "newtype", "instance", "foreign", or name-bindings like variable names or pattern bindings.
.. user-defined: type, newtype, data. overloading: class, instance, default. nested declarations:
   value bindings, type signatures, fixity declarations.
.. !!!  "We found that existing code, existing expertise, and open source libraries are the dominant drivers of adoption."


Learning objectives:

* Explain the list datatype
* Explain how to pattern match on lists
* Explain the special syntax that Haskell provides to make working with lists more concise
* Learn about the representation of lists values as a series of linked data constructors
* See what this representation means for lists evaluation
* Do a lot of exercises


9.2 The list datatype
---------------------
This section explains the components of the list datatype, a little bit about how many values can be
constructed by each data constructor, and also alludes to the performance characteristics of the
type by comparing it to singly-linked lists.

Paragraph one and the first example show the data declaration of the list type.

Paragraph two clarifies how to read the cons data constructor. (:) is an infix data constructor. The
right argument to (:) is a type argument (*list-of-a* ``[a]``) rather than the nill data constructor
(*end-of-list* ``[]``), even though they're written similarly.

Paragraph three discusses the relationship between the arguments belonging to each data constructor
and the number of term-level values that it can construct. The number of values that the list type
can construct is a sum of all possible values that each data constructor can construct.

.. Why is this subject being discussed?
   Why is it useful to know that list is a sum type?
   What does knowing that cons is a product of its arguments, one of which is recursive, let us do?
   Why do we care how many values the list type can construct?
   How does this paragraph advance the reader towards acheiving the learning objectives?
   Which learning objective is this even relevant to?

.. The Algebra (and Calculus!) of Algebraic Data Types

   +-------------------------------------+---------------+----------------------------------+
   |            declaration              |  cardinality  |  explanation                     |
   +=====================================+===============+==================================+
   | data void                           |    0          |  Needs LANGUAGE EmptyDataDecls   |
   +-------------------------------------+---------------+----------------------------------+
   | data Unit = Unit                    |    1          |  The type with just one term     |
   +-------------------------------------+---------------+----------------------------------+
   | data Bool = True | False            |    1 + 1      |                                  |
   +-------------------------------------+---------------+----------------------------------+
   | data Maybe a = Just a | Nothing     |    a + 1      |  Read "+" as "Or"                |
   +-------------------------------------+---------------+----------------------------------+
   | data Either a b = Left a | Right b  |    a + b      |                                  |
   +-------------------------------------+---------------+----------------------------------+
   | data (a,b) = (a,b)                  |    a * b      |  Read "*" as "And"               |
   +-------------------------------------+---------------+----------------------------------+
   | a -> b                              |    b^a        |                                  |
   +-------------------------------------+---------------+----------------------------------+

   ``Either a b`` has as many  as ``a`` and ``b`` combined.
   ``(a,b)`` has an nhabitant for each combination of ``a``'s and ``b``'s, ``a * b``.
   ``a -> b`` has b^a.

Paragraph four and its accompanying figure explains how to read each part of the data declaration.

Paragraph five explains that cons' right argument is a self-reference to the list type, so
the number of term-level values cons can construct is infinite.

Paragraph six mentions that lists have similar performance to singly-linked lists (which means cons
cells may be spread out in different memory locations on the heap) and also that they're evaluated
lazily and can be infinite.

.. Knowing that lists in Haskell are similar to singly-linked lists lets us make some assumptions
   about the memory layout for terms of this type, which operations are fast, and which are slow.
   Particularly - opeartions that work on the head of the list are faster, since direct access is
   impossible. Each access to an element e_{n} has to traverse {n} number of cons cells to reach
   that element. Every Single Time. As for memory layout, each cons cell can be located at a
   different place on the heap, so they may be spread out to the point that cache locality is
   reduced compared to dataypes that allocate their elements contiguously in memory, such as Arrays.


9.8 Spines and non-strict evaluation
------------------------------------
..
  Spine : structure of a collection that ties the elements together.
  In the case of lists, it is the nested cons data constructors, ending with nil.
  It is possible to evaluate only the spine of a list without evaluating the elements within it.
  It is also possible to evaluate only part of the spine of a list and not the rest of it.
  Haskells evaluation is non-strict, so the list isn't constructed until its consumed.
  Nothing in the list is evaluated until its is forced.

9.8.2 Spines are evaluated independently of values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

..
  Normal form
    Completely evaluated. All subexpressions have been reduced to their simplest form.

  Weak head normal form
    Using outermost reduction, the expression has been reduced to **at least** the point where a
    data constructor or lambda awaiting and argument encoloses all other expressions. The
    subexpressions within the lambda or data constructor does not need to be evaluated. WHNF is a
    subset of NF, so anything in normal form is also by definition in WHNF.

  Haskell for imperative programmers #31 - weak head normal form
  * https://www.youtube.com/watch?v=QBQ9_9R7o8I&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=31

  Haskell for imperative programmers #32 - DeepSeq

..
  What is lazy evaluation?
  How does it work in Haskell?

  Reductions

  * Applying function definitions and applying atomic operations that we can do.

  The end result is a value that we cannot reduce any further.

  Church-Rosser Theorem

  * Applies to any typed variant of the lambda calculus. Including Haskell!
  * If there is some reduction strategy that terminates, the outermost reduction strategy *always* terminates!

  Evaluation strategies

  * Innermost reduction: Arguments are fully evaluated before the function is applied to it.
  * Outermost reduction: Arguments are substituted into the function definition first, and then only evaluated as needed later on.

  Lazy evaluation = Outermost reductions + Sharing

  Sharing
  * If there are multiple occurences of the same expression, the result of one of them is shared
    with or memoized for all other occurences of that expression. Haskell only calculuates it once.

  Graph reduction
  * Expressions are turned into a graph before evaluation to facilitate sharing.

  Normal Form: An expression is in NF if and only if it is fully evaluted. Fully evaluated -> Fully executed.

  WHNF:

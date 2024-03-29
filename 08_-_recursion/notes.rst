**********************
 Chapter 8: Recursion
**********************




  .. image:: figures/nickieatmirror.jpg
     :align: center
     :alt: A picture of a girl looking into a mirror, opposite another mirror, forming an
           infinite cooridor of reflections. This is a visual example of recursion. The nested
           instances of the reflection are the recursive case. The fade to dark green is the
           base case.




  "Recursion (rĭ-kûr’-zhən) noun. If you still don’t get it, see recursion."




  **A recursive structure is one that contains a smaller instance of itself.**




  A process or structure is recursive when it contains progressively smaller nested instances of itself.




  "An object is said to be recursive if it partially consists or is defined in terms of itself."

  ~ Niklaus Wirth. Algorithms + Data Structures = Programs. ISBN-13: 978-0130224187.




  "An entity or concept is said to be recursive when simpler or smaller
  self-similar instances form part of its constituents."

  ~ Manuel Rubio-Sánchez. Introduction to Recursive Programming. ISBN-13: 978-1-4987-3528-5.




  "The recursive problem solving process can be described loosely as follows:

    * If the given instance of the problem can be solved directly, do so.
    * Otherwise, reduce it to one or more smaller instances of the same problem."

  ~ Jeff Erickson. `Algorithms <https://jeffe.cs.illinois.edu/teaching/algorithms/>`_. ISBN-13: 978-1792644832.




  "The power of recursion evidently lies in the possibility of defining an infinite
  set of objects by a finite statement. In the same manner, an infinite number of
  computations can be described by a finite recursive program, even if this program
  contains no explicit repetitions."

  ~ Niklaus Wirth. `Algorithms + Data Structures = Programs <https://archive.org/
  details/algorithmsdatast00wirt/mode/2up>`_. ISBN-13: 978-0130224187.




  "Recursion is the root of computation since it trades description for time."

  ~ Alan Perlis. `Epigrams on Programming <http://pu.inf.uni-tuebingen.de/users/klaeren/epigrams.html>`_.




8.1 Recursion
-------------
.. topic:: Unifing control-flow and data-flow

   Function calls are the only mechanism for control flow in Haskell. They're also the only mechanism
   for changing values. This effectively means that the control flow and data flow of a program are
   the same; both control and data go from function call to function call. Rather than having a
   separate graph for the control flow and data flow of a program, there is one graph that represents
   both. This makes execution of a Haskell program less like updating registers of a machine that
   advances a program counter, and more like evaluating a system of equations in algebra.

.. topic:: Recursive function definitions without a name

   Paragraph three asks this question: How do we define a recursive anonymous function? Doesn't the
   function need a name so that it can call itself?

   One workaround is to provide a duplicate of the function as an input argument value, which is
   then bound to a parameter name so that it can be called within the function definition, like so::

     ·∾  import Data.Function (fix)

     ·∾ -- Fix duplicates the function and feeds
     ·∾ -- it to itself as the first argument.

     ·∾  fix (\f n -> if n == 0 then 1 else n * f (n-1)) 3
     6

     ·∾ import Unsafe.Coerce (unsafeCoerce)

     ·∾ -- Fix, above, corresponds to the Y-combinator
     ·∾ -- in lambda calculus, but the definition is
     ·∾ -- pretty different. It looks like
     ·∾ -- fix f = let x = f x in x. The lambda calculus
     ·∾ -- version looks like (λy.(λx.y(xx))(λx.y(xx))).

     ·∾ -- Using unsafeCoerce we can write a definition
     ·∾ -- of the Y-combinator that corresponds closely
     ·∾ -- to that found in untyped lambda calculus.

     ·∾ uc = unsafeCoerce

     ·∾ :{
      ⋮ (\y -> (\x -> y (uc x x)) (\x -> y (uc x x)))
      ⋮ (\f n -> if n == 0 then 1 else n * f (n-1))
      ⋮ 6
      ⋮ :}
     6

     ·∾ :{
      ⋮ (\f -> (\x -> f (uc x x)) (\x -> f (uc x x)))
      ⋮ (\f n -> if n == 0 then 1 else n * f (n-1))
      ⋮ 20
      ⋮ :}
     2432902008176640000

In this chapter, we will:

* explore what recursion is and how recursive functions evaluate;
* go step-by-step through the process of writing recursive functions;
* have fun with bottom.


8.2 Factorial!
--------------
Let's examine a simple factorial function::

  factorial 0 = 1
  factorial n = n * factorial (n - 1)

.. "Base case" is first mentioned in 8.2, paragraph 5, **"Let's look at some broken code to
.. introduce the concept of a base case:"**.
..
.. paragraph 7, sentence a **"The way we can stop a recursive expression is by having a base case
.. that stops the self-application to further arguments."**, sentence c **"Here's what that looks
.. like for factorial:"**.

Recursive functions are comprised of two categories of input cases: base cases and recursive cases.
A recursive function can contain any number of base cases, but must have at least one recursive
case.

.. What is a "case" in mathematics? I've seen a few phrases that use the term: special case, edge
.. case, base case, recursive case.
..
.. ddg.gg "!mw case". 1a: a set of circumstances or conditions. 2: condition. 4: what actually
.. exists or happens. "He thought he had failed, but that wasn't the case."
..
.. Case. Noun (1). Case is used to direct attention to a real or assumed occurence or situation that
.. is to be considered, studied, or dealt with. "a case of mistaken identity"
..
.. ddg.gg "!etymology case".
.. https://www.etymonline.com/word/case
.. case, from latin casus "a chance, occasion, opportunity; accident, mishap". Literally "a falling"
.. from cas-, past participle stem of cadere "to fall, sink, settle down, decline perish". from PIE
.. root \*kad- "to fall". The notion is of "that which falls" as "that which happens" (compare
.. befall).
..
.. Maybe it's called the "base case" because it forms the *basis* of any inductive reasoning steps
.. describe in the recursive case.

.. "Every proof by induction consists of two parts, the basis and the induction step." ~ Theory of
   Computation, Sipser, page 23, ch 0: Introduction, 0.4 Types of proof, proof by induction.

..  Base cases are instances of problems that can be solved without carrying out recursive calls.

Base cases are where a functions output value can be obtained without
requiring further recursive calls. In the ``factorial`` function
declaration above, the base case is written as the equation
``factorial 0 = 1``. Technically, the input value ``0`` is the base
case, and ``1`` is the value that corresponds to it. In the case of
``0``, we return ``1``. If you think of recursive calls as inductive
reasoning steps, the base case forms *the basis* for those inferences.

Recursive cases are where self-referencing function calls occur. A recursive call applies the
function definition to different input values. Each call of the function splits the input into
smaller values of that function. Theses values are then combined to arrive at the final answer. In
our function declaration, the recursive case is the equation ``factorial n = n * factorial (n - 1)``.

In order for function evaluation to eventually stop, each recursive call of this case should move
progressively closer towards one of the base cases. If not, the function will call itself forever, a
phenomena know as infinite recursion.

Here is an example of how ``factorial 4`` evaluates::

  factorial 0 = 1
  factorial n = n * factorial (n - 1)

  -- The ==> symbol indicates a new step in the reduction process. Each call of
  -- ⧼factorial arg⧽ is replaced with its definition, where ⧼n⧽ has been replaced
  -- by the current argument.

  factorial 4 ==> 4 * factorial (4 - 1)
              ==> 4 * (4 - 1) * factorial ((4 - 1) - 1)
              ==> 4 * (4 - 1) * ((4 - 1) - 1) * factorial (((4 - 1) - 1) - 1)
              ==> 4 * (4 - 1) * ((4 - 1) - 1) * (((4 - 1) - 1) - 1) * factorial ((((4 - 1) - 1) - 1) - 1)
              --                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^
              --                                       The base case is triggered here.
              --                                                      v
              ==> 4 * (4 - 1) * ((4 - 1) - 1) * (((4 - 1) - 1) - 1) * 1
              ==> 4 * 3 * 2 * 1 * 1
              ==> 4 * 3 * 2 * 1
              ==> 4 * 3 * 2
              ==> 4 * 6
              ==> 24


If we didn't supply the base case ``0 -> 1``, then the recursive call would
never stop, subtracting forever.

8.2.1 Another way to look at recursion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. 2b **"The number of times the function may be applied depends on the arguments to the function,
..       and the applications can be infinite if a stopping point is not clearly defined"**.

Here's another example, to show how building up the call stack resembles
composing multiple instances of the same function (this is a combination
of figures 8 and 9)::

  applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
  applyTimes 0 f b = b
  applyTimes n f b = f (applyTimes (n-1) f b)

  incTimes' :: (Eq a, Num a) => a -> a -> a
  incTimes' times n = applyTimes times (+1) n

  applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
  applyTimes 0 f b  =  b
  applyTimes n f b  =  f . applyTimes (n-1) f $ b
  --                     ^
  --  Building up the series of self-referential function compositions...

.. include:: exercises/8.2.2_-_intermission_exercise.rst


8.3 Bottom
----------
``⊥``, or bottom, denotes computations that don't successfully result in a
value. The two main varieties of bottom are computations that failed with an
error or those that failed to terminate (for example, and infinite loop).
In logic ``⊥`` corresponds to ``False``.

Non-termination
^^^^^^^^^^^^^^^
Here's a non-terminating expression. When we run it, GHCi gets stuck in an
infinite loop::

  ·∾ let x = x in x
  ^CInterrupted.

With a different version of GHC, the expression ``let x = x in x`` may have
resulted in an exception, instead of heating up my laptop until I pressed Control-c.

Partial functions
^^^^^^^^^^^^^^^^^
Another source of bottom are partial functions. For example::

  ·∾ :{
   ⋮ f :: Bool -> Int
   ⋮ f False = 0
   ⋮ :}
  ·∾
  ·∾ f False
  0
  ·∾ f True
  *** Exception: <interactive>:3:1-11: Non-exhaustive patterns in function f

In order to defend against this, we can define a catch-all case. Or, if it doesn't make sense to do
so, we can explicitly return nothing, using the ``Maybe`` data type, like this::

  ·∾ :{
   ⋮ f :: Bool -> Maybe Int
   ⋮ f False = Just 0
   ⋮ f _     = Nothing
   ⋮ :}
  ·∾
  ·∾ f True
  Nothing
  ·∾ f False
  Just 0

Undefined values
^^^^^^^^^^^^^^^^
What happens if we try to evaluate ``undefined`` in the repl? ::

  ·∾ undefined
  *** Exception: Prelude.undefined
  CallStack (from HasCallStack):
    error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
    undefined, called at <interactive>:22:1 in interactive:Ghci9

Intentionally thrown errors
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Another source of bottom values are intentionally thrown errors. The function
``error`` prints a string and returns ``undefined``. ::

  ·∾ error "Should this be in a monad?"
  *** Exception: Should this be in a monad?
  CallStack (from HasCallStack):
    error, called at <interactive>:27:1 in interactive:Ghci9


8.4 Fibonacci numbers
---------------------

8.4.1 Consider the types
^^^^^^^^^^^^^^^^^^^^^^^^
.. topic:: Why didn't the authors use ``Natural``?

   The Fibonacci function we're defining only works on positive numbers, so it makes more sense to
   use a type that can only represent positive integers, like ``Natural`` or ``Word``. Why didn't
   the authors do that?

   Probably to avoid discussing the mechanics of how negative numeric literals are resolved to
   values by GHC.

   When GHC evaluates an expression like ``(-10)``, it does not immediately turn it into a
   number with the value :math:`-10`, but instead desugurs it into ``(negate 10)``. At this
   point, ``negate`` tries to turn a value ``10`` to ``(-10)`` of type ``Natural``, but it
   can't, so it throws an exception if done at runtime. If we try compiling a file with this
   expression, instead, we'll get a warning and it won't compile. I was expecting a type
   error, instead.

   I'm still confused, and I'll have to read up on this. But hey, I learned something new!

   https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/lexical_negation.html#extension-LexicalNegation

.. topic:: Can I write a better type signature for Fibonacci?

   Here's an article about writing a proof for Fibonacci in a LiquidHaskell type annotation.
   https://ucsd-progsys.github.io/liquidhaskell-blog/2016/09/18/refinement-reflection.lhs/


8.5 Integral division from scratch
----------------------------------
Here's an example for integral division. The inner ``go``
function keeps a count that the outer ``dividedBy`` function
doesn't care about:

.. include:: figures/DividedBy.hs
   :code:


8.6 Chapter Exercises
---------------------

.. include:: exercises/8.6.1_-_review_of_types.rst

.. include:: exercises/8.6.2_-_reviewing_currying.rst

.. include:: exercises/8.6.3_-_recursion.rst

.. include:: exercises/8.6.4_-_fixing_dividedby.rst

.. include:: exercises/8.6.5_-_mccarthy_91_function.rst


.. further reading: https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

.. Categories of recursive procedures
.. ----------------------------------
.. linear: when a methods call itself only once each repetition.
.. tail: like linear recursion, but the recursive call is the last operation carried out in the recursive case.
.. multiple: when a method calls itself several times at once in some recursive case.
.. mutual: When a set of methods call each other cyclically. Sometimes called indirect recursion.
.. nested: when an argument of a recursive function is defined through another recursive call.  Consider the McCarthy 91 function.

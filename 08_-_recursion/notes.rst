**********************
 Chapter 8: Recursion
**********************

  .. image:: figures/nickieatmirror.jpg
     :align: center

  **Something is recursive when it contains progressively smaller nested instances of itself.**


  "Recursion is the root of computation since it trades description for time."

  ~ Alan Perlis


  "The power of recursion evidently lies in the possibility of defining an infinite
  set of objects by a finite statement. In the same manner, an infinite number of
  computations can be described by a finite recursive program, even if this program
  contains no explicit repetitions."

  ~ Niklaus Wirth. "Algorithms + Data Structures = Programs"


  "The recursive problem solving process can be described loosely as follows:

    * If the given instance of the problem can be solved directly, do so.
    * Otherwise, reduce it to one or more smaller instances of the same problem."

  ~ Jeff Erickson


8.1 Recursion
-------------
In this chapter, we will:

* explore what recursion is and how recursive functions evaluate;
* go step-by-step through the process of writing recursive functions;
* have fun with bottom.


8.2 Factorial!
--------------
Let's examine a simple factorial function::

  factorial 0 = 1
  factorial n = n * factorial (n - 1)

Recursive functions are comprised of two categories of input cases: base cases and recursive cases.
A recursive function can contain any number of base cases, but must have at least one recursive
case.

Base cases are where the functions output can be obtained without requiring further recursive calls.
In the ``factorial`` function declaration above, the base case is ``factorial 0 = 1``.

Recursive cases are where self-referencing function calls occur. A recursive function call is where
the function definition is applied to different input values. Each call of the function splits the
input it into smaller instances of that function, building up intermediate results along the way
which will eventually be combined to arrive at the final answer. In our function declaration, the
recursive case is the equasion ``factorial n = n * factorial (n - 1)``.

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
Here's another example, to show how building up the call stack resembles
composing multiple instances of the same function::

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
error or those that failed to terminate. In logic ``⊥`` corresponds to
``False``.

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
This section describes how the author would come up with a Fibonacci function in Haskell. I wasn't
able to understand the reasoning process, here. So it's completely useless. Shit.


8.5 Integral division from scratch
----------------------------------
Here's and example for integral division. The inner ``go`` function keeps a count that the outer
``dividedBy`` function doesn't care about:

.. include:: figures/DividedBy.hs
   :code:


8.6 Chapter Exercises
---------------------

.. include:: exercises/8.6.1_-_review_of_types.rst

.. include:: exercises/8.6.2_-_reviewing_currying.rst

.. include:: exercises/8.6.3_-_recursion.rst

.. include:: exercises/8.6.4_-_fixing_dividedby.rst

.. include:: exercises/8.6.5_-_mccarthy_91_function.rst

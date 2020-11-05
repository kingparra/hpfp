**********************
 Chapter 8: Recursion
**********************


8.1 Recursion
-------------
In this chapter, we will

* explore what recursion is and how recursive functions evaluate;
* go step-by-step through the process of writing recursive functions;
* have fun with bottom.


8.1 & ½, Some remarks about recursion
-------------------------------------
Recursion, at its essence, is really just a *pattern*.

**If smaller self-similar instances of an overall strucutre form
part of itself, then that structure is recursive.**

As programmers, however, we're most interested in recursion as an algorithmic
problem solving tool.

Most recursive solutions are a special case of divide-and-conquer. In
divide-and-conquer a problem is repeatedly broken down into sub-problems, which
are then used as a black box. The result of these black box sub-problems are
then combined to produce the final result. With recursive solutions, however,
each sub-problem is a smaller instance of the overall problem.

A recursive function is often defined as "a function that calls itself". What we
mean by "calls itself" is that the function creates another execution instance.
A function is a "cookie cutter" from which any number of execution instances can
be created. While there is only on definition for a recursive function, there
can be any number of execution instances.

If the definition of a recursive function were written so that the function
calls itself unconditionally, then every execution instance would
unconditionally call another instance, forever. This is referred to as
*infinite recursion*, or non-termination.

So, to make our functions useful, we must supply a non-recursive component, that
is triggered on some condition, to stop the recursive self-referencing calls.
This component is known as the *base case*.

In order to ensure that our base case is triggered, though, we must design the
recursive case to work towards it. Each invocation (or execution instance) of
the recursive case should either decrease the input size or simplify the
problem in a way that brings you closer to the base case.

Linear recursion occurs when methods call themselves only once.

Tail recursion is a type of linear recursion where the recursive call is the
last operation carried out int the recursive case. Therefore, they do not
manipulate the result of the recursive call.

Multiple recursion is where a method calls itself several times in some
recursive case.

Mutual recursion is when a functions call each other in a cyclical order.

Nested recursion occurs when an argument of a recursive function is defined
through another recursive call.


8.2 Factorial!
--------------
Let's examine a simple factorial function::

  factorial 0 = 1
  factorial n = n * factorial (n - 1)

Base case: recursion stops when we get a 0. The return value, 1, is the identity
value.

Recursive case: Critically, the input size of this case gets smaller with each
application.

An example of how ``factorial 4`` evaluates::

  factorial n  =  n * factorial (n - 1)
            4  =  4 * factorial (4 - 1)
            3  =  4 * 3 * factorial (3 - 1)
            2  =  4 * 3 * 2 * factorial (2 - 1)
            1  =  4 * 3 * 2 * factorial (1 - 1)
            0  =  -- this triggers the base case 0 -> 1
                  4 * 3 * 2 * 1
                  -- the call stack collapses here
                  -- and the expression is reduced
               =  24

If we didn't supply the base case ``0 -> 1``, then the recursive call would
never stop, subtracting infinitely.

.. include:: exercises/8.2.2_-_intermission_exercise.rst


8.3 Bottom
----------
``⊥``, or bottom, denotes computations that don't successfully result in a
value. The two main varieties of bottom are computations that failed with an
error or those that failed to terminate. In logic ``⊥`` corresponds to
``False``.

Here's a non-terminating expression. When we run it, GHCi gets stuck in an
infinite loop::

  ·∾ let x = x in x
  ^CInterrupted.

With a different version of GHC, the expression ``let x = x in x`` may have
resulted in an exception, instead.

What happens if we try to evaluate ``undefined`` in the repl? ::

  ·∾ undefined
  *** Exception: Prelude.undefined
  CallStack (from HasCallStack):
    error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
    undefined, called at <interactive>:22:1 in interactive:Ghci9

Another source of bottom values are intentionally thrown errors. The function
``error`` prints a string and returns ``undefined``. ::

  ·∾ error "Should this be in a monad?"
  *** Exception: Should this be in a monad?
  CallStack (from HasCallStack):
    error, called at <interactive>:27:1 in interactive:Ghci9


8.4 Fibonacci numbers
---------------------
1. Consider the types
2. Consider the base case
3. Consider the arguments
4. Consider the recursion

I think I prefer these questions, instead...

* What are the preconditions?
* What stays the same? (base case)
* What changes during a recursive step?
* Under what circumstances does it change?
* Are the subproblems getting smaller?


8.6 Chapter Exercises
---------------------

.. include:: exercises/8.6.1_-_review_of_types.rst

.. include:: exercises/8.6.2_-_reviewing_currying.rst

.. include:: exercises/8.6.3_-_recursion.rst

.. include:: exercises/8.6.4_-_fixing_dividedby.rst

.. include:: exercises/8.6.5_-_mccarthy_91_function.rst

*******************
 Chapter 8 Outline
*******************

* 8.1 Recursion

  What makes a function recursive. Indefinite repetitions.

  Recursion is a pattern that occurs in language and nature. Indefinite repetitions.

  Haskell and lambda calculus have similar evaluation strategies, and recursive anonymous function
  literals can be written using the Y-combinator in both languages.

  Recursion is the only method of expressing repetition in Haskell, so you need to understand it in
  order to read other peoples code. Even though you can express repetition yourself using only
  higher-order functions like map, filter, and fold, direct recursion is used a lot by other
  programmers.

* 8.2 Factorial!

  What a factorial function looks like.

  A stub for factorial that only works for one input.

  We want to generalize this.

  Broken code to demonstrate factorial, generalized but without a base case.

  A base case is non-recursive, and is what stops self-application. An example follows.

  The base case for this function is the identity for multiplication.

  * 8.2.1 Another way to look at recursion

    Both composition and recursive calls chain functions together.

    Composition has a definite number of repetitions, but recursive calls are indefinite.

    An example of composition.

    "Recursion is self-referential composition." Or, really, composition resembles the function call
    stack that is accumulated during recursive calls.

    The rest of this section is examples or bullshit.

  * 8.2.2 Intermission: Exercise

    Write the evaluation steps for this expression. When I did that, it looked like a representation
    of building and collapsing the call stack.

* 8.3 Bottom

  Bottom represents computations that don't result in a value, like expressions that result in an
  error or infinite loops.

  Example of an infinite loop.

  Example of a partial function.

  How unhandled cases of a partial function correspond to exceptions thrown with the error function.

* 8.4 Fibonacci numbers

  * 8.4.1 Consider the types
  * 8.4.2 Consider the base case
  * 8.4.3 Consider the arguments
  * 8.4.4 Consider the recursion

* 8.5 Integral division from scratch
* 8.6 Chapter exercises

  * 8.6.1 Review of types

    * 1
    * 2
    * 3
    * 4

  * 8.6.2 Reviewing currying

    * 1
    * 2
    * 3
    * 4
    * 5
    * 6

  * 8.6.3 Recursion

    * 1
    * 2
    * 3

  * 8.6.5 Fixing ``dividedBy``
  * 8.6.6 ``McCarthy91`` function
  * 8.6.7 Numbers into words

* 8.7 Definitions

  * Recursion

    This is a terrible definition of recursion.

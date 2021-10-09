***********************************
 How to design recursive functions
***********************************


.. topic:: Design recipe for writing recursive programs

   Stolen from `CS457`_.

   Here is a set of steps to help you write recursive functions.
   If you follow each step you are more likely to figure out
   what to do.

   .. _CS457: http://web.cecs.pdx.edu/~sheard/
      course/CS457-557/Haskell/RecursiveDesignRecipe.html

   * Write down several examples of how the function works (include
     both calls and outputs).

   * Decide which of the arguments is the one that is recursively
     decomposed.

   * Figure out the base case input for that argument.

   * Figure out the recursion by breaking the non-base case input
     for that argument into multiple pieces.

   * Give a name to each of the pieces, there might be 1, 2, or many
     pieces.

   * Work out a recursive example

     * Pick an example from the several examples above that will be
       recursive.

     * Study the inputs and the result.

     * Choose one of the named pieces of the recursively decomposed
       recursive argument as an argument of a recursive call. What
       part of the recursively decomposed argument (from the example)
       does that name refer to? What do the other names refer to?

     * What is the answer for that recursive call?

     * How can you combine the answer from the recursive call with
       the other named pieces to get the answer for the whole
       problem

   * Use the steps from the generic program design recipe (where the
     structure of the body mimics the structure of the input data)
     to complete the function definition.

     * It is almost always the case that the structure of breaking
       up the recursive argument is the structure you need to mimic.


.. topic:: The basic steps of a function design recipe

   Stolen from `HTDPs preface, figure 1`_.

   .. _HTDPs preface, figure 1: https://htdp.org/2021-5-4/Book/part_preface.html

   **From Problem Analysis to Data Definitions**

   Identify the information that must be represented and how it is
   represented in the chosen programming language. Formulate data
   definitions and illustrate them with examples.

   **Signature, Purpose Statement, Header**

   State what kind of data the desired function consumes and
   produces. Formulate a concise answer to the question what the
   function computes. Define a stub that lives up to the
   signature.

   **Functional Examples**

   Work through examples that illustrate the function's purpose.

   **Function Template**

   Translate the data definitions into an outline of the function.

   **Function Definition**

   Fill in the gaps in the function template. Exploit the purpose
   statement and the examples.

   **Testing**

   Articulate the examples as tests and ensure that the function
   passes all. Doing so discovers mistakes. Tests also supplement
   examples in that they help others read and understand the
   definition when the need arises — and it will arise for any
   serious program.

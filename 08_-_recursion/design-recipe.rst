***********************************
 How to design recursive functions
***********************************
Here are a few problem solving techniques I've found from various
sources. These are only outlines of the general process, the
source material that they come from has examples of their use. To
really understand them, you should read the source material, and
practice using them on a few simple programming problems.


.. topic:: Rules for recursion

   From "Get Programming with Haskell" by Will Kurt.

   The way to solve recursive function is by following this simple set of rules:

   * Identify the end goals.
   * Determine what happens when a goal is reached.
   * List all alternate possibilities.
   * Determine your "rinse and repeat" process.
   * Ensure that each alternative moves you toward your goal.


.. topic:: General program design recipe

   Stolen from `CS457`_.

   To write a program one follows a recipe of steps. Here are the steps we will use for all our
   programs.

   * Each step is a concrete activity.
   * Each step has a product that can be inspected.

   Here are the steps we will follow:

   * Understand the problem. Often the analysis is inserted in the program as a comment.

   * Write a contract for each function about how its inputs and outputs behave. In Haskell, a
     contract is written using a typing prototype.

   * Create a set of examples. We will use HUnit assertions as the product that encodes examples.

   * Write the body of each function. Use the guideline that the structure of the body follows the
     structure of the input data.

   * Testing. Test your program. We will use HUnit tests as the testing product. Later we will learn
     how to use QiuckCheck random testing as additonal testing products.

   If you do not have all the products then you have not followed the design recipe, and you are
   much more likely to make mistakes.


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


.. topic:: Process of computational problem solving

   Stolen from "Introduction to Computer Science Using Python: A Computational Problem-Solving
   Focus" by Charles Dierbach, Page 18, Figure 1-22 Process of Computational Problem Solving.

   **Analysis**

   * Clearly understand the problem.
   * Know what constitutes a solution.

   **Design**

   * Determine what type of data is needed.
   * Determine how data is to be structured.
   * Find and/or design appropriate algorithms.

   **Implementation**

   * Represent data within the programming language.
   * Implement algorithms in the programming language.

   **Testing**

   * Test the program on a selected set of problem instances.
   * Correct and understand the causes of any errors found.


.. topic:: PEDAC

   Taken from the medium article `Solving Coding Problems With PEDAC
   <https://medium.com/launch-school/solving-coding-problems-with-pedac-29141331f93f>`_.

   **Problem**

   * Identify expected input and output.
   * Make the requirements explicit.
   * Identify rules.
   * Form a mental model of the problem, if you can.

   **Examples**

   * Validate your understanding of the problem by coming up with examples for how the program or
     subprogram should behave given particular inputs. Ask the customer if the output in your
     example matches their expectation. Try to find properties that always hold true for any output.
   * These examples can be turned into automated tests.

   **Data**

   * How we represent data that we'll work with in our program. Think about what representation most
     closely matches your mental model of the problem, and is easiest for other programmers to read.

   **Algorithm**

   * Steps for converting input to output. Consider time complexity, intermediate results.

   **Code**

   * Implement the algorithm and any data structures needed in your programming language.


.. topic:: Polya's problem solving techniques

   Huerestics for solving mathematical problems, taken from "How to Solve It: A New Aspect of
   Mathematical Method" by George Pólya, `mirrored here <http://www.math.utah.edu/~pa/math/polya.html>`_.

   **Understanding the problem**

   * First. You have to understand the problem.
   * What is the unknown? What are the data? What is the condition?
   * Is it possible to satisfy the condition? Is the condition sufficient to determine the unknown? Or
     is it insufficient? Or redundant? Or contradictory?
   * Draw a figure. Introduce suitable notation.
   * Separate the various parts of the condition. Can you write them down?

   **Devising a plan**

   * Second. Find the connection between the data and the unknown. You may be obliged to consider
     auxiliary problems if an immediate connection cannot be found. You should obtain eventually a
     plan of the solution.
   * Have you seen it before? Or have you seen the same problem in a slightly different form?
   * Do you know a related problem? Do you know a theorem that could be useful?
   * Look at the unknown! And try to think of a familiar problem having the same or a similar unknown.
   * Here is a problem related to yours and solved before. Could you use it? Could you use its
     result?  Could you use its method? Should you introduce some auxiliary element in order to make
     its use possible?
   * Could you restate the problem? Could you restate it still differently? Go back to definitions.
   * If you cannot solve the proposed problem try to solve first some related problem. Could you
     imagine a more accessible related problem? A more general problem? A more special problem? An
     analogous problem? Could you solve a part of the problem? Keep only a part of the condition,
     drop the other part; how far is the unknown then determined, how can it vary? Could you derive
     something useful from the data? Could you think of other data appropriate to determine the
     unknown? Could you change the unknown or data, or both if necessary, so that the new unknown
     and the new data are nearer to each other?
   * Did you use all the data? Did you use the whole condition? Have you taken into account all
     essential notions involved in the problem?

   **Carrying out the plan**

   * Third. Carry out your plan.
   * Carrying out your plan of the solution, check each step. Can you see clearly that the step is
     correct? Can you prove that it is correct?

   **Looking back**

   * Fourth. Examine the solution obtained.
   * Can you check the result? Can you check the argument?
   * Can you derive the solution differently? Can you see it at a glance?
   * Can you use the result, or the method, for some other problem?

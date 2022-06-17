*********************
 Chapter 1 Questions
*********************

* Why did the author choose to write about LC for chapter 1?
* How is LC relevant to programming in haskell?
* What abilities do you gain from the chapter that are relevant to functional programming?
* **How should I explain lambda calculus to my mom over the phone?**

* What are the codomain, image, and range of a function, and how do they differ?

  * What can go into a function is called the Domain.

  * What may possibly come out of a function is called the codomain. This includes semi-computable
    elements, like the empty set.

  * What actually comes out of a function is called the range. It is a subset of the codomain.
    Assuming the function under consideration is *total*, or defined for all inputs, then the term
    image and range mean the same thing.

* Are partial functions really functions at all, or are they binary relations, instead?

  ::

    .liberachat   weechat    2.  #haskell 3.  #cs      4.  ##math
    -----------------------------------------------------------------------------------------------------------------------------------------
    justsomeguy | Is there a difference between the range of a function and the image of that same function?
    justsomeguy | Wikipedia says that they mean the same thing! Phew, that was confusing for a moment.
       Z-module | justsomeguy: well, "range" also has an older meaning taht we now call codomain, not generally the same as the image.
       Z-module | best to avoid the word "range", really
       Z-module | "Let f be defined on the corange X ...."
    justsomeguy | Another dumb question -- are partial functions really functions at all, or are they relations, instead?
       Z-module | they're not functions in the normal sense
       Z-module | a true function must have a definite value f(x) in the codain for each x in the domain
          mh_le | Z-module: why is the codomain distinct from the image of a function?
      loopspace | Not every function is surjective
       Z-module | Say f: X -> Y   (Y being the codomain). That only means f(x) is in Y, for every x in X. The "image" of f is f(X) = {f(x) :
                | all x in X}
          mh_le | Z-module: right, what I mean is, why is it desirable to have make the co-domain distinct from the image?
       Z-module | reasons having to do with exact sequences, quotient structures, etc etc
          mh_le | Z-module: fair enough
    justsomeguy | I don't really know for sure, but my guess is that its a way to underspecify the possible outputs. I imagine that sometimes
                | it's difficult to understand what a function will output (if it comes from measurements obtained in real life). Do you
                | think that sound reasonable?
            riv | yes dom(f) may be simpler to describe than img(f)
          mh_le | riv: don't you man codom(f)?
       Z-module | mh_le: what I should have said, in a more big-picture sense, is that for just a *single* function, the particular codomain
                | being strictly bigger than the image is probably quite unimportant. It's more an issue when dealing with many functions,
                | particularly ones arrayed in some sort of diagram.
          mh_le | ahha yeah that makes sense
       Z-module | and also you get trivial but occasionally useful topology lemmas such as:  If  f: X -> Y  is any continuous map of
                | topological spaces, then enlarging Y to any bigger set, while keeping all the original f values, yields a new map that is
                | still continuous.
       Z-module | (s/bigger set/topology on a larger set Y' in which all the open sets of Y are still open/ )

* What is the "universe of discourse"?
* Why does section 1 bother me so fucking much?

  * Something about the way it's written. It doesn't do a good job of motivating the
    subject, and introduces a lot of jargon right away, in the first paragraph. There
    are no learning objective listed, and the target audience isn't clear, either.
    Why doesn't the author explain *why lambda calculus is useful*?

* What is functional programming?

  https://www.lihaoyi.com/post/WhatsFunctionalProgrammingAllAbout.html

  https://www.manning.com/books/grokking-simplicity

  "Introduction to Functional Programming from Lambda Calculus" by Greg Michaelson

* Why is it often said that FP requires garbage collection to be practical?

  * A HN comment https://news.ycombinator.com/item?id=15138429

* What is a function? (Describe it ten different ways.)

  "A function is a rule of correspondence by which when anything is given (as argument)
  another thing (the value of the function for that argument) may be obtained. That is, a
  function is an operation which may be applied on one thing (the argument) to yield
  another thing (the value of the function). It is not, however, required that the
  operation shall necessarily be applicable to everything whatsoever; but for each
  function there is a class, or range, of possible arguments -- the class of things to
  which the operation is significantly applicable -- and this we shall call the range of
  arguments, or range of the independent variable, for that function. The class of all
  values of the function, obtained by taking all possible arguments, will be called the
  range of values, or range of the dependent variable."

  ~ Alonzo Church,
    "The Calculi of Lambda Conversion" 1941,
    http://users.bestweb.net/~sowa/logic/alonzo.htm

  Functions are abstract dependencies between objects.

  Functions are expressions that describe a dependent variable (the output) in terms of an
  independent variable (the input value, or in more words: the value of a saturated
  parameter for one execution instance).

  A function:

    * takes one argument
    * binds the dereferenced value of that argument to a parameter
    * replaces all occurrences of that parameter in the body with value it stands for
    * performs any operations within the function body to combine all of the literal values
    * and then reduces to a single resulting value

  A function is a machine for generating lexically scoped name bindings.

  A function is an expression that introduces a name local to its body.

  A functions is a literal value independent of time.

  A function is a machine for generating a value that is executed in a certain amount of time.

  A function is a series of instructions for performing an operation.

  A function has no instructions -- it's a description of a value, instead.

  A function is a circuit.

  A function is a way to express the idea of *change*.

  A function doesn't express change, it only relates an input to an output.

  "A [function is a] mathematical object that sets up an input-output relationship."

  FP is all about treating programs as expressions.

  .. The essential characteristic of a function
  .. is that it can be applied. Functions
  .. introduce a locally scoped name binding as a
  .. parameter. You can think of a function as a
  .. description of a dependent variable in terms
  .. of an independent variable.

  FP languages are based on an execution model
  similar to how LC evaluates expressions into
  values using text substitution. Languages
  that execute things


* Is it possible to tell if two functions are equal?

  * What would it mean for two functions to be equal?

    * extensional vs intensional
    * should running time be considered?
    * should space usage be considered?
    * should the abstract machine be considered?
    * should the susceptibility of reduction to other kinds of problems be considered?

* Are functions by their nature reliant on the concept of time?
* Are purely functional languages by their nature also declarative? (Using function calls as the only means for control
  unifies data flow and control flow, but does it eliminate having to think of control flow entirely?)
* What is the church-rosser theorem, and what does it mean?
* I want to explicitly represent reduction steps. What is a good notation to use for that?
* What are operational semantics and denotational semantics?
* Can an algorithm exists separately of a model of computation?
* How are common data types represented?
* What particular lambda calculus is the Core type an implementation of?
* What problems or events motivated Church to create the lambda calculus?

  * Hilbert's 10th problem (1900)
  * Hilbert's decision problem (1928)

* What kinds of things can we measure with lambda calculus?
* What are the basic units of time and space complexity in LC?

  * Things to consider:

    * for beta reduction

      * mechanism for enumerating all occurrences of the parameter in the body of the function

        * time needed for that enumeration
        * memory needed to hold the input expression before substitution
        * time needed to substitute all occurrences of the parameter

          * can the results of common subexpressions be shared?

            * will that require building a graph (or other intermediary structure) -- what are the
              time and space complexity requirements of that?

  * The typed lambda calculus is not elementary recursive. Richard Statman.
    https://www.sciencedirect.com/science/article/pii/0304397579900070?via%3Dihub
    https://doi.org/10.1016/0304-3975(79)90007-0

  * Lambda-Calculus with Director Strings
    Maribel Fernández, Ian Mackie, François-Régis Sinot
    DOI 10.1007/s00200-005-0169-9

  * Beta reduction is invariant, indeed.
    Beniamino Accattoli, Ugo Dal Lago.
    https://dl.acm.org/doi/10.1145/2603088.2603105

  * Typed lambda-calculi with sharing and unsharing.
    A summary of university of baths research project to develop a new approach to
    efficient evaluation in the lambda calculus.
    https://researchportal.bath.ac.uk/en/projects/typed-lambda-calculi-with-sharing-and-unsharing

  * A simpler lambda calculus.
    Barry Jay.
    https://dl.acm.org/doi/10.1145/3294032.3294085

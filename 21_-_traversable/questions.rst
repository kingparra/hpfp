**********************
 Chapter 21 Questions
**********************


**21.2 reading comprehension check**

* What does sequenceA do?
* What does traverse do?
* What is the type signature of sequenceA?
* What is the type signature of traverse?
* How can you derive traverse from sequenceA?
* How can you derive sequenceA from traverse?
* Which type classes does Traversable inherit
  from?
* What are the minimum required class methods
  for a complete instance definition?


**What do I find difficult right now?**

* Writing generators for QuickCheck.
* Understanding how to use the checkers package.
* Remembering the type class laws.
* Writing instances for type classes.
* Understanding trees.
* The Morse code example program.

* Problem decomposition.
* Defining functions in terms of primitive recursion.
* Desugaring the function composition operator.


**The first and third paragraphs of 21.1 left me
with several questions.** Here they are, again:

  "Functor gives us a way to transform any
  values embedded in structure. Applicative
  gives us a way to transform any values
  contained within a structure using a
  function that is also embedded in structure.
  **This means that each application produces
  the effect of "adding structure," which is
  then applicatively combined with other such
  effects.** Foldable gives us a way to
  process values embedded in a structure as if
  they existed in a sequential order, as we've
  seen ever since we learned about list
  folding."

* How does it follow that applicative function
  application adds structure?
* Does each application really add structure?
* *Why* does each application add structure?
* What structure does it add?
* Can you show an example?
* What does "applicatively combined" mean?
* How are the structures combined?

-------------------------------------------------

Applicative gives us a way to transform any
values contained within a structure using a
function that is also embedded in structure.
  
* Can the function and the values be contained
  in the same structure?
* How many structures are we talking about,
  here?
* You say "any values", is this opposed to
  transforming only some values in a structure?

...maybe this can be rephrased to:

Applicative lets us apply functions contained
in one structure to some terms contained in
another structure, respectively, and then
return a resultant structure which contains
the each function applied to each value.

-------------------------------------------------

  "Traversable allows you to transform
  elements inside the structure like a
  functor, **producing applicative effects
  along the way, and lift those potentially
  multiple instances of applicative structure
  outside of the traversable structure.** It
  is commonly described as a way to traverse a
  data structure, mapping a function inside a
  structure while accumulating the
  **applicative contexts** along the way."

* What is an applicative effect?
* What does it mean to lift an instance of
  applicative structure?
* What does lifting mean, generally?
* What is an applicative context?
* Can you show some examples how values are
  accumulated? I can see the mapping, but I'm
  not sure I recognize any accumulation going
  on.


**Is traversable the same thing as an iterator?**
An iterator interface presents operations to:

* access the current element,
* move to the next element,
* and to test for completion.


**So, here's what happened when I asked about
Traversable on IRC**

::

  justsomeguy:
    Can someone give me a dumbed-down, possibly
    inaccurate summary of what Traversable is,
    just to give me a sense of it? Is it an
    iterator? A thingy that flips inner
    structure with outer structure? Something
    that helps me climb trees?

  koz_ justsomeguy:
    Traversable is 'effectful fmap'.
  koz_ :t fmap
  lambdabot Functor f => (a -> b) -> f a -> f b
  koz_ :t traverse
  lambdabot (Traversable t, Applicative f) =>
  (a -> f b) -> t a -> f (t b)
  koz_ s/is/allows/

  * | justsomeguy slowly mulls over what an
      applicative effect is.

  monochrom :
   IO is an example.
  monochrom :
    If you have ["hello", "hi", "aloha"], and
    if you want for each string there you
    putStrLn then getLine, and you want the
    result list to be the 3 lines you get from
    the 3 getLines:
  monochrom :
    traverse (\s -> putStrLn s >> getLine) ["hello", "hi", "aloha"]
  monochrom :
    THE END

  justsomeguy :
    Hrm, that's pretty similar to forM, which
    I've been using for a while now without
    understanding.

  monochrom :
    Just different argument orders.

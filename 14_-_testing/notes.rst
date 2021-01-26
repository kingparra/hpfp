*********************
 Chapter 14: Testing
*********************


This chapter is a little different. Rather than incorporate
my notes of the chapter here, the idea is to improve the
``hangman`` project from last chapter.

Features to implement for ``hangman``:

* Move all functions out of IO other than ``Main.main``.
* Add a test suite using ``hspec`` and ``QuickCheck``.
* Write a "Player" AI who can solve puzzles.
* Add tests for that, too.

As for the AI, Hyiltiz has this to say:

  For the ideas of implementing hangman "bots" (these are
  also called game AI, algorithmic actor, ideal actor if the
  algorithm is the theoretically optimal etc.), here are a
  few things to try:

  * stupid/stuck AI: always guesses A;
  * blind AI: randomly guesses one out of 26 letters;
  * blind AI with memory: randomly guesses one out of the
    remaining letters that wasn't guessed before;
  * deterministic English AI: picks based on the probability
    of character frequency in English in order;
  * stochastic English AI:picks based on the probability of
    character frequency in English in order with memory;
  * English AI with memory: similar to the above but doesn't
    guess what's already been guessed;
  * Omniscient/Ideal AI: has access to the dictionary,
    starts as "deterministic English AI with memory" but
    filters the dictionary based on previous guesses and
    other game state, then computes most likely character
    across all alternatives then guesses that character.

  No need to try them all, but definitely start with the
  simplest and maybe try if you can do the ideal one in the
  end.

Along the way, I hope to add a greater variety of tests. As
I progress through my refactoring I'll be reading:

* `What I Wish I Knews When Learning Haskell, Testing <http://dev.stephendiehl.com/hask/#testing>`_;
* the `QuickCheck Documentation <http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html>`_;
* and the `HSpec Documentation <https://hspec.github.io/>`_.

See the ``projects`` folder and associated git history of
this branch for more information.

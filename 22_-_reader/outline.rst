********************
 Chapter 22 Outline
********************

* 22.1 Reader
* 22.2 A new beginning

  This section is a mix of a long running
  interactive ghci session, and a series of
  examples.

  Here are some short summaries of what each
  paragraph is about.

  1. Creating a module with the ``boop``,
     ``doop``, and ``bip`` functions in it,
     so we can load it into ghci.

  2. What can we know about the code just by
     reading it? Check our assumptions by
     examining expressions in GHCi.

  3. How ``bip`` evaluates by composing
     ``doop`` and ``boop``.

  4. Writing the same composition using
     ``fmap``.

  5. Fmapping over something involves
     modifying elements within a structure
     (or "functorial context").

     What is the structure we ``fmap`` over
     when the thing we ``fmap`` over is a
     function?

  6. Answer: The structure is a partially
     applied function, and the elements are
     the arguments to the function.

  7. Describing arguments to ``(\x -> fmap
     boop doop x)``.

  8. This is the ``Functor`` instance for
     the function type constructor ``(->)``.

  9. Adding the ``bbop`` and ``duwop``
     functions to our file from paragraph 1.

  10. **I don't understand what this paragrph
      is about**

  11. **or this**

  12. What the expression ``bbop 3`` desugars
      to, and how it and evaluates.

  13. **no idea what this means**

  14. Composing ``(+)`` and ``(*2)`` with
      ``(.)``.

  15. The same expression rephrased using
      ``(.)`` and ``(<$>)``.

  16. **this is some kind of demonstration, I
      don't understand what it's trying to
      illustrate, though**

* 22.2.1 Short Exercise: Warming Up

* 22.3 This is Reader
* 22.4 Breaking down the Functor of functions
* 22.5 But uh, Reader?

  * 22.5.1 Exercise: Ask

* 22.6 Functions have an Applicative too

  * 22.6.1 Demonstrating the function Applicative
  * 22.6.2 Exercise: Reading Comprehension

    * 1. Write ``liftA2`` yourself.
    * 2. Write the following function.
    * 3. Implement the ``Applicative`` for ``Reader``.

      * a
      * b

* 22.7 The Monad of functions

  * 22.7.1 The Monad instance
  * 22.7.2 Example uses of the Reader type
  * 22.7.3 Exercise: Reader Monad

    * 1. Implement the Reader Monad.
    * 2. Rewrite the monadic ``getDogRM`` to
         use your Reader datatype.

  * 22.8 Reader Monad by itself is boring
  * 22.9 You can only change what comes below
  * 22.10 You tend to see ReaderT, not Reader
  * 22.11 Chapter Exercises -- page 877

    * 22.11.1 A warm-up stretch

      * 1. Fold the Boolean conjunction operator over the list of
           results of ``sequA`` (applied to some value).
      * 2. Apply ``sequA`` to ``s'`` -- you'll need ``fromMaybe``.
      * 2. Apply ``bold`` to ``ys`` -- you'll need ``fromMaybe``.

    * 22.11.2 Rewriting Shawty

  * 22.12 Definition

    * Monad transformer

  * 22.13 Follow-up resources

    * Reader Monad; All About Monads
      https://wiki.haskell.org/All_About_Monads

    * Reader Monad; Programming with Monads; Real World Haskell
      http://book.realworldhaskell.org/read/programming-with-monads.html

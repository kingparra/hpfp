**********************
 Chapter 22 Questions
**********************
* What problem is ``Reader`` intended to solve?
* Can't I use another simpler method to solve
  that problem, instead?

  * Why not make a function that gets runtime
    configuration information, ingest another
    function, and then spits out a new
    function that can "see" the info?

    (Basically, a function factory.)

* What package and module does the ``Reader``
  type come from?

  * ``tranformers`` https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Reader.html

    * ``Control.Monad.Trans.Reader`` https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Reader.html

  * ``mtl`` https://hackage.haskell.org/package/mtl-2.2.2

    * ``Control.Monad.Reader`` https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html

  Note that the ``mtl`` package depends on ``transformers``.


* Is it included with GHC?

  * Yes, see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/9.0.1-notes.html#included-libraries


* Is it brought into scope by Prelude?

  * No, you need to import either
    ``Control.Monad.Reader`` or
    ``Control.Monad.Trans.Reader``

* What do the ``ask``, ``local``, and
  ``reader`` class methods do?

*********************
 Chapter 16: Functor
*********************


https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Functor.html

**Functor**

  A type f is a Functor if it provides a function fmap
  which, given any types *a* and *b* lets you apply any
  function from **(**\ *a* **->** *b*\ **)** to turn an *f
  a* into an *f b*, preserving the structure of *f*.

  Furthermore *f* needs to adhere to the following:

  * Identity:    ``fmap id == id``
  * Composition: ``fmap (f . g) == fmap f . fmap g``

  Note, that the second law follows from the free theorem of
  the type fmap and the first law, so you need only check
  that the former condition holds.

::

  ·∾ :doc (<$)
   Replace all locations in the input with the same value.
   The default definition is @'fmap' . 'const'@, but this may be
   overridden with a more efficient version.

  ·∾ :info Functor
  class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
    {-# MINIMAL fmap #-}

Instances: ``(Either a)``, ``[]``, ``Maybe``, ``IO``, ``((->) r)``, ``((,) a)``,

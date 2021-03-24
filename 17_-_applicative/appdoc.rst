*************
 Applicative
*************
(The following notes are cobbled together
from the output of ``:doc Applicative``, and
the API docs for ``Control.Applicative``.)

A functor with application, providing operations
to embed pure expressions (``pure``), and sequence
computations and combine their results (``<*>``
and ``liftA2``). ::

  -- pure embeds a regular value into an
  -- applicative context.
  --
  pure :: Applicative f => a -> f a


  -- Sequential application operator.
  -- Pronounced "apply" or "ap".
  --
  -- Takes operation(s) embedded in an
  -- applicative as its left argument, and
  -- values embedded in a applicative as
  -- its right argument.
  --
  -- (<*>) then distributes the operations to
  -- each value, returning a new applicative.
  --
  -- The end result looks a bit like a
  -- cartesian product of functions to
  -- values.
  --
  (<*>) :: Applicative f
        => f (a -> b)
        -> f a
        -> f b


  -- Lift a binary function to actions.
  liftA2 :: Applicative f
         => (a -> b -> c)
         -> f a
         -> f b
         -> f c

A minimal complete definition of ``Applicative``
must include implementations of pure and of either
``<*>`` or ``liftA2``. If it defines both, then
they must behave the same as their default
definitions::

  (<*>) = liftA2 id

  liftA2 f x y = f <$> x <*> y

There are a few additional utility functions for
``Applicative`` in the ``Control.Applicative``
module. ::

  ·∾ :browse Control.Applicative

  . . .

  -- A variant of (<*>) with the arguments reversed.
  (<**>) :: Applicative f
         => f a
         -> f (a -> b)
         -> f b

  . . .

  class Functor f => Applicative (f :: * -> *) where
    . . .
    -- Sequence actions, discarding the value of the first argument.
    (*>) :: f a -> f b -> f b
    -- Sequence actions, discarding the value of the second argument.
    (<*) :: f a -> f b -> f a
    . . .

  -- Other versions of liftA with different arity.

  -- Lift a function to actions. This function may be
  -- used as a value for fmap in a Functor instance.
  liftA :: Applicative f
        => (a -> b)
        -> f a
        -> f b

  -- Lift a ternary function to actions.
  liftA3 :: Applicative f
         => (a -> b -> c -> d)
         -> f a
         -> f b
         -> f c
         -> f d

Further, any definition of ``Applicative`` must
satisfy the following:

**Identity**
``pure id <*> v  =  v``

**Composition**
``pure (.) <*> u <*> v <*> w  =  u <*> (v <*> w)``

**Homomorphism**
``pure f <*> pure x  =  pure (f x)``

**Interchange**
``u <*> pure y  =  pure ($ y) <*> u``

The other methods have the following default
definitions, which may be overridden with
an equivalent specialized implementations::

  u *> v   =   (id <$ u) <*> v

  u <* v   =   liftA2 const u v

As a consequence of these laws, the Functor
instance for f will satisfy::

  fmap f x = pure f <*> x

It may be useful to note that supposing::

  forall x y. p (q x y) = f x . g y

...it follows from the above that::

  liftA2 p (liftA2 q u v) = liftA2 f u . liftA2 g v

If f is also a Monad, it should satisfy::

  pure       =  return

  m1 <*> m2  =  m1 >>= (\x1 -> m2 >>=
                          (\x2 -> return (x1 x2)))

  (*>)       =  (>>)

(which implies that pure and ``<*>`` satisfy the
applicative functor laws).

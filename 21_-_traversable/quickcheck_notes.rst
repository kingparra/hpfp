* ``Arbitrary`` -- type class for types we can
  generate arbitrary elements of.

* ``arbitrary`` -- value of type (Gen a),
  class method of Arbitrary, generates
  arbitrary values.

* ``(Gen a)`` -- An abstract type representing
  a generator for type a.

* generator -- A generator is a function which
  can manufacture an ``a`` in a psuedorandom way.

  newtype Gen a = Gen (Rand -> a)

  * Rand is a random number seed;

  choose :: (Int, Int) -> Gen Int

::

  ·∾ -- These functions trip me up often enough that
  ·∾ -- listing these examples makes sense.
  ·∾ :doc div
   integer division truncated toward negative infinity
  ·∾ :type div
  div :: Integral a => a -> a -> a
  ·∾ div 1 1
  1
  ·∾ div 1 0
  *** Exception: divide by zero
  ·∾ div 0 1
  0

  ·∾ :doc mod
   integer modulus, satisfying
   > (x `div` y)*y + (x `mod` y) == x
  ·∾ :type mod
  mod :: Integral a => a -> a -> a
  ·∾ mod 1 1
  0
  ·∾ mod 1 0
  *** Exception: divide by zero
  ·∾ mod 8 4
  0
  ·∾ mod 4 8
  4
  ·∾ 4 `mod` 8
  4

  ·∾ :doc quot
   integer division truncated toward zero
  ·∾ :type quot
  quot :: Integral a => a -> a -> a
  ·∾ quot 1 1
  1
  ·∾ 11 `quot` 9
  1

  ·∾ :doc rem
   integer remainder, satisfying
   > (x `quot` y)*y + (x `rem` y) == x
  ·∾ :type rem
  rem :: Integral a => a -> a -> a
  ·∾ 11 `rem` 9
  2
  ·∾ rem 1 1
  0
  ·∾ rem 1 8
  1
  ·∾ rem 8 7
  1
  ·∾ rem 8 3
  2
  ·∾ 8 `rem` 2
  0
  ·∾ 8 `rem` 6
  2

  ·∾ -- rounds down
  ·∾ div 20 (-6)
  -4
  ·∾ -- rounds towards zero
  ·∾ quot 20 (-6)
  -3

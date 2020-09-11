module WaxOnWithWhere where

waxOn = x * 5
  where
    z = 7
    x = y ^ 2
    y = z + 8

triple x = x * 3

waxOff x = triple x

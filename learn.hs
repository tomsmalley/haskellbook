module Learn where

f1 = x * 3 + y
    where x = 3
          y = 1000

f2 = x * 5
    where x = 10 * 5 + y
          y = 10

f3 = z / x + y
    where z = y * 10
          y = negate x
          x = 7

waxOn = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

triple x = x * 3

waxOff = triple

module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function   (fix)
import           Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec xs' -> case xs' of
                    []     -> []
                    (y:ys) -> f y : rec ys
                )

fib :: Natural -> Natural
fib = fix (\rec (a, b) m -> if m == 0
                    then a
                    else rec (b, a + b) (m - 1)
                  ) (0, 1)

fac :: Natural -> Natural
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n-1))

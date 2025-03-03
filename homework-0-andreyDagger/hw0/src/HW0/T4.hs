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
map' f = fix (\r xs -> case xs of
                             []     -> []
                             (y:ys) -> f y : r ys)

fib :: Natural -> Natural
fib n = snd (fibHelper n)

fibHelper :: Natural -> (Natural, Natural)
fibHelper = fix (\f n -> case n of
    0 -> (0, 0)
    1 -> (0, 1)
    _ -> (y, x + y) where (x, y) = f (n - 1))

fac :: Natural -> Natural
fac = fix (\f n -> case n of
    0 -> 1
    _ -> f (n - 1) * n)

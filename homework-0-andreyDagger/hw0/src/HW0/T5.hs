module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import           Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ z = z

ns :: Nat a -> Nat a
ns n f z = f (n f z)

nplus :: Nat a -> Nat a -> Nat a
nplus n m f z = n f (m f z)

nmult :: Nat a -> Nat a -> Nat a
nmult n m f = n (m f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns (nFromNatural (n - 1))

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0

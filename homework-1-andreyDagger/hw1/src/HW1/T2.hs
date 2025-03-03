module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import           Numeric.Natural

data N = Z | S N
 deriving Show

nplus :: N -> N -> N
nplus a Z     = a
nplus a (S b) = S (nplus a b)

nmult :: N -> N -> N
nmult _ Z     = Z
nmult a (S b) = nplus (nmult a b) a

nsub :: N -> N -> Maybe N
nsub a Z = Just a
nsub Z (S _) = Nothing
nsub (S a) (S Z) = Just a
nsub a (S b) = case nsub a b of
                  Just val -> nsub val (S Z)
                  Nothing  -> Nothing

ncmp :: N -> N -> Ordering
ncmp a b = case nsub a b of
              Just Z  -> EQ
              Just _  -> GT
              Nothing -> LT


nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S (nFromNatural (x - 1))

-- Uncomment this function signature to implement this function.
-- The correct signature is commented to avoid compilation failure
-- with '-Werror' in the template project.
--
nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = nToNum a + 1

nEven :: N -> Bool
nEven Z     = True
nEven (S a) = not (nEven a)

nOdd :: N -> Bool
nOdd a = not (nEven a)

ndiv :: N -> N -> N
ndiv a b = case nsub a b of
           Nothing  -> Z
           Just val -> nplus (ndiv val b) (S Z)

nmod :: N -> N -> N
nmod a b = case nsub a b of
           Nothing  -> a
           Just val -> nmod val b

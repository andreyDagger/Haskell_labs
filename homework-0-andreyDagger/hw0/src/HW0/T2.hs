{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg x notA = (\case) (notA x)

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f x = f (\g -> g x)
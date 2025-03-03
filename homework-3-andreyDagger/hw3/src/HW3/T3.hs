module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import           HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption None            = None
joinOption (Some None)     = None
joinOption (Some (Some a)) = Some a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)             = Error e
joinExcept (Success (Error e))   = Error e
joinExcept (Success (Success a)) = Success a

-- You may add necessary constraints here
joinAnnotated :: Monoid e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# (e1 <> e2)

joinList :: List (List a) -> List a
joinList Nil        = Nil
joinList (lh :. lt) = append lh (joinList lt)

append :: List a -> List a -> List a
append Nil ys       = ys
append (x :. xs) ys = x :. append xs ys

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F g) = F (\i -> unpack (g i) i)

unpack :: Fun i a -> (i -> a)
unpack (F g) = g

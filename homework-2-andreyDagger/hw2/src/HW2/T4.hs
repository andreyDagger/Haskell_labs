module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (Last a) <> b = a :+ b
  (a :+ t) <> b = a :+ (t <> b)

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints here
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This i <> This j = This (i <> j)
  This i <> That j = Both i j
  This i <> Both j1 j2 = Both (i <> j1) j2
  That i <> This j = Both j i
  That i <> That j = That (i <> j)
  That i <> Both j1 j2 = Both j1 (i <> j2)
  Both i1 i2 <> This j = Both (i1 <> j) i2
  Both i1 i2 <> That j = Both i1 (i2 <> j)
  Both i1 i2 <> Both j1 j2 = Both (i1 <> j1) (i2 <> j2)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS "" <> DS b = DS b
  DS a <> DS "" = DS a
  DS a <> DS b = DS (a ++ "." ++ b)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f1 <> F f2 = F (f1 . f2)

instance Monoid (Fun a) where
  mempty = F id

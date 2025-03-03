module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f z (Branch _ l x r) = tfoldr f (f x (tfoldr f z r)) l
tfoldr _ z Leaf             = z

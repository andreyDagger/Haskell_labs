-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..),
  gUp,
  gDown,
  gLeft,
  gRight,
  gWrite,
  ) where

import           Control.Comonad (Comonad (..))

import           Data.ListZipper (ListZipper (..), lGenerator, lLeft, lRight,
                                  lWrite)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

gUp, gDown :: Grid a -> Grid a
gUp (Grid g) = Grid (lLeft g)
gDown (Grid g) = Grid (lRight g)

gLeft, gRight :: Grid a -> Grid a
gLeft (Grid g) = Grid (fmap lLeft g)
gRight (Grid g) = Grid (fmap lRight g)

gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid $ lWrite newLine g
  where
    oldLine = extract g
    newLine = lWrite x oldLine

gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical = lGenerator gUp gDown

instance Functor Grid where
  fmap f (Grid (LZ ls x rs)) = Grid (LZ (map (fmap f) ls) (fmap f x) (map (fmap f) rs))

instance Comonad Grid where
  extract = extract . extract . unGrid

  duplicate = Grid . fmap gHorizontal . gVertical

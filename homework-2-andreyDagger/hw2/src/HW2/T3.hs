module HW2.T3
  ( epart
  , mcat
  ) where

mcatFolder :: Monoid a => Maybe a -> a -> a
mcatFolder Nothing a  = a
mcatFolder (Just b) a = b <> a

-- You may add necessary constraints here
mcat :: Monoid a => [Maybe a] -> a
mcat = foldr mcatFolder mempty

toMonoid :: (Monoid a, Monoid b) => Either a b -> (a, b)
toMonoid (Right b) = (mempty, b)
toMonoid (Left a)  = (a, mempty)

-- You may add necessary constraints here
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap toMonoid

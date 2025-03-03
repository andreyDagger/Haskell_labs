{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f1 f2) = Iso f2 f1

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f1 _) = f1

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso toAssoc fromAssoc
  where
    toAssoc :: (a, (b, c)) -> ((a, b), c)
    toAssoc (x, (y, z)) = ((x, y), z)

    fromAssoc :: ((a, b), c) -> (a, (b, c))
    fromAssoc ((x, y), z) = (x, (y, z))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso f1 f2
  where
    f1 :: Either a (Either b c) -> Either (Either a b) c
    f1 (Left a) = Left (Left a)
    f1 (Right (Left b)) = Left (Right b)
    f1 (Right (Right c)) = Right c

    f2 :: Either (Either a b) c -> Either a (Either b c)
    f2 (Left (Left a)) = Left a
    f2 (Left (Right b)) = Right (Left b)
    f2 (Right c) = Right (Right c)

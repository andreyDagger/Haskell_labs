module HW2.T2
  ( joinWith
  , splitOn
  ) where

import           Data.List.NonEmpty as NE (NonEmpty (..), head, tail, toList)

splitOnFolder :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitOnFolder delimiter c arr
  | c == delimiter = [] :| toList arr
  | otherwise = (c : NE.head arr) :| NE.tail arr

-- You may add necessary constraints here
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delimeter = foldr (splitOnFolder delimeter) ([] :| [])

joinWith :: a -> NonEmpty [a] -> [a]
joinWith delimiter (x :| xs) = x ++ concatMap (delimiter :) xs

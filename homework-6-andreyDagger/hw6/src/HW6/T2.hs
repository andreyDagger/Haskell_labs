{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import           GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
    Contains name '[] = 'False
    Contains name (name ': xs) = 'True
    Contains name (x ': xs) = Contains name xs

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
    Delete name '[] = '[]
    Delete name (name ': xs) = xs
    Delete name (x ': xs) = (x ': Delete name xs)

type family Add (v :: Symbol) (set :: TSet) :: TSet where
    Add v set = If (Contains v set) set (v ': set)

type family If (cond :: Bool) (thenType :: k) (elseType :: k) :: k where
    If 'True thenType elseType = thenType
    If 'False thenType elseType = elseType

module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import           HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

getResult :: Annotated e a -> a
getResult (a :# _) = a

getState :: Annotated e a -> e
getState (_ :# s) = s

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S (\s -> f (getResult (g s)) :# s)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState st = S (\s ->
  let temp = runS (getResult (runS st s)) (getState (runS st s))
      in getResult temp :# getState temp)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) (S f) (S g) = S (\s ->
    let temp = getState (f s)
    in getResult (f s) (getResult (g temp)) :# getState (g temp))

instance Monad (State s) where
  (>>=) (S g) f = S (\s ->
    let S h = f (getResult (g s))
        in h (getState (g s)))

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) e1 e2 = Op (Add e1 e2)
  (-) e1 e2 = Op (Sub e1 e2)
  (*) e1 e2 = Op (Mul e1 e2)
  abs e = Op (Abs e)
  signum e = Op (Sgn e)
  fromInteger i = Val (fromIntegral i)

instance Fractional Expr where
  (/) e1 e2 = Op (Div e1 e2)
  fromRational r = Val (fromRational r)

prepend :: a -> [a] -> [a]
prepend h t = h : t

eval :: Expr -> State [Prim Double] Double
eval (Val val) = return val
eval (Op op) = case op of
    Add e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      let res = v1 + v2
      modifyState (prepend (Add v1 v2))
      return res
    Sub e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      let res = v1 - v2
      modifyState (prepend (Sub v1 v2))
      return res
    Mul e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      let res = v1 * v2
      modifyState (prepend (Mul v1 v2))
      return res
    Div e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      let res = v1 / v2
      modifyState (prepend (Div v1 v2))
      return res
    Abs e -> do
      v <- eval e
      let res = abs v
      modifyState (prepend (Abs v))
      return res
    Sgn e -> do
      v <- eval e
      let res = signum v
      modifyState (prepend (Sgn v))
      return res

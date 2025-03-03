module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import           HW4.Types (Annotated (..), Except (Error, Success),
                            Expr (Op, Val), Prim (Abs, Add, Div, Mul, Sgn, Sub))

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES run) = ES (\s ->
    case run s of
        Error e          -> Error e
        Success (a :# e) -> Success (f a :# e))

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES run) = ES (\s ->
    case run s of
        Error e -> Error e
        Success (ES innerRun :# newState) ->
            case innerRun newState of
                Error e'                  -> Error e'
                Success (a :# finalState) -> Success (a :# finalState))

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) (ES f) (ES g) = ES (\s ->
    case f s of
      Error e -> Error e
      Success (func :# s1) ->
        case g s1 of
          Error e           -> Error e
          Success (a :# s2) -> Success (func a :# s2))

instance Monad (ExceptState e s) where
  (>>=) (ES f) g = ES (\s ->
    case f s of
      Error e -> Error e
      Success (a :# s1) ->
        let ES h = g a
        in h s1)

data EvaluationError = DivideByZero
  deriving Show

prepend :: a -> [a] -> [a]
prepend h t = h : t

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val val) = return val
eval (Op op) = case op of
    Add e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      let res = v1 + v2
      modifyExceptState (prepend (Add v1 v2))
      return res
    Sub e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      let res = v1 - v2
      modifyExceptState (prepend (Sub v1 v2))
      return res
    Mul e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      let res = v1 * v2
      modifyExceptState (prepend (Mul v1 v2))
      return res
    Div e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      if v2 == 0 then
         throwExceptState DivideByZero
      else do
        let res = v1 / v2
        modifyExceptState (prepend (Div v1 v2))
        return res
    Abs e -> do
      v <- eval e
      let res = abs v
      modifyExceptState (prepend (Abs v))
      return res
    Sgn e -> do
      v <- eval e
      let res = signum v
      modifyExceptState (prepend (Sgn v))
      return res


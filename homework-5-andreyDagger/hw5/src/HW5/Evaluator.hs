module HW5.Evaluator
  ( eval
  ) where

import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except (throwE)
import           HW5.Base                   (HiError (..), HiExpr (..),
                                             HiFun (..), HiValue (..))

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalImpl expr)

data Arity
  = Unary HiFun
  | Binary HiFun
  | Ternary HiFun

evalImpl :: Monad m => HiExpr -> ExceptT HiError m HiValue
evalImpl (HiExprValue val) = return val
evalImpl (HiExprApply f list) = do
  val <- evalImpl f
  case val of
    HiValueFunction nextFun -> case nextFun of
      HiFunNot            -> evalList (Unary HiFunNot) list
      HiFunDiv            -> evalList (Binary HiFunDiv) list
      HiFunMul            -> evalList (Binary HiFunMul) list
      HiFunAdd            -> evalList (Binary HiFunAdd) list
      HiFunSub            -> evalList (Binary HiFunSub) list
      HiFunAnd            -> evalList (Binary HiFunAnd) list
      HiFunOr             -> evalList (Binary HiFunOr) list
      HiFunLessThan       -> evalList (Binary HiFunLessThan) list
      HiFunGreaterThan    -> evalList (Binary HiFunGreaterThan) list
      HiFunEquals         -> evalList (Binary HiFunEquals) list
      HiFunNotLessThan    -> evalList (Binary HiFunNotLessThan) list
      HiFunNotGreaterThan -> evalList (Binary HiFunNotGreaterThan) list
      HiFunNotEquals      -> evalList (Binary HiFunNotEquals) list
      HiFunIf             -> evalList (Ternary HiFunIf) list
    _ -> throwE HiErrorInvalidFunction

-- lazy!!
evalOr :: Monad m => HiExpr -> HiValue -> ExceptT HiError m HiValue
evalOr b a = case a of
  HiValueBool False -> evalImpl b
  _                 -> return a

-- lazy, too!!
evalAnd :: Monad m => HiExpr -> HiValue -> ExceptT HiError m HiValue
evalAnd b a = case a of
  HiValueBool False -> return a
  _                 -> evalImpl b

evalUnary :: Monad m => HiFun -> HiValue -> ExceptT HiError m HiValue
evalUnary HiFunNot (HiValueBool a) = return (HiValueBool $ not a)
evalUnary _ _                      = throwE HiErrorInvalidArgument

evalBin :: Monad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue

evalBin HiFunLessThan a b       = return (HiValueBool $ a < b)
evalBin HiFunGreaterThan a b    = return (HiValueBool $ a > b)
evalBin HiFunNotLessThan a b    = return (HiValueBool $ a >= b)
evalBin HiFunNotGreaterThan a b = return (HiValueBool $ a <= b)
evalBin HiFunEquals a b         = return (HiValueBool $ a == b)
evalBin HiFunNotEquals a b      = return (HiValueBool $ a /= b)

evalBin fun (HiValueNumber a) (HiValueNumber b) = case fun of
  HiFunMul -> return (HiValueNumber $ a * b)
  HiFunAdd -> return (HiValueNumber $ a + b)
  HiFunSub -> return (HiValueNumber $ a - b)
  HiFunDiv -> case b of
    0 -> throwE HiErrorDivideByZero
    _ -> return (HiValueNumber $ a / b)
  _ -> throwE HiErrorInvalidArgument

evalBin _ _ _                                   = throwE HiErrorInvalidArgument

evalList :: Monad m => Arity -> [HiExpr] -> ExceptT HiError m HiValue
evalList (Unary single) [s]              = evalUnary single =<< evalImpl s
evalList (Binary HiFunAnd) (lhs : [rhs])          = evalAnd rhs =<< evalImpl lhs
evalList (Binary HiFunOr) (lhs : [rhs])           = evalOr rhs =<< evalImpl lhs
evalList (Binary binary) (lhs : [rhs])       = do
  res <- evalImpl lhs
  evalBin binary res =<< evalImpl rhs
evalList (Ternary triple) (lhs : (mhs : [rhs])) = do
  res <- evalImpl lhs
  case triple of
    HiFunIf -> case res of
      HiValueBool a -> (if a then evalImpl mhs else evalImpl rhs)
      _             -> throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument

evalList _ _                         = throwE HiErrorArityMismatch

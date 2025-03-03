module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  ) where

-- | Evaluation errors (invalid arguments, ...)
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

-- | function names (e.g. div, sort, length, ...)
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  deriving (Ord, Eq, Enum)

-- | Expressions (literals, function calls, ...)
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Eq, Ord, Show)

-- | Values (numbers, booleans, strings, ...)
data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  deriving (Eq, Ord, Show)

instance Show HiFun where
    show HiFunDiv            = "div"
    show HiFunMul            = "mul"
    show HiFunAdd            = "add"
    show HiFunSub            = "sub"
    show HiFunNot            = "not"
    show HiFunAnd            = "and"
    show HiFunOr             = "or"
    show HiFunLessThan       = "less-than"
    show HiFunGreaterThan    = "greater-than"
    show HiFunEquals         = "equals"
    show HiFunNotLessThan    = "not-less-than"
    show HiFunNotGreaterThan = "not-greater-than"
    show HiFunNotEquals      = "not-equals"
    show HiFunIf             = "if"

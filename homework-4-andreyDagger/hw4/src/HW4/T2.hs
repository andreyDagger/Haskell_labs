{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  , pEof
  , (<|>)
  ) where

import           Control.Applicative
import           Control.Monad
import           Numeric.Natural     (Natural)

import           Data.Char           (digitToInt, isDigit)
import           HW4.T1              (ExceptState (..))
import           HW4.Types           (Annotated ((:#)), Except (..), Expr (..),
                                      Prim (Add, Div, Mul, Sub))

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES run)) s =
  let initialState = (0, s)
      result = run initialState
  in case result of
    Error e          -> Error e
    Success (a :# _) -> Success a


-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

pCharPredicate :: (Char -> Bool) -> Parser Char
pCharPredicate p = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> if p c then Success (c :# (pos + 1, cs))
              else Error (ErrorAtPos pos)



pCharConcrete :: Char ->           Parser Char
pCharConcrete a = pCharPredicate (== a)

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P pp) (P qq) = P $ ES $ \s ->
    case runES pp s of
      Error _ -> runES qq s
      w       -> w

-- No metohds
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)


parseExpr :: String -> Except ParseError Expr
parseExpr = runP start

start :: Parser Expr
start = pExpr <* pEof

pExpr :: Parser Expr
pExpr = pSpace *> pFactor <**> pExpr1 <* pSpace

pExpr1 :: Parser (Expr -> Expr)
pExpr1 = pSpace *> (op <|> pure id) <* pSpace
  where
    op = fmap (\operation rightExpr f leftExpr -> case operation of
      '+' -> f (Op (Add leftExpr rightExpr))
      '-' -> f (Op (Sub leftExpr rightExpr))
      _   -> undefined) (pCharConcrete '+' <|> pCharConcrete '-') <*> pFactor <*> pExpr1

pFactor :: Parser Expr
pFactor = pSpace *> pTerminalOrExpr <**> pFactor1 <* pSpace

pFactor1 :: Parser (Expr -> Expr)
pFactor1 = pSpace *> (op <|> pure id) <* pSpace
  where
    op = fmap (\operation rightExpr f leftExpr -> case operation of
      '*' -> f (Op (Mul leftExpr rightExpr))
      '/' -> f (Op (Div leftExpr rightExpr))
      _   -> undefined)  (pCharConcrete '*' <|> pCharConcrete '/') <*> pTerminalOrExpr <*> pFactor1

pTerminalOrExpr :: Parser Expr
pTerminalOrExpr = pSpace *> (Val <$> pNumber <|> pSurroundedBy '(' pExpr ')') <* pSpace

pSurroundedBy :: Char -> Parser a -> Char -> Parser a
pSurroundedBy leftChar p rightChar = pSpace *> pCharConcrete leftChar *> pSpace *> p <* pSpace <* pCharConcrete rightChar

pNumber :: Parser Double
pNumber = pSpace *> (fmap concatToDouble pNotEmptyInt <*> pDotIntegerOrEmpty)

pDotIntegerOrEmpty :: Parser String
pDotIntegerOrEmpty = pCharConcrete '.' *> pNotEmptyInt <|> pure []

reversedStringToInt :: String -> Int
reversedStringToInt []    = 0
reversedStringToInt (h:t) = reversedStringToInt t * 10 + digitToInt h

stringToInt :: String -> Int
stringToInt s = reversedStringToInt (reverse s)

concatToDouble :: String -> String -> Double
concatToDouble beforeDot afterDot =
  let intValue = fromIntegral (stringToInt beforeDot)
      fracValue = fromIntegral (stringToInt afterDot) / (10^length afterDot)
  in intValue + fracValue

pNotEmptyInt :: Parser String
pNotEmptyInt = some (pCharPredicate Data.Char.isDigit)

pSpace :: Parser String
pSpace = many (pCharConcrete ' ' <|> pCharConcrete '\t' <|> pCharConcrete '\n' <|> pCharConcrete '\r')

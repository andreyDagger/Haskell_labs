module HW5.Parser
  ( HW5.Parser.parse
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer
import Control.Monad.Combinators.Expr
import Control.Monad.Identity (Identity)
import Control.Monad
import Data.Void (Void)
import HW5.Base (HiExpr (..), HiFun (..), HiValue (..))

parserIdentity :: String
parserIdentity = ""

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser parser parserIdentity

parser :: (Parsec Void String) HiExpr
parser = expr <* eof

expr :: (Parsec Void String) HiExpr
expr = makeExprParser termParser operatorTable

whitespaces :: (Parsec Void String) ()
whitespaces = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

termParser :: Parsec Void String HiExpr
termParser = do
  _ <- whitespaces
  value <- try pHiValue <|> pExprBrackets
  result <- pHiArgs value
  _ <- whitespaces
  return result

pHiArgs :: HiExpr -> (Parsec Void String) HiExpr
pHiArgs inExpr = try (
  (pHiArgs >=> return) =<< whitespaces *> fmap (HiExprApply inExpr) (pHiArgsVals '(' expr ')')
  ) <|> pure inExpr

pExprBrackets :: (Parsec Void String) HiExpr
pExprBrackets = do
  _ <- char '(' >> whitespaces
  res <- expr
  _ <- whitespaces >> char ')'
  return res

pHiArgsVals :: Char -> (Parsec Void String) a -> Char -> (Parsec Void String) [a]
pHiArgsVals start p end = between (char start  <* whitespaces) (char end) (p  `sepBy` (char ',' <* whitespaces))

pHiValue :: (Parsec Void String) HiExpr
pHiValue = fmap HiExprValue (try pHiValueFunction <|> try pHiValueNumber <|>  try pHiBool)

pHiValueNumber :: (Parsec Void String) HiValue
pHiValueNumber = fmap (HiValueNumber . toRational) (signed whitespaces scientific)

pHiBool :: (Parsec Void String) HiValue
pHiBool = choice
  [HiValueBool True <$ string "true"
  , HiValueBool False <$ string "false"]

pHiValueFunction :: (Parsec Void String) HiValue
pHiValueFunction = fmap HiValueFunction (choice (map (\x -> x <$ string (show x)) [HiFunDiv ..]))

createExpr :: HiFun -> HiExpr -> HiExpr -> HiExpr
createExpr fun a b = HiExprApply (HiExprValue $ HiValueFunction fun) [a, b]

createOperator :: MonadParsec e s m => (m (a -> a -> a) -> Operator m a) -> (a -> a -> a) -> Tokens s -> Operator m a
createOperator inf f name = inf $ try (f <$ string name)

operatorTable :: [[Operator (ParsecT Void String Identity) HiExpr]]
operatorTable =
  [ [ InfixL . try $ createExpr HiFunDiv <$ char '/' <* notFollowedBy (char '=')
    , createOperator InfixL (createExpr HiFunMul)            "*"]
  , [ createOperator InfixL (createExpr HiFunSub)            "-"
    , createOperator InfixL (createExpr HiFunAdd)            "+"]
  , [ createOperator InfixN (createExpr HiFunEquals)         "=="
    , createOperator InfixN (createExpr HiFunNotEquals)      "/="
    , createOperator InfixN (createExpr HiFunNotGreaterThan) "<="
    , createOperator InfixN (createExpr HiFunNotLessThan)    ">="
    , createOperator InfixN (createExpr HiFunLessThan)       "<"
    , createOperator InfixN (createExpr HiFunGreaterThan)    ">"]
  , [createOperator InfixR (createExpr HiFunAnd)             "&&"]
  , [createOperator InfixR (createExpr HiFunOr)              "||"]]
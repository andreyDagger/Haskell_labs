module HW5.Pretty
  ( prettyValue
  ) where

import           Data.Ratio                    (denominator, numerator)
import           Data.Scientific               (fromRationalRepetendUnlimited,
                                                toRealFloat)
import           HW5.Base                      (HiValue (..))
import           Numeric                       (showFFloat)
import           Prettyprinter                 (Doc, Pretty (pretty))
import           Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber rational) = pretty ( case (numerator rational, denominator rational) of
  (n, 1) -> show n
  (n, d) -> prettyRational (rational, n, d))
prettyValue (HiValueFunction fun)    = pretty (show fun)
prettyValue (HiValueBool True)          = pretty "true"
prettyValue (HiValueBool False)          = pretty "false"

isInfiniteFraction :: Integer -> Bool
isInfiniteFraction denom = maximumDivideBy (maximumDivideBy denom 2) 5 == 1

maximumDivideBy :: Integer -> Integer -> Integer
maximumDivideBy x y = if x `mod` y == 0 then maximumDivideBy (x `div` y) y else x

prettyRational :: (Rational, Integer, Integer) -> String
prettyRational (x, n, d)
  | isInfiniteFraction d = case fromRationalRepetendUnlimited x of
    (s, Nothing) -> showFFloat Nothing (toRealFloat s :: Double) ""
    (_, Just _)  -> helper n d
  | otherwise = helper n d

helper :: Integer -> Integer -> String
helper n d = case quotRem n d of
    (0, r) -> show r ++ "/" ++ show d
    (q, r) -> show q ++ sign r ++ (show (abs r) ++ "/" ++ show d)

sign :: Integer -> String
sign x
  | x > 0 = " + "
  | otherwise = " - "

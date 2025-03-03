module Main (main) where

import HW1.Exam(A(..))
import Prelude (putStrLn)

main :: IO ()
main = do
    putStrLn (foo (Bar 1))
    putStrLn (bar (Foo 1))
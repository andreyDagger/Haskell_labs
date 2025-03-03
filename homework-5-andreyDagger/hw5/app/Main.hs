module Main
  ( main
  ) where

import           Control.Monad.IO.Class
import           HW5.Base
import           HW5.Evaluator
import           HW5.Parser
-- import           HW5.Pretty
import           System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "hi> "
  case minput of
    Just ""    -> loop
    Just ":q"  -> return ()
    Just input -> process input
    Nothing    -> return ()

process :: String -> InputT IO ()
process input = do
  pf <- getExternalPrint
  let parseResult = parse input
  case parseResult of
    Left parseError -> liftIO $ pf $ show parseError
    Right parsed -> do
      let evaluateResult = eval parsed :: IO (Either HiError HiValue)
      r <- liftIO evaluateResult
      liftIO $ case r of
        Left _ -> pf (show parseResult)
        Right _   -> pf (show parseResult)
  loop

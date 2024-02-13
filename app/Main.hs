module Main (main) where

import Eval
import Lib
import Parser
import Primitives
import Types
import Control.Monad.State
import System.IO

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case (evalState (parseAndEval input) $ env) of
    Just expr -> putStrLn $ show expr
    _         -> putStrLn "error"

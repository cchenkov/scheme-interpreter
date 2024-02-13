module Main (main) where

import Eval
import Lib
import Parser
import Primitives
import Types
import Control.Monad.State
import Data.Maybe
import System.IO
import System.Exit

exec :: String -> Stateful IO ()
exec input =
  parseAndEval input
  >>= lift
  . putStrLn
  . show
  . fromMaybe (String "error")

main :: IO ()
main = do
  evalStateT (forever run) env
  where
    run = do
      lift $ putStr "> " >> hFlush stdout
      input <- lift getLine
      when (input == "(exit)") $ lift exitSuccess
      exec input

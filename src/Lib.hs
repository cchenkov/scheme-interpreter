module Lib where

import Eval
import Parser
import Primitives
import Types
import Control.Monad.State

parseAndEval :: String -> Stateful IO (Maybe Expr)
parseAndEval input = do
  let expr = parse input
  case expr of
    Just expr' -> eval expr'
    _          -> pure Nothing

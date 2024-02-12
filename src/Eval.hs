module Eval where

import Types
import Primitives
import Control.Monad.State
import Control.Applicative (liftA2)
import Data.Maybe

extendContext :: (Ident, Expr) -> State Context ()
extendContext pair = do
  ctx <- get
  put (pair : ctx)
  pure ()

eval :: Expr -> State Context (Maybe Expr)

-- number
eval val@(Number _) = pure $ Just val

-- bool
eval val@(Bool _) = pure $ Just val

-- variable
eval (Var i) = do
  ctx <- get
  pure $ lookup i ctx

-- add
eval (List [Var "+", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 add x' y'

-- sub
eval (List [Var "-", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 sub x' y'

-- mult
eval (List [Var "*", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 mult x' y'

-- divide
eval (List [Var "/", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 divide x' y'

-- modulo
eval (List [Var "mod", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 modulo x' y'

-- quotient
eval (List [Var "quotient", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 quotient x' y'

-- remainder
eval (List [Var "remainder", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 remainder x' y'

-- equal
eval (List [Var "=", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 equal x' y'

-- lessThan
eval (List [Var "<", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 lessThan x' y'

-- greaterThan
eval (List [Var ">", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 greaterThan x' y'

-- notEqual
eval (List [Var "/=", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 notEqual x' y'

-- lessThanOrEqual
eval (List [Var "<=", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 lessThanOrEqual x' y'

-- greaterThanOrEqual
eval (List [Var ">=", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 greaterThanOrEqual x' y'

-- -- car
-- eval (List [Var "car", List xs]) =
--   pure $ Just $ car $ List $ xs

-- -- cdr
-- eval (List [Var "cdr", List xs]) =
--   pure $ Just $ cdr $ List $ xs

-- if
eval (List [Var "if", cex, tex, eex]) = do
  cex' <- eval cex
  case cex' of
    Just (Bool True) -> eval tex
    _                -> eval eex

-- cond
eval (List [Var "cond",  List [Var "else", eex]]) = eval eex
eval (List (Var "cond" : List [cex, tex] : rest)) = do
   cex' <- eval cex
   case cex' of
     Just (Bool True) -> eval tex
     _                -> eval (List (Var "cond" : rest))

-- lambda
eval (List (Var "lambda" : List ids : exprs)) = do
  pure $ Just $ Func (map showExpr ids) (last exprs)

-- func
eval (List (func : args)) = do
  args' <- mapM (eval) args
  func' <- eval func
  case func' of
    Just (Func ids expr) -> do
      mapM extendContext (zip ids (catMaybes args'))
      eval expr
    _                    -> pure Nothing

-- discard
eval func@(Func _ _) = pure $ Just func
eval list@(List _)   = pure $ Just list

module Eval where

import Types
import Primitives
import Control.Monad.State
import Control.Applicative (liftA2)
import Data.Maybe

type Stateful = StateT Context

lookupContext :: Ident -> Stateful IO (Maybe Expr)
lookupContext i = do
  ctx <- get
  pure $ lookup i ctx

extendContext :: (Ident, Expr) -> Stateful IO ()
extendContext pair = do
  ctx <- get
  put (pair : ctx)
  pure ()

eval :: Expr -> Stateful IO (Maybe Expr)

-- string
eval val@(String _) = pure $ Just val

-- number
eval val@(Number _) = pure $ Just val

-- bool
eval val@(Bool _) = pure $ Just val

-- variable
eval (Var i) = lookupContext i

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

-- define
eval (List [Var "define", Var i, expr]) = do
  val <- eval expr
  case val of
    Just val' -> do
      extendContext (i, val')
      pure val
    _         -> pure Nothing

eval (List (Var "define" : List (Var i : ids) : exprs)) = do
  let func = Func (map showExpr ids) (last exprs)
  extendContext (i, func)
  pure $ Just func

-- lambda
eval (List (Var "lambda" : List ids : exprs)) = do
  pure $ Just $ Func (map showExpr ids) (last exprs)

-- func | primitive
eval (List (func : args)) = do
  args' <- mapM eval args
  func' <- eval func
  let justArgs = catMaybes args'
  if length args /= length justArgs
    then pure Nothing
      else case func' of
      Just (Func ids expr)  -> do
        mapM_ extendContext (zip ids justArgs)
        eval expr
      Just (Primitive prim) -> pure $ prim justArgs
      _                     -> pure Nothing

-- discard
eval primexpr@(Primitive _) = pure $ Just primexpr
eval funcexpr@(Func _ _)    = pure $ Just funcexpr
eval listexpr@(List _)      = pure $ Just listexpr

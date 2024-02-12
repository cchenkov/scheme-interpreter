module Eval where

import Types
import Primitives
import Control.Monad.State
import Control.Applicative (liftA2)
import Data.Maybe

lookupContext :: Ident -> State Context (Maybe Expr)
lookupContext i = do
  ctx <- get
  pure $ lookup i ctx

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

-- lambda
eval (List (Var "lambda" : List ids : exprs)) = do
  pure $ Just $ Func (map showExpr ids) (last exprs)

-- func | primitive
eval (List (func : args)) = do
  args' <- mapM eval args
  func' <- eval func
  if length args /= length (catMaybes args')
    then pure Nothing
      else case func' of
      Just (Func ids expr)  -> do
        mapM_ extendContext (zip ids (catMaybes args'))
        eval expr
      Just (Primitive prim) -> pure $ prim (catMaybes args')
      Just x                -> pure $ Just $ List (x : catMaybes args')
      _                     -> pure Nothing

-- discard
eval primexpr@(Primitive _) = pure $ Just primexpr
eval funcexpr@(Func _ _)    = pure $ Just funcexpr
eval listexpr@(List _)      = pure $ Just listexpr

module Eval where

import Control.Monad.State
import Control.Applicative (liftA2)
import Data.Maybe

type Ident = String

type Context = [(Ident, Expr)]

data Expr = Var Ident
          | Number Integer
          | Bool Bool
          | List [Expr]
          | Func [Ident] Expr

plus :: Expr -> Expr -> Expr
plus (Number x) (Number y) = Number (x + y)
plus _ _ = undefined

minus :: Expr -> Expr -> Expr
minus (Number x) (Number y) = Number (x - y)
minus _ _ = undefined

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

-- plus
eval (List [Var "+", x, y]) = do
  x'  <- eval x
  y'  <- eval y
  pure $ liftA2 plus x' y'

-- minus
eval (List [Var "-", x, y]) = do
  x' <- eval x
  y' <- eval y
  pure $ liftA2 minus x' y'

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

showExpr :: Expr -> String
showExpr (Var i) = i
showExpr (Number n) = show n
showExpr (Bool True) = "#t"
showExpr (Bool False) = "#f"
showExpr (List xs) = "(" ++ unwords (map showExpr xs) ++ ")"
showExpr (Func ids expr) = "(lambda (" ++ unwords ids ++ ") " ++ showExpr expr ++ ")"

instance Show Expr where show = showExpr

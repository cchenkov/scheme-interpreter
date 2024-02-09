module Lib where

import Prelude hiding (lookup)
import Data.Maybe

type Ident = String

type Context = [(Ident, Value)]

data Value = Number Integer
           | Closure [Ident] Expr Context
           deriving (Show)

data Expr = Var Ident
          | Val Integer
          | Plus Expr Expr
          | Minus Expr Expr
          | If Expr Expr Expr
          | Lambda [Ident] Expr
          | Apply Expr [Expr]
          deriving (Show)

maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen Nothing _ = Nothing
maybeAndThen (Just x) f = f x

ifNumberThen :: Value -> (Integer -> Maybe Value) -> Maybe Value
ifNumberThen (Number n) f = f n
ifNumberThen _ _ = Nothing

lookup :: String -> Context -> Maybe Value 
lookup _ [] = Nothing
lookup x ((k, v) : r) = 
  if x == k
    then Just v
    else lookup x r

apply :: Value -> [Value] -> Maybe Value
apply (Closure is expr ctx) xs = eval (zip is xs ++ ctx) expr
apply _ _ = Nothing

eval :: Context -> Expr -> Maybe Value 
eval _ (Val n) = Just $ Number n
eval ctx (Var i) = lookup i ctx
eval ctx (Plus x y) = 
  eval ctx x `maybeAndThen` \x' ->
  x' `ifNumberThen` \xn ->
  eval ctx y `maybeAndThen` \y' ->
  y' `ifNumberThen` \yn ->
  Just $ Number (xn + yn)
eval ctx (Minus x y) = 
  eval ctx x `maybeAndThen` \x' ->
  x' `ifNumberThen` \xn ->
  eval ctx y `maybeAndThen` \y' ->
  y' `ifNumberThen` \yn ->
  Just $ Number (xn - yn)
eval ctx (If cex tex eex) =
  eval ctx cex `maybeAndThen` \cex' ->
    cex' `ifNumberThen` \cexn ->
      if cexn == 0
        then eval ctx tex
        else eval ctx eex
eval ctx (Lambda is expr) = Just $ Closure is expr ctx
eval ctx (Apply f xs) = 
  mapM (eval ctx) xs >>= \mxs -> 
  eval ctx f `maybeAndThen` \f' ->
  apply f' mxs
-- eval ctx (Apply f xs) = 
--   eval ctx f `maybeAndThen` \f' ->
--   apply f' $ catMaybes $ map (eval ctx) xs

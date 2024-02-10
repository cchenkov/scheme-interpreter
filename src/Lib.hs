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

type Parser a = String -> [(a, String)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = \inp -> concat [f v rest | (v, rest) <- p inp]

result :: a -> Parser a
result v = \inp -> [(v, inp)]

zero :: Parser a
zero = \_ -> []

item :: Parser Char
item = 
  \inp -> case inp of
    [] -> []
    (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = 
  item `bind` \x ->
    if p x
      then result x
      else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

lookup :: String -> Context -> Maybe Value 
lookup _ [] = Nothing
lookup x ((k, v) : r) 
  | x == k = Just v
  | otherwise = lookup x r

apply :: Value -> [Value] -> Maybe Value
apply (Closure is expr ctx) xs = eval (zip is xs ++ ctx) expr
apply _ _ = Nothing

plus :: Value -> Value -> Maybe Value
plus (Number x) (Number y) = Just $ Number (x + y)
plus _ _ = Nothing

minus :: Value -> Value -> Maybe Value
minus (Number x) (Number y) = Just $ Number (x - y)
minus _ _ = Nothing

ifcond :: Value -> Maybe Value -> Maybe Value -> Maybe Value
ifcond (Number 0) tex _ = tex
ifcond (Number _) _ eex = eex
ifcond _ _ _ = Nothing

eval :: Context -> Expr -> Maybe Value 
eval _ (Val n) = Just $ Number n
eval ctx (Var i) = lookup i ctx
eval ctx (Plus x y) = 
  eval ctx x >>= \x' ->
  eval ctx y >>= \y' ->
  plus x' y'
eval ctx (Minus x y) = 
  eval ctx x >>= \x' ->
  eval ctx y >>= \y' ->
  minus x' y'
eval ctx (If cex tex eex) =
  eval ctx cex >>= \cex' ->
  ifcond cex' (eval ctx tex) (eval ctx eex)
eval ctx (Lambda is expr) = Just $ Closure is expr ctx
eval ctx (Apply f xs) = 
  mapM (eval ctx) xs >>= \mxs -> 
  eval ctx f >>= \f' ->
  apply f' mxs

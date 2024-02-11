module Eval where

type Ident = String

type Context = [(Ident, Expr)]

-- data Value = Number Integer
--            | ListVal [Value]
--            | Closure [Ident] Expr Context

data Expr = Var Ident
          | Number Integer
          | Bool Bool
          | List [Expr]
          | Plus Expr Expr
          | Minus Expr Expr
          | Lambda [Ident] Expr
          | Apply Expr [Expr]

-- fmap show $ eval [] (Apply (Lambda ["x", "y"] (Plus (Var "x") (Var "y"))) [(Val 1), (Val 2)])
-- apply :: Value -> [Value] -> Maybe Value
-- apply (Closure ids expr ctx) xs = eval (zip ids xs ++ ctx) expr
-- apply _ _ = Nothing

plus :: Expr -> Expr -> Maybe Expr
plus (Number x) (Number y) = Just $ Number (x + y)
plus _ _ = Nothing

minus :: Expr -> Expr -> Maybe Expr
minus (Number x) (Number y) = Just $ Number (x - y)
minus _ _ = Nothing

eval :: Context -> Expr -> Maybe Expr

-- number
eval _   val@(Number _) = Just val

-- bool
eval _   val@(Bool _) = Just val

-- variable
eval ctx (Var i) = lookup i ctx

-- plus
eval ctx (Plus x y) = do
  x' <- eval ctx x
  y' <- eval ctx y
  plus x' y'

-- minus
eval ctx (Minus x y) = do
  x' <- eval ctx x
  y' <- eval ctx y
  minus x' y'

-- if
eval ctx (List [Var "if", cex, tex, eex]) = do
  cex' <- eval ctx cex
  case cex' of
    Bool True -> eval ctx tex
    _         -> eval ctx eex

-- cond
eval ctx (List [Var "cond",  List [Var "else", tex]]) = eval ctx tex
eval ctx (List (Var "cond" : List [cex, tex] : rest)) = do
   cex' <- eval ctx cex
   case cex' of
     Bool True -> eval ctx tex
     _         -> eval ctx (List (Var "cond" : rest))

-- lambda
-- eval ctx (Lambda ids expr) = Just $ Closure ids expr ctx
eval _ (Lambda _ _) = Nothing

-- apply
-- eval ctx (Apply f xs) = do
--   xs' <- mapM (eval ctx) xs
--   f'  <- eval ctx f
--   apply f' xs'
eval _ (Apply _ _) = Nothing

-- list
-- eval ctx (List xs) = do
--   xs' <- mapM (eval ctx) xs
--   Just $ ListVal xs'
eval _ (List _) = Nothing

showExpr :: Expr -> String
showExpr (Var i) = i
showExpr (Number n) = show n
showExpr (Bool True) = "#t"
showExpr (Bool False) = "#f"
showExpr (List xs) = "(" ++ unwords (map showExpr xs) ++ ")"
showExpr (Lambda ids _ ) = "(lambda (" ++ unwords ids ++ ") ...)"
showExpr (Plus _ _) = "not supported"
showExpr (Minus _ _) = "not supported"
showExpr (Apply _ _) = "not supported"

instance Show Expr where show = showExpr

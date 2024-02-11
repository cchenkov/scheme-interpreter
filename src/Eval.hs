module Eval where

type Ident = String

type Context = [(Ident, Expr)]

data Expr = Var Ident
          | Number Integer
          | Bool Bool
          | List [Expr]
          | Func [Ident] Expr Context

apply :: Expr -> [Expr] -> Maybe Expr
apply (Func ids expr ctx) args = eval (zip ids args ++ ctx) expr
apply _ _ = Nothing

plus :: Expr -> Expr -> Maybe Expr
plus (Number x) (Number y) = Just $ Number (x + y)
plus _ _ = Nothing

minus :: Expr -> Expr -> Maybe Expr
minus (Number x) (Number y) = Just $ Number (x - y)
minus _ _ = Nothing

eval :: Context -> Expr -> Maybe Expr

-- number
eval _ val@(Number _) = Just val

-- bool
eval _ val@(Bool _) = Just val

-- variable
eval ctx (Var i) = lookup i ctx

-- plus
eval ctx (List [Var "+", x, y]) = do
  x' <- eval ctx x
  y' <- eval ctx y
  plus x' y'

-- minus
eval ctx (List [Var "-", x, y]) = do
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
eval ctx (List (Var "lambda" : List ids : exprs)) =
  Just $ Func (map showExpr ids) (last exprs) ctx

-- func
eval ctx (List (func : args)) = do
  args' <- mapM (eval ctx) args
  func' <- eval ctx func
  apply func' args'

-- discard
eval _ func@(Func _ _ _) = Just func

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
showExpr (Func ids expr _) = "(lambda (" ++ unwords ids ++ ") " ++ showExpr expr ++ ")"

instance Show Expr where show = showExpr

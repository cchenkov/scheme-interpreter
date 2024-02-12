module Types where

type Ident = String

type Context = [(Ident, Expr)]

data Expr = Var Ident
          | Number Integer
          | Bool Bool
          | List [Expr]
          | Func [Ident] Expr
          | Primitive ([Expr] -> Maybe Expr)

showExpr :: Expr -> String
showExpr (Var i) = i
showExpr (Number n) = show n
showExpr (Bool True) = "#t"
showExpr (Bool False) = "#f"
showExpr (List xs) = "(" ++ unwords (map showExpr xs) ++ ")"
showExpr (Func ids expr) = "(lambda (" ++ unwords ids ++ ") " ++ showExpr expr ++ ")"
showExpr (Primitive _) = "<primitive>"

instance Show Expr where show = showExpr

module Lib where

import Prelude hiding (lookup)
import Control.Applicative (Alternative, empty, (<|>), many, some)

type Ident = String

type Context = [(Ident, Value)]

data Value = Number Integer
           | ListVal [Value]
           | Closure [Ident] Expr Context

data Expr = Var Ident
          | Val Integer
          | List [Expr]
          | Plus Expr Expr
          | Minus Expr Expr
          | If Expr Expr Expr
          | Cond [Expr]
          | Lambda [Ident] Expr
          | Apply Expr [Expr]
          deriving (Show)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser x) = Parser $ \s -> do
    (x', s') <- x s
    return (f x', s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)

  (Parser x) <*> (Parser y) = Parser $ \s -> do
    (x', s')  <- x s
    (y', s'') <- y s'
    return (x' y', s'')

instance Monad Parser where
  (Parser x) >>= f = Parser $ \s -> do
    (x', s') <- x s
    runParser (f x') s'

instance MonadFail Parser where
  fail _ = Parser $ const Nothing

instance Alternative Parser where
  empty = fail ""

  (Parser x) <|> (Parser y) = Parser $ \s -> 
    case x s of
      Just v  -> Just v
      Nothing -> y s

next :: Parser Char
next = Parser nextP
  where
    nextP []     = Nothing
    nextP (x:xs) = Just (x, xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- next
  if p x
    then pure x
    else fail ""

oneOf :: [Char] -> Parser Char
oneOf xs = satisfy (`elem` xs)

noneOf :: [Char] -> Parser Char
noneOf xs = satisfy (`notElem` xs)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = mapM char

space :: Parser Char
space  =  char ' '
      <|> char '\n'
      <|> char '\r'
      <|> char '\t'

spaces :: Parser [Char]
spaces = many space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

letter :: Parser Char
letter = oneOf $ ['A' .. 'Z'] ++ ['a' .. 'z']

digit :: Parser Char
digit = oneOf ['0' .. '9']

nat :: Parser Integer
nat = do
  n <- some digit
  pure $ read n

int :: Parser Integer
int = do
  char '-'
  n <- nat
  pure (-n)
  <|> nat

nfloat :: Parser Float
nfloat = do
  x <- some digit
  char '.'
  y <- some digit
  pure $ read $ x ++ "." ++ y

float :: Parser Float
float = do
  char '-'
  n <- nfloat
  pure (-n)
  <|> nfloat

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy px psep =
  many $ do
    x <- px
    psep
    pure x
    <|> px

parseExpr :: Parser Expr
parseExpr  =  parseVar
          <|> parseInt
          <|> parseIf
          <|> parseCond
          <|> parseList

parseVar :: Parser Expr
parseVar = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  pure $ Var (first : rest)

parseInt :: Parser Expr
parseInt = Val <$> int

parseIf :: Parser Expr
parseIf = do
  char '('
  string "if"
  spaces
  cex <- parseExpr
  spaces
  tex <- parseExpr
  spaces
  eex <- parseExpr
  spaces
  char ')'
  pure $ If cex tex eex

parseCond :: Parser Expr
parseCond = do
  char '('
  string "cond"
  spaces
  cexs <- sepBy parseExpr spaces
  char ')'
  pure $ Cond cexs

parseList :: Parser Expr
parseList = do
  char '('
  xs <- sepBy parseExpr spaces
  char ')'
  pure $ List xs

lookup :: String -> Context -> Maybe Value 
lookup _ [] = Nothing
lookup x ((k, v) : r) 
  | x == k = Just v
  | otherwise = lookup x r

apply :: Value -> [Value] -> Maybe Value
apply (Closure ids expr ctx) xs = eval (zip ids xs ++ ctx) expr
apply _ _ = Nothing

plus :: Value -> Value -> Maybe Value
plus (Number x) (Number y) = Just $ Number (x + y)
plus _ _ = Nothing

minus :: Value -> Value -> Maybe Value
minus (Number x) (Number y) = Just $ Number (x - y)
minus _ _ = Nothing

eval :: Context -> Expr -> Maybe Value 
eval _   (Val n) = Just $ Number n
eval ctx (Var i) = lookup i ctx
eval ctx (List xs) = do
  xs' <- mapM (eval ctx) xs
  Just $ ListVal xs'
eval ctx (Plus x y) = do
  x' <- eval ctx x
  y' <- eval ctx y
  plus x' y'
eval ctx (Minus x y) = do
  x' <- eval ctx x
  y' <- eval ctx y
  minus x' y'
eval ctx (If cex tex eex) = do
  cex' <- eval ctx cex
  case cex' of
    Number 0 -> eval ctx tex
    _        -> eval ctx eex
eval _   (Cond []) = Nothing
eval ctx (Cond [List [Var "else", tex]]) = eval ctx tex
eval ctx (Cond (List [cex, tex] : rest)) = do
   cex' <- eval ctx cex
   case cex' of
     Number 0 -> eval ctx tex
     _        -> eval ctx (Cond rest)
eval _   (Cond _) = Nothing
eval ctx (Lambda ids expr) = Just $ Closure ids expr ctx
eval ctx (Apply f xs) = do
  xs' <- mapM (eval ctx) xs
  f'  <- eval ctx f
  apply f' xs'

showValue :: Value -> String
showValue (Number n) = show n
showValue (ListVal xs) = "(" ++ unwords (map showValue xs) ++ ")"
showValue (Closure ids _ _) = "(lambda (" ++ unwords ids ++ ") ...)"

instance Show Value where show = showValue

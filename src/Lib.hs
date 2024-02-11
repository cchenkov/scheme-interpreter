module Lib where

import Prelude hiding (lookup)
import Control.Applicative (Alternative, empty, (<|>), many, some)

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
  fail _ = Parser $ \_ -> Nothing

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

parseInt :: Parser Expr
parseInt = do
  n <- int
  pure $ Val n

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

ifcond :: Value -> Value -> Value -> Maybe Value
ifcond (Number 0) tex _ = Just $ tex
ifcond (Number _) _ eex = Just $ eex
ifcond _ _ _ = Nothing

eval :: Context -> Expr -> Maybe Value 
eval _   (Val n) = Just $ Number n
eval ctx (Var i) = lookup i ctx
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
  tex' <- eval ctx tex
  eex' <- eval ctx eex
  ifcond cex' tex' eex'
eval ctx (Lambda is expr) = Just $ Closure is expr ctx
eval ctx (Apply f xs) = do
  mxs <- mapM (eval ctx) xs
  f'  <- eval ctx f
  apply f' mxs

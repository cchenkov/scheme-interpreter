module Parser where

import Types
import Control.Applicative (Alternative, empty, (<|>), many, some)

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
parseExpr  =  parseBool
          <|> parseVar
          <|> parseInt
          <|> parseList

parseBool :: Parser Expr
parseBool = parseTrue <|> parseFalse

parseTrue :: Parser Expr
parseTrue = do
  string "#t"
  pure (Bool True)

parseFalse :: Parser Expr
parseFalse = do
  string "#f"
  pure (Bool False)

parseVar :: Parser Expr
parseVar = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  pure $ Var (first : rest)

parseInt :: Parser Expr
parseInt = Number <$> int

parseList :: Parser Expr
parseList = do
  char '('
  xs <- sepBy parseExpr spaces
  char ')'
  pure $ List xs

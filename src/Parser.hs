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
module Primitives where

import Types

env :: Context
env = [("+",         Primitive add),
       ("-",         Primitive sub),
       ("*",         Primitive mult),
       ("/",         Primitive divide),
       ("mod",       Primitive modulo),
       ("quotient",  Primitive quotient),
       ("remainder", Primitive remainder),
       ("=",         Primitive equal),
       ("<",         Primitive lessThan),
       (">",         Primitive greaterThan),
       ("/=",        Primitive notEqual),
       ("<=",        Primitive lessThanOrEqual),
       (">=",        Primitive greaterThanOrEqual)]

add :: [Expr] -> Maybe Expr
add [Number x, Number y] = Just $ Number (x + y)
add _                    = Nothing

sub :: [Expr] -> Maybe Expr
sub [Number x, Number y] = Just $ Number (x - y)
sub _                    = Nothing

mult :: [Expr] -> Maybe Expr
mult [Number x, Number y] = Just $ Number (x * y)
mult _                    = Nothing

divide :: [Expr] -> Maybe Expr
divide [Number x, Number y] = Just $ Number (x `div` y)
divide _                    = Nothing

modulo :: [Expr] -> Maybe Expr
modulo [Number x, Number y] = Just $ Number (x `mod` y)
modulo _                    = Nothing

quotient :: [Expr] -> Maybe Expr
quotient [Number x, Number y] = Just $ Number (x `quot` y)
quotient _                    = Nothing

remainder :: [Expr] -> Maybe Expr
remainder [Number x, Number y] = Just $ Number (x `rem` y)
remainder _                    = Nothing

equal :: [Expr] -> Maybe Expr
equal [Number x, Number y] = Just $ Bool (x == y)
equal _                    = Nothing

lessThan :: [Expr] -> Maybe Expr
lessThan [Number x, Number y] = Just $ Bool (x < y)
lessThan _                    = Nothing

greaterThan :: [Expr] -> Maybe Expr
greaterThan [Number x, Number y] = Just $ Bool (x > y)
greaterThan _                    = Nothing

notEqual :: [Expr] -> Maybe Expr
notEqual [Number x, Number y] = Just $ Bool (x /= y)
notEqual _                    = Nothing

lessThanOrEqual :: [Expr] -> Maybe Expr
lessThanOrEqual [Number x, Number y] = Just $ Bool (x <= y)
lessThanOrEqual _                    = Nothing

greaterThanOrEqual :: [Expr] -> Maybe Expr
greaterThanOrEqual [Number x, Number y] = Just $ Bool (x >= y)
greaterThanOrEqual _                    = Nothing

car :: Expr -> Expr
car (List (x : _)) = x
car _              = undefined

cdr :: Expr -> Expr
cdr (List (_ : xs)) = List xs
cdr _               = undefined

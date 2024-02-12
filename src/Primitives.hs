module Primitives where

import Types

add :: Expr -> Expr -> Expr
add (Number x) (Number y) = Number (x + y)
add _          _          = undefined

sub :: Expr -> Expr -> Expr
sub (Number x) (Number y) = Number (x - y)
sub _          _          = undefined

mult :: Expr -> Expr -> Expr
mult (Number x) (Number y) = Number (x * y)
mult _          _          = undefined

divide :: Expr -> Expr -> Expr
divide (Number x) (Number y) = Number (x `div` y)
divide _          _          = undefined

modulo :: Expr -> Expr -> Expr
modulo (Number x) (Number y) = Number (x `mod` y)
modulo _          _          = undefined

quotient :: Expr -> Expr -> Expr
quotient (Number x) (Number y) = Number (x `quot` y)
quotient _          _          = undefined

remainder :: Expr -> Expr -> Expr
remainder (Number x) (Number y) = Number (x `rem` y)
remainder _          _          = undefined

equal :: Expr -> Expr -> Expr
equal (Number x) (Number y) = Bool (x == y)
equal _          _          = undefined

lessThan :: Expr -> Expr -> Expr
lessThan (Number x) (Number y) = Bool (x < y)
lessThan _          _          = undefined

greaterThan :: Expr -> Expr -> Expr
greaterThan (Number x) (Number y) = Bool (x > y)
greaterThan _          _          = undefined

notEqual :: Expr -> Expr -> Expr
notEqual (Number x) (Number y) = Bool (x /= y)
notEqual _          _          = undefined

lessThanOrEqual :: Expr -> Expr -> Expr
lessThanOrEqual (Number x) (Number y) = Bool (x <= y)
lessThanOrEqual _          _          = undefined

greaterThanOrEqual :: Expr -> Expr -> Expr
greaterThanOrEqual (Number x) (Number y) = Bool (x >= y)
greaterThanOrEqual _          _          = undefined

car :: Expr -> Expr
car (List (x : _)) = x
car _              = undefined

cdr :: Expr -> Expr
cdr (List (_ : xs)) = List xs
cdr _               = undefined

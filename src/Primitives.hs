module Primitives where

import Types

env :: Context
env = [("=",         Primitive $ boolOp (==)),
       ("<",         Primitive $ boolOp (<)),
       (">",         Primitive $ boolOp (>)),
       ("/=",        Primitive $ boolOp (/=)),
       ("<=",        Primitive $ boolOp (<=)),
       (">=",        Primitive $ boolOp (>=)),
       ("+",         Primitive $ numericOp (+)),
       ("-",         Primitive $ numericOp (-)),
       ("*",         Primitive $ numericOp (*)),
       ("/",         Primitive $ numericOp (div)),
       ("modulo",    Primitive $ binaryNumericOp (mod)),
       ("quotient",  Primitive $ binaryNumericOp (quot)),
       ("remainder", Primitive $ binaryNumericOp (rem))]

numericOp :: (Integer -> Integer -> Integer) -> [Expr] -> Maybe Expr
numericOp _  [] = Nothing
numericOp op xs =
    let numbers = [x | Number x <- xs] in
    if length xs /= length numbers
        then Nothing
        else case numbers of
            []       -> Nothing
            [x']     -> Just $ Number x'
            (x':xs') -> Just $ Number $ foldl op x' xs'

binaryNumericOp :: (Integer -> Integer -> Integer) -> [Expr] -> Maybe Expr
binaryNumericOp op [Number x, Number y] = Just $ Number $ op x y
binaryNumericOp _  _                    = Nothing

boolOp :: (Integer -> Integer -> Bool) -> [Expr] -> Maybe Expr
boolOp _  [] = Nothing
boolOp op xs =
    let numbers = [x | Number x <- xs] in
    if length xs /= length numbers
        then Nothing
        else case numbers of
            []       -> Nothing
            [_]     -> Just $ Bool True
            (x':xs') -> Just $ Bool $ all (x' `op`) xs'

car :: Expr -> Expr
car (List (x : _)) = x
car _              = undefined

cdr :: Expr -> Expr
cdr (List (_ : xs)) = List xs
cdr _               = undefined

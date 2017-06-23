module Calc where

import ExprT (ExprT(Lit, Add, Mul))
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

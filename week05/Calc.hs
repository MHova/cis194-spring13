module Calc where

import ExprT (ExprT(Lit, Add, Mul))
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n | n <= 0 = False
        | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
  deriving (Eq, Show)
  {- |
    Alternately we can cheat and have MinMax derive Ord. Because MinMax is just
    a newtype around Integer, the compiler is smart enough to simply defer to
    Integer's Ord instance to generate the implementation of MinMax's Ord
    instance. See below for why this is beneficial.
  -}
  -- deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax {- or lit -} $ min a b
  mul (MinMax a) (MinMax b) = MinMax {- or lit -} $ max a b
  {-|
    add and mul can be reduced to the below implementations if we cheat and
    have MinMax derive Ord as well.
  -}
  -- add = min
  -- mul = max

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  -- probably easier to read in non-point-free style
  -- lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = lit $ add a b
  mul (Mod7 a) (Mod7 b) = lit $ mul a b

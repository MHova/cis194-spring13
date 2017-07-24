{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Control.Applicative (liftA2)
import ExprT (ExprT(Lit, Add, Mul))
import Parser (parseExp)
import qualified StackVM as SV
import qualified Data.Map as M

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

-- Exercise 4

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

-- Exercise 5 (which made very little sense to me)

instance Expr SV.Program where
  lit a = [SV.PushI a]
  add a b = a ++ b ++ [SV.Add]
  mul a b = a ++ b ++ [SV.Mul]

compile :: String -> Maybe SV.Program
compile = parseExp lit add mul

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT =
    VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i = \_theMap -> Just i
  -- can be reduced to: lit = const . Just
  add lookupFunc1 lookupFunc2 =
    \theMap ->
      case lookupFunc1 theMap of
        Nothing -> Nothing
        Just valA ->
          case lookupFunc2 theMap of
            Nothing -> Nothing
            Just valB -> Just (valA `add` valB)
  mul lookupFunc1 lookupFunc2 =
    \theMap ->
      case lookupFunc1 theMap of
        Nothing -> Nothing
        Just valA ->
          case lookupFunc2 theMap of
            Nothing -> Nothing
            Just valB -> Just (valA `mul` valB)

{- |
  add' and mul' are alternate versions of add and mul using tools that the
  student would not be expected to know at this point in the course.
-}

add' ::
     (M.Map String Integer -> Maybe Integer)
  -> (M.Map String Integer -> Maybe Integer)
  -> (M.Map String Integer -> Maybe Integer)
add' a b theMap = liftA2 add (a theMap) (b theMap)

mul' ::
     (M.Map String Integer -> Maybe Integer)
  -> (M.Map String Integer -> Maybe Integer)
  -> (M.Map String Integer -> Maybe Integer)
mul' a b theMap = liftA2 mul (a theMap) (b theMap)

withVars ::
     [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

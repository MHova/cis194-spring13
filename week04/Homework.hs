module Homework where

import Data.Bool (bool)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- Wow, `product [] == 1`. That's convenient!
fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where
    f n = bool (3 * n + 1) (n `div` 2) (even n)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode newElement Leaf = Node 0 Leaf newElement Leaf
insertNode newElement (Node currHeight left currElement right)
  | height left < height right =
      Node (max (height newLeft) (height right) + 1) newLeft currElement right
  | otherwise =
      Node (max (height left) (height newRight) + 1) left currElement newRight
  where
    newLeft = insertNode newElement left
    newRight = insertNode newElement right

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

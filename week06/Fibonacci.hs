{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-2) + fib (x-1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

-- I definitely looked this one up
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a rest) = a : streamToList rest

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a rest) = Cons (f a) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed seed a = Cons a (streamFromSeed seed (seed a))

-- Exercise 5

-- there's gotta be a better way to do this
nats :: Stream Integer
nats = foldr Cons undefined [0..]

ruler :: Stream Integer
ruler = foldr (interleaveStreams . streamRepeat) undefined [0..]

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a restA) b = Cons a (interleaveStreams b restA)

-- I have no idea what Exercise 6 means

-- Exercise 7
{- Matrix ((a,b), (c,d)) ==
[a, b]
[c, d]
-}
newtype Matrix = Matrix ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
    Matrix ((a1,b1), (c1,d1)) * Matrix ((a2,b2), (c2,d2)) =
      Matrix (
              (a1*a2 + b1*c2, a1*b2 + b1*d2),
              (c1*a2 + d1*c2, c1*b2 + d1*d2)
             )

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n =
  let Matrix ((answer, _), _) = Matrix ((1,1), (1,0)) ^ n
  in answer
module CreditCards where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits int 
  | int <= 0 = []
  | otherwise = map (toInteger . digitToInt) $ show int

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ints =
    reverse . map maybeDouble $ zip (reverse ints) [1..]
  where
    maybeDouble (theInt, position)
      | position `mod` 2 == 0 = theInt * 2
      | otherwise = theInt

sumDigits :: [Integer] -> Integer
sumDigits ints = sum $ concatMap toDigits ints

validate :: Integer -> Bool
validate int = (sumDigits . doubleEveryOther . toDigits) int `mod` 10 == 0

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score = undefined

scoreString :: String -> Score
scoreString = foldr (mappend . score) mempty
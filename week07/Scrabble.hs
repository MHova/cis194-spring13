{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char (toUpper)
import Data.Map (Map)

import qualified Data.Map as M

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

toPairs :: String -> Int -> [(Char, Int)]
toPairs chars = zip chars . repeat

scoreMap :: Map Char Int
scoreMap = M.fromList $
  toPairs ['E', 'A', 'I', 'O', 'N', 'R', 'T', 'L', 'S', 'U'] 1 ++
  toPairs ['D', 'G'] 2 ++
  toPairs ['B', 'C', 'M', 'P'] 3 ++
  toPairs ['F', 'H', 'V', 'W', 'Y'] 4 ++
  toPairs ['K'] 5 ++
  toPairs ['J', 'X'] 8 ++
  toPairs ['Q', 'Z'] 10

score :: Char -> Score
score c = maybe (Score 0) Score $ M.lookup c scoreMap

scoreString :: String -> Score
scoreString = foldr (mappend . score . toUpper) mempty
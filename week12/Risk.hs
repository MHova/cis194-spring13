{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Risk where

import Control.Monad.Random
import Data.List (sort, partition, length)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle Battlefield{attackers, defenders} =
    roll attackingUnits >>= \attackRolls ->
    roll defendingUnits >>= \defenseRolls ->
    let (aWins, dWins) = partition didAttackerRollHigher $ zip attackRolls defenseRolls
    in return $ Battlefield (attackers - length dWins) (defenders - length aWins)
  where
    attackingUnits :: Int
    attackingUnits 
      | attackers >= 3 = 2
      | attackers == 2 = 1
      | otherwise = error "wtf can't attack with fewer than 2 units"
    
    defendingUnits :: Int
    defendingUnits
      | defenders >= 2 = 2
      | defenders == 1 = 1
      | otherwise = error "wtf why are we even battling if there are no defenders?"

    roll :: Int -> Rand StdGen [DieValue]
    roll numDice = reverse . sort <$> sequence (replicate numDice die)

    didAttackerRollHigher :: (DieValue, DieValue) -> Bool
    didAttackerRollHigher (aRoll, dRoll) = aRoll > dRoll

battle' :: Battlefield -> Rand StdGen Battlefield
battle' Battlefield{attackers, defenders} = do
    attackRolls <- roll attackingUnits
    defenseRolls <- roll defendingUnits
    let (aWins, dWins) = partition didAttackerRollHigher $ zip attackRolls defenseRolls
    return $ Battlefield (attackers - length dWins) (defenders - length aWins) 
  where
    attackingUnits :: Int
    attackingUnits 
      | attackers >= 3 = 2
      | attackers == 2 = 1
      | otherwise = error "wtf can't attack with fewer than 2 units"
    
    defendingUnits :: Int
    defendingUnits
      | defenders >= 2 = 2
      | defenders == 1 = 1
      | otherwise = error "wtf why are we even battling if there are no defenders?"

    roll :: Int -> Rand StdGen [DieValue]
    roll numDice = reverse . sort <$> sequence (replicate numDice die)

    didAttackerRollHigher :: (DieValue, DieValue) -> Bool
    didAttackerRollHigher (aRoll, dRoll) = aRoll > dRoll
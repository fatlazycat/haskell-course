{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture12.Risk where

import           Control.Monad
import           Control.Monad.Random
import           Data.List
import Data.Map

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

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show, Eq, Ord)

encounter :: DieValue -> DieValue -> Char
encounter a d = if a > d then 'a' else 'd'

-- battle :: Battlefield -> Rand StdGen Battlefield
-- battle b = do
--   attackersDie <- dice (attackers b)
--   defendersDie <- dice (defenders b)
--   return (Battlefield 1 1)

processBattle :: [DieValue] -> [DieValue] -> Battlefield
processBattle unsortedAttackers unsortedDefenders =
  Battlefield (updatedCount sortedAttackers results 'a') (updatedCount sortedDefenders results 'd')
  where results = frequency outcomes
        outcomes = zipWith encounter sortedAttackers sortedDefenders
        sortedAttackers = sortBy (flip compare) unsortedAttackers
        sortedDefenders = sortBy (flip compare) unsortedDefenders

frequency :: (Ord a) => [a] -> Map a Int
frequency xs = fromListWith (+) [(x, 1) | x <- xs]

updatedCount :: [DieValue] -> Map Char Int -> Char -> Int
updatedCount rolls m t = (length rolls) - (m ! t)

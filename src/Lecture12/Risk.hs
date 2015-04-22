{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lecture12.Risk where

import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.Map

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

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  attackersDie <- dice (attackers b)
  defendersDie <- dice (defenders b)
  return $ processBattle attackersDie defendersDie

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
updatedCount rolls m t = (length rolls) - (findWithDefault 0 t m)

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | a < 2 = return b
  | d == 0 = return b
  | otherwise = do
      resultOfBattle <- battle (Battlefield (maxAttackers a) (maxDefenders d))
      let ra = attackers resultOfBattle
      let rd = defenders resultOfBattle
      invade (Battlefield (a-ra) (d-rd))

maxAttackers :: Int -> Int
maxAttackers n
  | n == 0 = 0
  | n == 1 = 0
  | n == 2 = 1
  | otherwise = 2

maxDefenders :: Int -> Int
maxDefenders n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = 2

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  results <- multipleRuns 1000 b
  return (calcPercentage results 1000)

attackerWins :: Battlefield -> Bool
attackerWins (Battlefield a d) = a > d

calcPercentage :: [Battlefield] -> Int -> Double
calcPercentage bs n = (fromIntegral totalWins) / (fromIntegral n)
  where totalWins = Prelude.foldl (\(acc :: Integer) b -> if (attackerWins b) then (acc + 1) else acc ) 0 bs

multipleRuns :: Int -> Battlefield -> Rand StdGen [Battlefield]
multipleRuns n b = replicateM n (battle b)

main :: IO()
main = do
  result <- evalRandIO $ invade (Battlefield 113 2)
  putStrLn $ "Result is " ++ show result

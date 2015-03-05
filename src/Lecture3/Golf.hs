module Lecture3.Golf(skips, localMaxima, histogram) where

import           Data.Char
import           Data.List
import qualified Data.Map    as Map

skips :: [a] -> [[a]]
skips xs = [mapForDivisor n xs | n <- [1..(length xs)]]

mapForDivisor :: Int -> [a] -> [a]
mapForDivisor n xs = map snd $ filter (isDivisor n) (zippedIndex xs)

isDivisor :: Integral a => a -> (a, t) -> Bool
isDivisor n (x,_) = mod x n == 0

zippedIndex :: (Num a, Enum a) => [b] -> [(a, b)]
zippedIndex = zip [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map extractAnswer $ filter isMaxima $ makeCandidate xs

extractAnswer :: (t, t1, t2) -> t1
extractAnswer (_,y,_) = y

makeCandidate :: [c] -> [(c, c, c)]
makeCandidate xs
  | length xs >= 3 = zip3 xs (tail xs) (tail $ tail xs)
  | otherwise = []

isMaxima :: Ord a => (a, a, a) -> Bool
isMaxima (x,y,z)
  | y > x && y > z = True
  | otherwise = False

histogram :: [Int] -> String
histogram xs = concat $ reverse $ map (++ "\n") $ transpose dataRows
  where partitionSet = group xs
        maxNumberOfEntries = maxSize partitionSet
        entries = Map.fromList $ createMap maxNumberOfEntries partitionSet
        blanks = replicate maxNumberOfEntries ' '
        dataRows = [createEntry n blanks entries | n <- [0..9]]

createEntry :: Int -> String -> (Map.Map Int String) -> String
createEntry n d m = [(intToDigit n)] ++ "=" ++ (Map.findWithDefault d n m)

maxSize :: [[a]] -> Int
maxSize xs = maximum $ map length xs

createMap :: Int -> [[Int]] -> [(Int, String)]
createMap n (x:xs) = [(head x, createRow n x)] ++ (createMap n xs)
createMap _ [] = []

createRow :: Int -> [Int] -> String
createRow n xs = (replicate len '*') ++ (replicate (n - len) ' ')
  where len = length xs

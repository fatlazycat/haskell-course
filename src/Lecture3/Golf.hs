module Lecture3.Golf(skips, localMaxima) where

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

extractAnswer (_,y,_) = y

makeCandidate xs
  | length xs >= 3 = zip3 xs (tail xs) (tail $ tail xs)
  | otherwise = []

isMaxima (x,y,z)
  | y > x && y > z = True
  | otherwise = False

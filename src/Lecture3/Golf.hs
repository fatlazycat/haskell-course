module Lecture3.Golf(skips) where

skips :: [a] -> [[a]]
skips xs = [mapForDivisor n xs | n <- [1..(length xs)]]

mapForDivisor :: Int -> [a] -> [a]
mapForDivisor n xs = map snd $ filter (isDivisor n) (zippedIndex xs)

isDivisor :: Integral a => a -> (a, t) -> Bool
isDivisor n (x,_) = mod x n == 0

zippedIndex :: (Num a, Enum a) => [b] -> [(a, b)]
zippedIndex xs = zip [1..] xs

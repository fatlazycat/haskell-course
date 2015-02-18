module Lecture1 where

toDigits :: Integer -> [Integer]
toDigits x = reverse $ toDigitsRev x

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherHelper $ reverse xs

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper [x] = [x]
doubleEveryOtherHelper (x:y:xs) = [x,y*2] ++ (doubleEveryOtherHelper xs)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap toDigits xs

module Lecture1.Lecture1 where

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

validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 _ = (p1,p2) : []
hanoi numDiscs p1 p2 p3 = (hanoi (numDiscs-1) p1 p3 p2) ++ [(p1,p2)] ++ (hanoi (numDiscs-1) p3 p2 p1)

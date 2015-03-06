module Lecture4.Code(fun1, fun2, fun1', fun2') where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x a -> if even x then (x-2)*a else a) 1 

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' x = sum . filter even $ takeWhile (/= 1) $ iterate (\n -> if even n then n `div` 2 else 3 * n + 1) x

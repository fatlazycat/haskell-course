module Lecture6.Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

fib2 :: Int -> Integer
fib2 = (map fib2' [0..] !!) where
  fib2' 0 = 0
  fib2' 1 = 1
  fib2' n = fib2 (n-1) + fib2(n-2)

fibs2 :: [Integer]
fibs2 = [fib2 n | n <- [0..]]

data Stream a = Node a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Node x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

streamRepeat :: a -> Stream a
streamRepeat x = Node x (streamRepeat x)

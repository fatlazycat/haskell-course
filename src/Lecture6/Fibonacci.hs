{-# LANGUAGE FlexibleInstances #-}
module Lecture6.Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fiba' :: [Integer]
fiba' = [fib n | n <- [0..]]

fib2 :: Int -> Integer
fib2 = (map fib2' [0..] !!) where
  fib2' 0 = 0
  fib2' 1 = 1
  fib2' n = fib2 (n-1) + fib2(n-2)

fibb' :: [Integer]
fibb' = [fib2 n | n <- [0..]]

data Stream a = Node a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Node a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

streamRepeat :: a -> Stream a
streamRepeat a = Node a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Node a s) = Node (fn a) (streamMap fn s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn a = Node a' (streamFromSeed fn a')
  where a' = fn a

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Node a stream) otherStream = Node a (interleaveStreams otherStream stream)

x :: Stream Integer
x = Node 0 (Node 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger a = Node a (streamRepeat 0)
  negate = streamMap negate
  (+) (Node a a') (Node b b') = Node (a+b) (a' + b')
  (*) (Node a a') (Node b b') = Node (a*b) (streamRepeat 0) + (Node 0 (streamMap (*a) b') + (a' * Node b b'))
  abs = streamMap abs
  signum _ = 1

instance Fractional (Stream Integer) where
  (/) (Node a a') (Node b b') =
    Node (div a b) (streamRepeat 0) + Node 0 (streamMap (`div` b) (a' - (Node a a' / Node b b')*b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2 )

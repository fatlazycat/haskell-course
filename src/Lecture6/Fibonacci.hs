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

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Node x s) = Node (fn x) (streamMap fn s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = Node x' (streamFromSeed fn x')
  where x' = fn x

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])
--ruler = undefined

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Node x stream) otherStream = Node x (interleaveStreams otherStream stream)

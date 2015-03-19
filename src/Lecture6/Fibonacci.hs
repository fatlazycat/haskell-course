{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
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

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Cons a as) = Cons (fn a) (streamMap fn as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn a = Cons as (streamFromSeed fn as)
  where as = fn a

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) sb = Cons a (interleaveStreams sb as)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger a = Cons a (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons a as) (Cons b bs) = Cons (a+b) (as + bs)
  (*) (Cons a as) sb@(Cons b bs) = Cons (a*b) (streamMap (*a) bs + as*sb)

instance Fractional (Stream Integer) where
  (/) sa@(Cons a as) sb@(Cons b bs) =
    Cons (a `div` b) (streamMap (`div` b) (as - (sa / sb) * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2 )

data Matrix = Matrix Integer Integer Integer Integer

f = Matrix 1 1 1 0

instance Num (Matrix) where
  (*) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) =
    Matrix (a1*a2 + b1*c2) (a1*b2 + b1*d2)
           (c1*a2 + d1*c2) (c1*b2 + d1*d2)

fib4 :: Integer -> Integer
fib4 n = extractFib $ f ^ n

extractFib (Matrix _ f _ _) = f


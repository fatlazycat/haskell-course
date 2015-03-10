module Lecture4.Code(fun1, fun2, fun1', fun2', foldTree, Tree(Node, Leaf), xor, map', myFoldl) where

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

data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\x tree -> insertTree x tree) Leaf

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node n treeLeft i treeRight)
  | hl <= hr = Node (hr+1) (insertTree x treeLeft) i treeRight
  | hl > hr = Node (hl+1) treeLeft i (insertTree x treeRight)
  where hl = heightTree treeLeft
        hr = heightTree treeRight

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n _ _ _) = n

xor :: [Bool] -> Bool
xor = foldr (\x acc -> xorBool x acc ) False

xorBool :: Bool -> Bool -> Bool
xorBool True True = False
xorBool True False = True
xorBool False True = True
xorBool False False = False

map' :: (a -> b) -> [a] -> [b]
map' fn = foldr (\x acc -> fn(x) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl fn base = foldr (\x acc -> fn acc x) base


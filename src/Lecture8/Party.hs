
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lecture8.Party where

import           Data.Monoid
import           Data.Tree
import           Lecture8.Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL employees fun) = GL (e:employees) (empFun e + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs1 n1) (GL xs2 n2) = GL (xs1 ++ xs2) (n1+n2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ x) gl2@(GL _ y)
        | x >= y = gl1
        | otherwise = gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold fn (Node label xs) = fn label (map (treeFold fn) xs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = (with, without)
  where with = foldl (\acc (_, wo) -> mappend acc wo) zeroWith gls
        without = foldl (\acc (wi, _) -> mappend wi acc) zeroWithout gls
        zeroWith = GL [e] (empFun e)
        zeroWithout = GL [] 0

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun with without
  where maxWithAndWithout = treeFold nextLevel t
        with = fst maxWithAndWithout
        without = snd maxWithAndWithout

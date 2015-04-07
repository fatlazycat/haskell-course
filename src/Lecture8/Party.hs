{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lecture8.Party where

import           Data.Monoid
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


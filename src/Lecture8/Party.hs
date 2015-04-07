{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lecture8.Party where

import Lecture8.Employee
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e (GL employees fun) = GL (e:employees) (empFun e + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs1 n1) (GL xs2 n2) = GL (xs1 ++ xs2) (n1+n2)

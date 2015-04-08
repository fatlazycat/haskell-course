{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lecture8.Tests where

import           Data.Monoid
import           Lecture8.Employee
import           Lecture8.Party
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 8 Unit tests"
  [
    testCase "Add one to guest list" $ glCons emp1 (GL [] 0) @?= GL [emp1] 1
  , testCase "Add one to an existing guest list" $ glCons emp1 (GL [emp2] 2) @?= GL [emp1, emp2] 3

  , testCase "Use of guest list monoid" $ (mappend gl1 gl2) @?= GL [emp1, emp2] 3
  , testCase "moreFun" $ moreFun gl1 gl2 @?= gl2
  , testCase "calced fun" $ treeFold fnAddFun testCompany @?= 46
  ]

emp1 = Emp "Emp1" 1
emp2 = Emp "Emp2" 2
gl1 = GL [emp1] 1
gl2 = GL [emp2] 2

fnAddFun emp results = sum (empFun emp : results)

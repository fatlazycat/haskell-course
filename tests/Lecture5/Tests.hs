module Lecture5.Tests where

import           Lecture5.Calc
import           Lecture5.ExprT
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 5 Unit tests"
  [
    testCase "calc to 20" $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @?= 20
  ]

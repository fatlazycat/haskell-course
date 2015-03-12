module Lecture5.Tests where

import           Lecture5.Calc
import           Lecture5.ExprT
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 5 Unit tests"
  [
    testCase "calc to 20" $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @?= 20,

    testCase "test evalStr" $ evalStr "(2+3)*4" @?= Just 20,
    testCase "test failure of evalStr" $ evalStr "(2+3)*" @?= Nothing,

    testCase "type class ExprT" $
    (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) @?= (Mul (Add (Lit 2) (Lit 3)) (Lit 4)),

    testCase "type class Integer" $
    (mul (add (lit 2) (lit 3)) (lit 4)) @?= eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
  ]

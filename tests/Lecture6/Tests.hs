module Lecture6.Tests where

import           Lecture6.Fibonacci
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 6 Unit tests"
  [
    testCase "fib 0" $ fib 0 @?= 0, 
    testCase "fib 1" $ fib 1 @?= 1, 
    testCase "fib 5" $ fib 5 @?= 5 
  ] 

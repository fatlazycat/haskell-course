module Lecture5.Tests where

import           Lecture5.Calc
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 5 Unit tests"
  [
    testCase "basic" $ 1 @?= 1
  ]

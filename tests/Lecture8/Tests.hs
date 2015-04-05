module Lecture8.Tests where

import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 8 Unit tests"
  [
    testCase "1==1" $ 1 @?= 1
  ]

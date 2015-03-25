module Lecture7.Tests where

import           Lecture7.JoinList
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 7 Unit tests"
  [
    testCase "1=1" $ 1 @?= 1
  ]

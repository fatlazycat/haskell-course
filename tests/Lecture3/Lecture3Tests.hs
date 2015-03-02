module Lecture3.Lecture3Tests where

import           Lecture3.Golf
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 3 Unit tests"
  [
    testCase "process ABCD" $ skips "ABCD" @?= ["ABCD", "BD", "C", "D"],
    testCase "process hello!" $ skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"],
    testCase "process [1]" $ skips [1::Int] @?= [[1]],
    testCase "process [True, False]" $ skips [True, False] @?= [[True, False], [False]]
    --testCase "process []" $ skips [] @?= []
  ]


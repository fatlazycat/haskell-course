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
    testCase "process [True, False]" $ skips [True, False] @?= [[True, False], [False]],
    testCase "process []" $ skips ([]::[Int]) @?= [],

    testCase "local maxima [2,9,5,6,1]" $ localMaxima [2,9,5,6,1] @?= [9,6],
    testCase "local maxima [2,3,4,1,5]" $ localMaxima [2,3,4,1,5] @?= [4],
    testCase "local maxima [1,2,3,4,5]" $ localMaxima [1,2,3,4,5] @?= [],

    testCase "histogram [1,1,1,5]" $ histogram [1,1,1,5] @?=
      " *        \n" ++
      " *        \n" ++
      " *   *    \n" ++
      "==========\n" ++
      "0123456789\n"
  ]

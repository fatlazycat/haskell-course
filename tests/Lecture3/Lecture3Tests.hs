module Lecture3.Lecture3Tests where

import           Lecture3.Golf
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 3 Unit tests"
  [
    testCase "process ABCD" $ skips "ABCD" @?= ["ABCD", "CD", "C", "D"],
    testCase "process ABCD" $ skips "ABCD" @?= ["ABCD", "CD", "C", "D"]
                                               ]


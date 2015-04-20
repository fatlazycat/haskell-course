{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lecture12.Tests where

import           Lecture12.Risk
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 12 Unit tests"
  [
    testCase "1 == 1" $ 1 @?= 1
  ]

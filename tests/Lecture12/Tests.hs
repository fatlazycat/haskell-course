{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lecture12.Tests where

import           Lecture12.Risk
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 12 Unit tests"
  [
    testCase "a wins" $ encounter 2 1 @?= 'a'
  , testCase "b wins with draw" $ encounter 1 1 @?= 'd'
  , testCase "b wins" $ encounter 1 2 @?= 'd'

  , testCase "processBattle" $ processBattle [DV 1, DV 3, DV 1] [DV 1, DV 1] @?= Battlefield 2 1
  ]

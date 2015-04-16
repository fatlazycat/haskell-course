{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lecture11.Tests where

import           Control.Applicative
import           Lecture11.AParser
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 10 Unit tests"
  [
    testCase "1 == 1" $ 1 @?= 1
  ]


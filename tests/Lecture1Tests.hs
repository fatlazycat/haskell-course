module Lecture1Tests where

import           Test.Tasty
import           Test.Tasty.HUnit

unitTests = testGroup "Unit tests"
  [ testCase "1 = 1" $
        1 @?= 1
    ]

module Lecture2.Lecture2Tests where

import           Lecture2.LogAnalysis
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "True == True" $ True @?= False ]

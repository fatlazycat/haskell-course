module Lecture1Tests where

import           Lecture1
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests = testGroup "Unit tests"
  [ testCase "0 gives []" $ (toDigits 0) @?= [],
    testCase "-17 gives []" $ (toDigits (-17)) @?= [],
    testCase "1234 gives [1,2,3,4]" $ (toDigits 1234) @?= [1,2,3,4],
    testCase "0 gives []" $ (toDigitsRev 0) @?= [],
    testCase "-17 gives []" $ (toDigitsRev (-17)) @?= [],
    testCase "1234 gives [4,3,2,1]" $ (toDigitsRev 1234) @?= [4,3,2,1],
    testCase "[8,7,6,5] gives [16,7,12,5]" $ (doubleEveryOther []) @?= [16,7,12,5],
    testCase "[1,2,3] gives [1,4,3]" $ (doubleEveryOther [1,2,3]) @?= [1,4,3],
    testCase "Sum digitis" $ (sumDigits [16,7,12,5]) @?= 22
  ]

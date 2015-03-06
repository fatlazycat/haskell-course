module Lecture4.Tests where

import           Lecture4.Code
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 4 Unit tests"
  [
    testCase "fun1 basic 1" $ fun1 [1,2,3] @?= fun1' [1,2,3],
    testCase "fun1 basic 2" $ fun1 [5,6,7,8,9] @?= fun1' [5,6,7,8,9],
    testCase "fun1 basic 3" $ fun1 [11,34,1,56,89] @?= fun1' [11,34,1,56,89],
    testCase "fun2 basic 1" $ fun2 68 @?= fun2' 68,
    testCase "fun2 basic 2" $ fun2 69 @?= fun2' 69
  ]

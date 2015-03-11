module Lecture4.Tests where

import           Lecture4.Code
import           Test.Tasty
import           Test.Tasty.HUnit

expectedTree =
  Node 3 (Node 2 (Node 1 (Node 0 Leaf 'E' Leaf) 'H' Leaf) 'I' (Node 1 (Node 0 Leaf 'A' Leaf) 'D' Leaf)) 'J' (Node 2 (Node 1 (Node 0 Leaf 'C' Leaf) 'F' Leaf) 'G' (Node 0 Leaf 'B' Leaf))

unitTests :: TestTree
unitTests = testGroup "Lecture 4 Unit tests"
  [
    testCase "fun1 basic 1" $ fun1 [1,2,3] @?= fun1' [1,2,3],
    testCase "fun1 basic 2" $ fun1 [5,6,7,8,9] @?= fun1' [5,6,7,8,9],
    testCase "fun1 basic 3" $ fun1 [11,34,1,56,89] @?= fun1' [11,34,1,56,89],
    testCase "fun2 basic 1" $ fun2 68 @?= fun2' 68,
    testCase "fun2 basic 2" $ fun2 69 @?= fun2' 69,

    testCase "binary tree" $ foldTree "ABCDEFGHIJ" @?= expectedTree,

    testCase "xor 1" $ xor [False, True, False] @?= True,
    testCase "xor 2" $ xor [False, True, False, False, True] @?= False,

    testCase "maps equal" $ map (+2) [1,2,3,4] @?= map' (+2) [1,2,3,4],

    testCase "test foldl" $ foldl (-) 54 [1,2,3,4] @?= myFoldl (-) 54 [1,2,3,4],

    testCase "first primes 100 or less" $ sieveSundaram 49 @?= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
  ]

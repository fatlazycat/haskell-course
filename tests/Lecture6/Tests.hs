module Lecture6.Tests where

import           Lecture6.Fibonacci
import           Test.Tasty
import           Test.Tasty.HUnit

testData :: [Int] -> Stream Int
testData (x:xs) = Cons x (testData xs)
testData [] = error "Stream should be infinite"

testDataFromOne :: Stream Int
testDataFromOne = testData [1..]

takeFromStream :: Int -> Stream a -> [a]
takeFromStream x s = take x (streamToList s)

unitTests :: TestTree
unitTests = testGroup "Lecture 6 Unit tests"
  [
    testCase "fib 0" $ fib 0 @?= 0,
    testCase "fib 1" $ fib 1 @?= 1,
    testCase "fib 5" $ fib 5 @?= 5,
    testCase "first 5 fibs" $ take 5 fibs1 @?= [0,1,1,2,3],

    testCase "streamToList" $ take 5 (streamToList testDataFromOne) @?= [1,2,3,4,5],
    testCase "streamRepeat" $ takeFromStream 5 (streamRepeat 1) @?= ([1,1,1,1,1] :: [Int]),
    testCase "streamMap" $ takeFromStream 5 (streamMap (+1) (streamRepeat 1)) @?= ([2,2,2,2,2] :: [Int]),
    testCase "streamFromSeed" $ takeFromStream 5 (streamFromSeed (+1) 0) @?= ([1,2,3,4,5] :: [Int]),
    testCase "nats" $ takeFromStream 5 nats @?= ([0,1,2,3,4] :: [Integer]),

    testCase "interleave Stream" $
      takeFromStream 6 (interleaveStreams
              (streamMap (+1) (streamRepeat 0))
              (streamMap (+1) (streamRepeat 1))) @?= ([1,2,1,2,1,2] :: [Int]),

    testCase "ruler" $ takeFromStream 20 ruler @?= [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2],

    testCase "fibs3" $ takeFromStream 5 fibs3 @?= [0,1,1,2,3]
  ]

module Lecture6.Tests where

import           Lecture6.Fibonacci
import           Test.Tasty
import           Test.Tasty.HUnit

testData :: [Int] -> Stream Int
testData (x:xs) = Node x (testData xs)
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
    testCase "streamRepeat" $ takeFromStream 5 (streamRepeat 1) @?= ([1,1,1,1,1] :: [Int])
  ]

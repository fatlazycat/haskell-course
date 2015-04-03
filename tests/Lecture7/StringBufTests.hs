{-# LANGUAGE FlexibleInstances #-}
module Lecture7.StringBufTests where

import           Lecture7.StringBuffer
import           Lecture7.Buffer
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 7 String Buffer Unit tests"
  [
    testCase "toString" $ toString "hi\nthere\nsomeone\n" @?= "hi\nthere\nsomeone\n" -- not really a surprise :)
  , testCase "replaceLine" $ toString (replaceLine 1 "replaced" "hi\nthere\nsomeone\n") @?= toString "hi\nreplaced\nsomeone\n" 
  , testCase "number of lines" $ numLines "hi\nthere\nsomeone\n" @?= 3
  ]


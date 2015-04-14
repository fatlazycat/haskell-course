module Lecture10.Tests where

import           Lecture10.AParser
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 10 Unit tests"
  [
    testCase "fmap for Parser" $ runParser (fmap (+ 1) posInt) "123 4" @?= Just(124, " 4")
--  , testCase "Applicate pure Parser" $ (pure 1 :: Parser Integer) @?= Just(1, []) 
  ]


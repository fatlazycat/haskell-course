{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lecture10.Tests where

import           Control.Applicative
import           Lecture10.AParser
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 10 Unit tests"
  [
    testCase "fmap for Parser" $ runParser (fmap (+ 1) posInt) "123 4" @?= Just(124, " 4")
  , testCase "applicative" $ runParser (TwoChar <$> parserA <*> parserB) "AB" @?= Just((TwoChar 'A' 'B'), [])
  ]

isA x = 'A' == x
isB x = 'B' == x

parserA = satisfy isA
parserB = satisfy isB

data TwoChar = TwoChar Char Char
  deriving(Show, Eq)


{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lecture11.Tests where

import           Control.Applicative()
import           Data.Char
import           Lecture11.AParser
import           Lecture11.SExpr
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 11 Unit tests"
  [
    testCase "zeroOrMore multiple" $ runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" @?= Just("ABC","dEfgH")
  , testCase "oneOrMore multiple" $ runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" @?= Just("ABC","dEfgH")
  , testCase "zeroOrMore multiple" $ runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" @?= Just("","abcdeFGh")
  , testCase "oneOrMore multiple" $ runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" @?= Nothing
  ]


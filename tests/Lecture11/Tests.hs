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

  , testCase "spaces fail" $ runParser spaces "abcdeFGh" @?= Just("", "abcdeFGh")
  , testCase "spaces multiple" $ runParser spaces "    abcdeFGh" @?= Just("    ", "abcdeFGh")
  , testCase "spaces one" $ runParser spaces " abcdeFGh" @?= Just(" ", "abcdeFGh")

  , testCase "foobar baz" $ runParser ident "foobar baz" @?= Just("foobar"," baz")
  , testCase "foo33fa" $ runParser ident "foo33fA" @?= Just("foo33fA","")
  , testCase "2bad" $ runParser ident "2bad" @?= Nothing
  , testCase "" $ runParser ident "" @?= Nothing

  , testCase "5" $ runParser parseSExpr "5" @?= Just(A (N 5), "")
  , testCase "foo3" $ runParser parseSExpr "foo3" @?= Just(A (I "foo3"), "")
  , testCase "(bar (foo) 3 5 874)" $ runParser parseSExpr "(bar (foo) 3 5 874)" @?=
      Just(Comb [(A(I "bar")), Comb [(A(I "foo"))], (A(N 3)), (A(N 5)), (A(N 874))], "")

  ]

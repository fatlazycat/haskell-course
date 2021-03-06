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
  , testCase "applicative abParser" $ runParser abParser "AB" @?= Just(('A','B'), [])
  , testCase "applicative abParser fail" $ runParser abParser "CB" @?= Nothing
  , testCase "applicative abParser_" $ runParser abParser_ "AB" @?= Just((), [])
  , testCase "applicative abParser_ fail" $ runParser abParser_ "CB" @?= Nothing
  , testCase "applicative abParser2_" $ runParser abParser2_ "AB" @?= Just((), [])
  , testCase "applicative abParser2_ fail" $ runParser abParser2_ "CB" @?= Nothing
  , testCase "applicative intPair" $ runParser intPair "12 34" @?= Just ([12,34],[]) 
  , testCase "applicative intPair fail" $ runParser intPair "12-34" @?= Nothing
  , testCase "a or b with b" $ runParser (id <$> char 'A' <|> char 'B') "BC" @?= Just('B', "C")
  , testCase "a or b with a" $ runParser (id <$> char 'A' <|> char 'B') "AC" @?= Just('A', "C")
  , testCase "a or b with neither" $ runParser (id <$> char 'A' <|> char 'B') "CC" @?= Nothing
  , testCase "intOrUppercase 342abcd" $ runParser intOrUppercase "342abcd" @?= Just((), "abcd")
  , testCase "intOrUppercase XYZ" $ runParser intOrUppercase "XYZ" @?= Just((), "YZ")
  , testCase "intOrUppercase foo" $ runParser intOrUppercase "foo" @?= Nothing
  ]

isA x = 'A' == x
isB x = 'B' == x

parserA = satisfy isA
parserB = satisfy isB

data TwoChar = TwoChar Char Char
  deriving(Show, Eq)


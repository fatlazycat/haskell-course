module Lecture7.Tests where

import           Data.Monoid
import           Lecture7.JoinList
import           Test.Tasty
import           Test.Tasty.HUnit

aJoinList = Single (Product (1 :: Integer)) 'a'
aJoinList2 = Single (Product (2 :: Integer)) 'b'
anAppendedJoinList = Append (mappend (Product 1) (Product 2)) aJoinList aJoinList2

unitTests :: TestTree
unitTests = testGroup "Lecture 7 Unit tests"
  [
    testCase "x and empty" $ (aJoinList +++ Empty) @?= aJoinList,
    testCase "+++ some join lists" $ (aJoinList +++ aJoinList2) @?= anAppendedJoinList

  ]

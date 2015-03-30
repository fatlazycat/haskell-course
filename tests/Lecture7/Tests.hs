{-# LANGUAGE FlexibleInstances #-}
module Lecture7.Tests where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Lecture7.JoinList
import           Lecture7.Sized
import           Lecture7.Scrabble
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

aJoinList = Single (Product (1 :: Integer)) 'a'
aJoinList2 = Single (Product (2 :: Integer)) 'b'
anAppendedJoinList = Append (mappend (Product 1) (Product 2)) aJoinList aJoinList2

testData = ((bi 1 +++ bi 2) +++ (bi 3 +++ bi 4)) +++ ((bi 5 +++ bi 6) +++ (bi 7 +++ bi 8))

oddTestData = bi 1 +++ bi 2 +++ bi 3 +++ bi 4 +++ bi 5

unitTests :: TestTree
unitTests = testGroup "Lecture 7 Unit tests"
  [
    testCase "x and empty" $ (aJoinList +++ Empty) @?= aJoinList,
    testCase "+++ some join lists" $ (aJoinList +++ aJoinList2) @?= anAppendedJoinList,

    testCase "index 0 == 1" $ indexJ 0 testData @?= Just 1,
    testCase "index 3 == 4" $ indexJ 3 testData @?= Just 4,
    testCase "index 4 == 5" $ indexJ 4 testData @?= Just 5,
    testCase "index 7 == 8" $ indexJ 7 testData @?= Just 8,
    testCase "with odd data" $ indexJ 4 oddTestData @?= Just 5,

    testCase "q == 10" $ score 'q' @?= Score 10,
    testCase "Q == 10" $ score 'q' @?= Score 10,
    testCase " == 10" $ score ' ' @?= Score 0,
    testCase "! == 10" $ score '!' @?= Score 0
  ]

calc :: (Sized b, Monoid b) => JoinList b a -> JoinList b a -> b
calc left right = mappend (tag left) (tag right)

--instance (Sized b, Monoid b) => Arbitrary (JoinList b Integer) where
instance Arbitrary (JoinList Size Integer) where
  arbitrary = sized joinList'
    where joinList' 0 = liftM (Single (Size 1)) arbitrary
          joinList' n | n > 0 =
            liftM3 Append (calc <$> left <*> right) left right
            where left = joinList' (n `div` 2)
                  right = joinList' (n `div` 2)

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ testProperty "indexJ == index" $
    \i jl -> indexJ (i :: Int) (jl :: JoinList Size Integer) == (jlToList jl !!? i),

    testProperty "dropJ" $
    \n jl -> jlToList (dropJ (n::Int) (jl::JoinList Size Integer)) == drop n (jlToList jl),

    testProperty "takeJ" $
    \n jl -> jlToList (takeJ (n::Int) (jl::JoinList Size Integer)) == take n (jlToList jl)
  ]

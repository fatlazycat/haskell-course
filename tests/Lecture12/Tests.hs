{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lecture12.Tests where

import           Control.Monad.Random
import           Lecture12.Risk
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lecture 12 Unit tests"
  [
    testCase "a wins" $ encounter 2 1 @?= 'a'
  , testCase "b wins with draw" $ encounter 1 1 @?= 'd'
  , testCase "b wins" $ encounter 1 2 @?= 'd'

  , testCase "processBattle" $ processBattle [DV 1, DV 3, DV 1] [DV 1, DV 1] @?= Battlefield 2 1

  , testCase "test battle" $ fst (runRand (battle (Battlefield 3 2)) (mkStdGen 2)) @?= Battlefield 2 1

  , testCase "test invade" $ fst (runRand (invade (Battlefield 93 24)) (mkStdGen 2)) @?= Battlefield 53 0

  , testCase "test successProb" $ fst (runRand (successProb (Battlefield 30 30)) (mkStdGen 2)) @?= 0.872

  ]

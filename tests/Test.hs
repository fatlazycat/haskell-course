module Main where

import           Lecture1Tests
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners

main :: IO ()
main =
   defaultMainWithIngredients
     [ rerunningTests [ consoleTestReporter ] ]
     tests

tests :: TestTree
tests = testGroup "Lecture Tests" [Lecture1Tests.unitTests]

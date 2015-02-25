module Main where

import           Lecture1.Lecture1Tests
import           Lecture2.Lecture2Tests
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners

main :: IO ()
main =
   defaultMainWithIngredients
     [ rerunningTests [ consoleTestReporter ] ]
     tests

tests :: TestTree
tests = testGroup "Lecture Tests"
        [Lecture1.Lecture1Tests.unitTests
       , Lecture2.Lecture2Tests.unitTests
        ]

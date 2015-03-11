module Main where

import           Lecture1.Lecture1Tests
import           Lecture2.Lecture2Tests
import           Lecture3.Lecture3Tests
import           Lecture4.Tests
import           Lecture5.Tests
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
       , Lecture3.Lecture3Tests.unitTests
       , Lecture4.Tests.unitTests
       , Lecture5.Tests.unitTests
        ]

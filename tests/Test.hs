module Main where

import           Lecture1.Lecture1Tests
import           Lecture2.Lecture2Tests
import           Lecture3.Lecture3Tests
import           Lecture4.Tests
import           Lecture5.Tests
import           Lecture6.Tests
import           Lecture7.Tests
import           Lecture7.StringBufTests
import           Lecture8.Tests
import           Lecture10.Tests
import           Lecture11.Tests
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
       , Lecture6.Tests.unitTests
       , Lecture7.Tests.unitTests
       , Lecture7.StringBufTests.unitTests
       , properties
       , Lecture8.Tests.unitTests
       , Lecture10.Tests.unitTests
       , Lecture11.Tests.unitTests
        ]

properties :: TestTree
properties = testGroup "Properties" [Lecture7.Tests.qcProps]

module Tests where

import           Lecture1Tests
import           Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Lecture Tests" [Lecture1Tests.unitTests]

module Lecture2.Lecture2Tests where

import           Lecture2.Log
import           Lecture2.LogAnalysis
import           Test.Tasty
import           Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Can parse error message" $
    parseMessage "E 2 562 help help" @?= LogMessage (Error 2) 562 "help help",
    testCase "Can parse info message" $
    parseMessage "I 29 la la la" @?= LogMessage Info 29 "la la la",
    testCase "Can handle unknown messages" $
    parseMessage "This is not in the right format" @?= Unknown "This is not in the right format",
    testCase "Can process file" $
    do let file = "tests/Lecture2/error.log"
       logMsgs <- testParse parse 10 file
       assertEqual "Can parse 10 lines" 10 (length logMsgs)
  ]

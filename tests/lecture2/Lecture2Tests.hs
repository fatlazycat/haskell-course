module Lecture2.Lecture2Tests where

import           Lecture2.Log
import           Lecture2.LogAnalysis
import           Test.Tasty
import           Test.Tasty.HUnit

aLogMsg =  (LogMessage Info 2 "Info")
aLessThanTSLogMsg =  (LogMessage Info 1 "Info")
aGreaterTSLogMsg =  (LogMessage Info 3 "Info")
inOrderTree = Node (Node Leaf aLessThanTSLogMsg Leaf) aLogMsg (Node Leaf aGreaterTSLogMsg Leaf)

errorMsgs =
 [ "Way too many pickles"
 , "Bad pickle-flange interaction detected"
 , "Flange failed!"
 ]

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
       assertEqual "Can parse 10 lines" 10 (length logMsgs),

    testCase "Insert unknown message into empty tree returns empty tree" $
    (insert (Unknown "msg") Leaf) @?= Leaf,

    testCase "Insert one log message to empty tree" $
    (insert aLogMsg Leaf) @?= Node Leaf aLogMsg Leaf,

    testCase "New greater timestamp log message" $
    (insert aGreaterTSLogMsg (Node Leaf aLogMsg Leaf)) @?= (Node Leaf aLogMsg (Node Leaf aGreaterTSLogMsg Leaf)),

    testCase "New less timestamp log message" $
    (insert aLessThanTSLogMsg (Node Leaf aLogMsg Leaf)) @?= (Node (Node Leaf aLessThanTSLogMsg Leaf) aLogMsg Leaf),

    testCase "Can get an in order list of a tree" $
    (inOrder inOrderTree) @?= [aLessThanTSLogMsg, aLogMsg, aGreaterTSLogMsg],

    testCase "Can extract errors of 50 or more " $
    do let file = "tests/Lecture2/sample.log"
       msgs <- testWhatWentWrong parse whatWentWrong file
       assertEqual "content matches" errorMsgs msgs
  ]

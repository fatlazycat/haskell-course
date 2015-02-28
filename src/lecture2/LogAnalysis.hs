{-# OPTIONS_GHC -Wall #-}
module Lecture2.LogAnalysis where

import           Lecture2.Log
import           Text.Read

parseMessage :: String -> LogMessage
parseMessage s = parseMessageWords (words s) s

parseMessageWords :: [String] -> String -> LogMessage
parseMessageWords ("E":e:t:xs) entireMsg = makeErrorMessage (readMaybe e) (readMaybe t) (unwords xs) entireMsg
parseMessageWords ("I":t:xs) entireMsg = makeInfoMessage (readMaybe t) (unwords xs) entireMsg
parseMessageWords _ entireMsg = Unknown entireMsg

makeErrorMessage :: Maybe Int -> Maybe Int -> String  -> String -> LogMessage
makeErrorMessage (Just errorNumber) (Just timeStamp) msg _ = LogMessage (Error errorNumber) timeStamp msg
makeErrorMessage _ _ _ entireMsg = Unknown entireMsg

makeInfoMessage :: Maybe Int -> String  -> String -> LogMessage
makeInfoMessage (Just timeStamp) msg _ = LogMessage Info timeStamp msg
makeInfoMessage  _ _ entireMsg = Unknown entireMsg

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ (Node _ (Unknown _) _) = Leaf
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node less lmNode@(LogMessage _ tsNode _) greater)
  | ts < tsNode = Node (insert lm less) lmNode greater
  | ts > tsNode = Node less lmNode (insert lm greater)
  | otherwise = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = (inOrder l) ++ [lm] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map extractString $ inOrder $ foldr insert Leaf $ filter candidateMsg xs

extractString :: LogMessage -> String
extractString (LogMessage _ _ s) = s
extractString _ = ""

candidateMsg :: LogMessage -> Bool
candidateMsg (LogMessage (Error num) _ _)
  | num >= 50 = True
  | otherwise = False
candidateMsg _ = False

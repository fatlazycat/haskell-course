{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Lecture7.Scrabble where 

import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Char

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

scrabbleScores :: M.Map Char Int
scrabbleScores = M.fromList [
  ('a',1), ('b',3), ('c',3), ('d',2), ('e',1), ('f',4), ('g',2), ('h',4), ('i',1),
  ('j',8), ('k',5), ('l',1), ('m',3), ('n',1), ('o',1), ('p',3), ('q',10), ('r',1),
  ('s',1), ('t',1), ('u',1), ('v',4), ('w',4), ('x',8), ('y',4), ('z',10)
                           ]

score :: Char -> Score
score c = Score $ M.findWithDefault 0 (toLower c) scrabbleScores

scoreString :: String -> Score
scoreString s = mconcat $ map score s

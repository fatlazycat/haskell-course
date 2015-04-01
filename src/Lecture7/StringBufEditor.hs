module Lecture7.StringBufEditor where

import Lecture7.StringBuffer
import Lecture7.Editor

main :: IO ()
main = runEditor editor $ unlines
               [ "This buffer is for notes you don't want to save, and for"
               , "evaluation of steam valve coefficients."
               , "To load a different file, type the character L followed"
               , "by the name of the file."
               ]

run :: IO ()
run = runEditor editor "This buffer is for notes you don't want to save, and for"


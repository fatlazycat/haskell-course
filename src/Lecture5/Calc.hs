module Lecture5.Calc where

import           Lecture5.ExprT
import           Lecture5.Parser

eval :: ExprT -> Integer
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)
eval (Lit x) = x

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Nothing -> Nothing
  Just expr -> Just (eval expr)

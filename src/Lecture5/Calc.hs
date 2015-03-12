module Lecture5.Calc where

import           Lecture5.ExprT
import           Lecture5.Parser

eval :: ExprT -> Integer
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)
eval (Lit x) = x

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Nothing -> Nothing
  Just expr -> Just (eval expr)

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit x = eval $ Lit x
  mul x y = eval $ Mul (Lit x) (Lit y)
  add x y = eval $ Add (Lit x) (Lit y)

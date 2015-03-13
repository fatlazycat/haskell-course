module Lecture5.Tests where

import           Lecture5.Calc
import           Lecture5.ExprT
import           Lecture5.Parser
import qualified Lecture5.StackVM as S
import           Test.Tasty
import           Test.Tasty.HUnit

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

unitTests :: TestTree
unitTests = testGroup "Lecture 5 Unit tests"
  [
    testCase "calc to 20" $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @?= 20,

    testCase "test evalStr" $ evalStr "(2+3)*4" @?= Just 20,
    testCase "test failure of evalStr" $ evalStr "(2+3)*" @?= Nothing,

    testCase "type class ExprT" $
    (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) @?= Mul (Add (Lit 2) (Lit 3)) (Lit 4),

    testCase "type class Integer" $
    mul (add (lit 2) (lit 3)) (lit 4) @?= eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)),

    testCase "type class Bool" $ (testExp :: Maybe Bool) @?= Just True,

    testCase "type class MinMax" $ (testExp :: Maybe MinMax) @?= Just (MinMax 5),

    testCase "type class Mod7" $ (testExp :: Maybe Mod7) @?= Just (Mod7 0),

    testCase "type class Program" $ (testExp :: Maybe S.Program) @?= Just [S.PushI 3,S.PushI (-4),S.Mul,S.PushI 5,S.Add]
  ]

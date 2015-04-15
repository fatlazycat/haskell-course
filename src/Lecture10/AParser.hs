{- CIS 194 HW 10
   due Monday, 1 April
-}

module Lecture10.AParser where

import           Control.Applicative
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (first f) . g

instance Applicative Parser where
  -- pure :: a -> f a
  pure a = Parser $ \xs -> Just(a, xs)
  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) (Parser p1) (Parser p2) =
    Parser $ \xs -> case (p1 xs) of
                     Nothing -> Nothing
                     Just(a,b) -> case (p2 b) of
                                   Nothing -> Nothing
                                   Just(c,d) -> Just(a c, d)

first :: (a -> b) -> (a,c) -> (b,c)
first fn (a,c) = (fn a,c)

second :: (b -> c) -> (a,b) -> (a,c)
second fn (a,c) = (a,fn c)

abParser :: Parser (Char, Char)
abParser = (\x y -> (x,y)) <$> char 'A' <*> char 'B'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'A' <*> char 'B'

abParser2_ :: Parser ()
abParser2_ = (\_ -> ()) <$> abParser

intPair :: Parser ([Integer])
intPair = (\x _ y -> [x,y]) <$> posInt <*> char ' ' <*> posInt

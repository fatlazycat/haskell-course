{- CIS 194 HW 11
   due Monday, 8 April
-}

module Lecture11.SExpr where

import           Control.Applicative
import           Data.Char
import           Lecture11.AParser

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseInteger :: Parser Atom
parseInteger = spaces *> (N <$> posInt) <* spaces

parseIdent :: Parser Atom
parseIdent = spaces *> (I <$> ident) <* spaces

parseAtom :: Parser Atom
parseAtom = parseInteger <|> parseIdent

parseComb :: Parser SExpr
parseComb = spaces *> (Comb <$> (oneOrMore parseSExpr)) <* spaces

parseSExpr :: Parser SExpr
parseSExpr = (A <$> parseAtom) <|> (spaces *> char '(' *> parseComb <* char ')' <* spaces)

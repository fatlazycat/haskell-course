{-# LANGUAGE FlexibleInstances #-}
module Lecture7.JoinList where

import           Data.Monoid
import           Lecture7.Sized

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x Empty = x
(+++) left right = Append (mappend (tag left) (tag right)) left right

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

bi = Single (Size 1)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ a) = Just a
indexJ n (Append totalSize j1 j2)
  | (n >= 0) && (n < lowerBound) = indexJ n j1
  | (n >= lowerBound) && (n < total) = indexJ (n-upperBound) j2
  | otherwise = Nothing
  where lowerBound = getSize $ size $ tag j1
        upperBound = getSize $ size $ tag j2
        total = getSize $ size totalSize

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


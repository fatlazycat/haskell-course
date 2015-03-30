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

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n (Single m a)
      | n <= 0 = Single m a
      | otherwise = Empty
dropJ n (Append totalSize j1 j2)
      | (n >= 0) && (n < lowerBound) = 
          let left = dropJ n j1
              right = j2
              t = mappend (tag left) (tag right) 
          in Append t left right
      | (n >= lowerBound) && (n < total) = dropJ (n-lowerBound) j2
      | n < 0 = Append totalSize j1 j2
      | otherwise = Empty
      where lowerBound = getSize $ size $ tag j1
            total = getSize $ size totalSize

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n (Single m a)
      | n > 0 = Single m a
      | otherwise = Empty
takeJ n (Append s j1 j2)
      | (n >= 1) && (n < lowerBound) = takeJ n j1
      | (n >= lowerBound) && (n < total) =
          let left = j1
              right = takeJ (n - lowerBound) j2
              t = mappend (tag left) (tag right) 
          in Append t left right
      | n <= 0 = Empty
      | otherwise = Append s j1 j2
      where lowerBound = getSize $ size $ tag j1
            total = getSize $ size s

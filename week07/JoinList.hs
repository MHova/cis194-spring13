module JoinList where

import Data.Monoid ((<>))
import Sized (Sized, Size(Size), getSize, size)

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- it's not really clear if Empty is supposed to act like mempty, but let's treat it that way.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ right = right
left +++ Empty = left
left +++ right = Append (tag left <> tag right) left right

-- Exercise 2

-- a version of indexJ that looks nice, but will spend logn time to figure out that the index is out of bounds
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Append _ left right)
  | index < leftSize = indexJ index left
  | otherwise        = indexJ (index - leftSize) right
    where
      leftSize = size' left
indexJ 0 (Single _ a) = Just a -- The only valid case is when we encounter a Single and current index is 0
indexJ _ (Single _ _) = Nothing
indexJ _ Empty = Nothing

-- an alternate version of indexJ that only does bounds checking once and will take constant time to
-- detect that the index is out of bounds, but it must rely on either the use of `error` or an
-- unreachable Nothing case for Empty
indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' index jl
  | index     <  0                         = Nothing
  | index     >= (getSize . size . tag) jl = Nothing
  | otherwise                              = Just $ go index jl
  where
    go :: (Sized b, Monoid b) => Int -> JoinList b a -> a
    go index (Append _ left right)
      | index < leftSize = go index left
      | otherwise        = go (index - leftSize) right
      where
        leftSize = size' left
    -- since we did bounds sanity checking at the very beginning,
    -- it must be the case that encountering a Single means we found the answer
    go index (Single _ a) = a
    go _     Empty        = error "We should never get here because of the `index >= size` check"

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ numToDrop append@(Append s left right)
  | numToDrop      <= 0      = append
  | Size numToDrop >= size s = Empty
  | otherwise = Append
      s -- uhh okay...there's no way for me to update the size...
      (dropJ numToDrop left)
      (dropJ (numToDrop - sizeLeft) right)
  where
    sizeLeft = size' left
dropJ numToDrop single@(Single _ _)
  | numToDrop <= 0 = single
  | otherwise      = Empty
dropJ _ Empty = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ numToTake single@(Single _ _)
  | numToTake <= 0 = Empty
  | otherwise      = single
takeJ numToTake append@(Append s left right)
  | numToTake      <= 0      = Empty
  | Size numToTake >= size s = append
  | otherwise                = left +++ takeJ (numToTake - size' left) right

size' :: (Sized m, Monoid m) => JoinList m a -> Int
size' = getSize . size . tag




-- Testing functions

-- produces an unbalanced JoinList. Eh that's good enough.
listToSizeJL :: [a] -> JoinList Size a
listToSizeJL = foldr ((+++) . Single (Size 1)) Empty

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
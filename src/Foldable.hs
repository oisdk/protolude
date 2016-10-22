module Foldable
  ( module F
  , foldl
  , foldr1
  , foldl1
  , head
  , last
  , product
  , sum
  , minimum
  , maximum
  , minimumBy
  , maximumBy
  ) where

import           Data.Foldable as F hiding (foldl, foldl1, foldr1, maximum,
                                     maximumBy, minimum, minimumBy, product,
                                     sum)

import           Data.Function (const, (.))
import           Data.Maybe    (Maybe (..), maybe)
import           Data.Ord      (Ord, Ordering (..), max, min)
import           GHC.Num       (Num, (*), (+))

foldl :: Foldable f => (b -> a -> b) -> b -> f a -> b
foldl = foldl'

foldr1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldr1 f = foldr (\e -> Just . maybe e (f e) ) Nothing

foldl1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldl1 f xs = foldr (\_ _ -> foldl g Nothing xs) Nothing xs where
  g a e = Just (maybe e (`f` e) a)

head :: Foldable f => f a -> Maybe a
head = foldr1 const

last :: Foldable f => f a -> Maybe a
last = foldl1 const

{-# INLINE product #-}
product :: (Foldable f, Num a) => f a -> a
product = foldl' (*) 1

{-# INLINE sum #-}
sum :: (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0

minimum :: (Foldable f, Ord a) => f a -> Maybe a
minimum = foldl1 min

maximum :: (Foldable f, Ord a) => f a -> Maybe a
maximum = foldl1 max

minimumBy :: (Foldable f) => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy cmp = foldl1 f where
  f x y = case cmp x y of
    GT -> y
    _  -> x

maximumBy :: (Foldable f) => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy cmp = foldl1 f where
  f x y = case cmp x y of
    LT -> y
    _  -> x

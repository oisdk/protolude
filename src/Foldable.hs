{-# LANGUAGE NoImplicitPrelude #-}

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
  , foldr2
  ) where

import           Data.Foldable as F hiding (foldl, foldl1, foldr1, maximum,
                                     maximumBy, minimum, minimumBy, product,
                                     sum, any, all)

import           Data.Function (const, (.))
import           Data.Maybe    (Maybe (..), maybe)
import           Data.Ord      (Ord, Ordering (..), max, min)
import Semiring

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
product :: (Foldable f, Semiring a) => f a -> a
product = foldl' (*) one

{-# INLINE sum #-}
sum :: (Foldable f, Semiring a) => f a -> a
sum = foldl' (+) zero

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

newtype ScottZip a b =
  ScottZip (a -> (ScottZip a b -> b) -> b)

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs = foldr f (const i) xs . ScottZip . foldr g (\_ _ -> i) where
 g e2 r2 e1 r1 = c e1 e2 (r1 (ScottZip r2))
 f e r (ScottZip x) = x e r

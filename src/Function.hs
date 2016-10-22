{-# LANGUAGE NoImplicitPrelude #-}

module Function
 ( module F
 , applyN
 , converge
 ) where

import           Base
import           Data.Bool
import           Data.Eq
import           Data.Function as F (const, fix, flip, on, ($))
import           Semiring

applyN :: (a -> a) -> Int -> a -> a
applyN f = go where
  go 0 x = x
  go n x = go (n-1) (f x)

converge :: Eq a => (a -> a) -> a -> a
converge f = r where
  r x | x == y = y
      | otherwise = r y
      where y = f x

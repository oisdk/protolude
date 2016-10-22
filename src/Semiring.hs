{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Semiring
 ( Semiring(..)
 , Ring(..)
 ) where

import           Data.Bool
import           Data.Int  (Int, Int16, Int32, Int64, Int8)
import           GHC.Float (Double, Float)
import qualified GHC.Num   as Num

class Semiring a where
  one  :: a
  zero :: a
  infixl 7 *
  (*)  :: a -> a -> a
  infixl 6 +
  (+)  :: a -> a -> a

  default one :: Num.Num a => a
  default zero :: Num.Num a => a
  one = 1
  zero = 0

  default (+) :: Num.Num a => a -> a -> a
  default (*) :: Num.Num a => a -> a -> a
  (+) = (Num.+)
  (*) = (Num.*)

instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Num.Integer
instance Semiring Float
instance Semiring Double

class Semiring a => Ring a where
  infixl 6 -
  (-) :: a -> a -> a
  default (-) :: Num.Num a => a -> a -> a
  (-) = (Num.-)

instance Ring Int
instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64
instance Ring Num.Integer
instance Ring Float
instance Ring Double

instance Semiring Bool where
  one = True
  zero = False
  (*) = (&&)
  (+) = (||)

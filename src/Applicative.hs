{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Applicative (
  purer,
  liftAA2,
  (<<*>>),
) where

import Data.Function ((.))
import Control.Applicative

purer :: (Applicative f, Applicative g) => a -> f (g a)
purer = pure . pure

liftAA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
liftAA2 = liftA2 . liftA2

(<<*>>) :: (Applicative f, Applicative g)  => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

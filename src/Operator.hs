module Operator
  ( (&)
  , (<-<)
  , (>->)
  , (.:)
  ) where

import Data.Function
import Data.Functor

infixl 1 <-<
(<-<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <-< f = fmap g . f

infixr 1 >->
(>->) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f >-> g = fmap g . f

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

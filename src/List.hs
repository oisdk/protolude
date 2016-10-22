{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module List
 ( uncons
 , unfoldl
 , module L
 ) where

import Data.List as L
  ( splitAt
  , break
  , intercalate
  , isPrefixOf
  , drop
  , reverse
  , replicate
  , take
  , sortBy
  , sortOn
  , sort
  , intersperse
  , transpose
  , subsequences
  , permutations
  , scanl
  , scanr
  , iterate
  , repeat
  , cycle
  , unfoldr
  , takeWhile
  , dropWhile
  , group
  , inits
  , tails
  , unfoldr
  , zipWith
  , zip
  )

import           Data.Function (flip, (.))
import           Data.Maybe    (Maybe (..), maybe)
import           Data.Tuple    (uncurry)

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f = r [] where r a = maybe a ((uncurry.flip) (r . (:a))) . f

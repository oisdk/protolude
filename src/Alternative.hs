module Alternative where

import           Control.Applicative (Alternative (..), Applicative (..))
import           Control.Monad       (Monad (..), guard, join, (=<<))
import           Control.Monad.State (evalState, gets, modify')
import           Data.Bool           (Bool)
import           Data.Either         (Either (..))
import           Data.Foldable       (Foldable (..))
import           Data.Function       (flip, (.))
import           Data.Functor        (fmap, (<$), (<$>))
import           Data.Ord            (Ord)
import qualified Data.Set            as Set
import           Data.Traversable    (Traversable (..))

filter :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
filter p = (=<<) ((<$) <*> guard . p)

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = x <$ guard (p x)

choice :: (Foldable f, Alternative m) => f a -> m a
choice = foldr ((<|>) . pure) empty

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = fmap Left x <|> fmap Right y

mapMaybe :: (Foldable f, Monad m, Alternative m) => (a -> f b) -> m a -> m b
mapMaybe = (=<<) . (.) choice

filterM :: (Monad m, Alternative m, Applicative f, Traversable m) => (a -> f Bool) -> m a -> f (m a)
filterM p = fmap join . traverse (\x -> (x <$) . guard <$> p x)

-- | >>> ordNub [1,2,3,2,4,1,5]
-- [1,2,3,4,5]
-- >>> take 5 (ordNub [1..])
-- [1,2,3,4,5]
ordNub :: (Monad m, Alternative m, Traversable m, Ord a) => m a -> m a
ordNub = flip evalState Set.empty
       . filterM (\e -> gets (Set.notMember e) <* modify' (Set.insert e))

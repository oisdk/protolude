{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Protolude
import           Test.DocTest
import           Test.QuickCheck


_expectTypes :: ()
_expectTypes = () where
  _dot :: Category cat => cat b c -> cat a b -> cat a c
  _dot = (.)
  _id :: Category cat => cat a a
  _id = id

prop_filter :: [Int] -> Property
prop_filter xs = filter even xs === [ x | x <- xs, even x ]

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO Bool
main = do
  doctest [ "-isrc"
          , "src/Alternative"
          , "src/Applicative"
          , "src/Base"
          , "src/Bifunctor"
          , "src/Bool"
          , "src/Cat"
          , "src/Conv"
          , "src/Debug"
          , "src/Either"
          , "src/Exceptions"
          , "src/Foldable"
          , "src/Function"
          , "src/Functor"
          , "src/List"
          , "src/Monad"
          , "src/Operator"
          , "src/Panic"
          , "src/Protolude"
          , "src/Semiring"
          , "src/Show"
          , "src/Unsafe" ]
  runTests

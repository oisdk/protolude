{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Protolude (
  module X,
  module Base,
  print,
  throwIO,
  throwTo,
  show,
  pass,
  LText,
  LByteString,
) where

import Show as X
import Cat as X
import Bool as X
import Debug as X
import Monad as X
import Functor as X
import Operator as X
import Function as X
import Either as X
import Applicative as X
import Alternative as X
import Foldable as X
import Conv as X
import Panic as X
import Exceptions as X

import Base as Base hiding
  ( putStr           -- Overriden by Show.putStr
  , putStrLn         -- Overriden by Show.putStrLn
  , print            -- Overriden by Protolude.print
  , error            -- Overriden by Debug.error
  , undefined        -- Overriden by Debug.undefined
  , show             -- Overriden by Protolude.show
  , showFloat        -- Custom Show instances deprecated.
  , showList         -- Custom Show instances deprecated.
  , showSigned       -- Custom Show instances deprecated.
  , showSignedFloat  -- Custom Show instances deprecated.
  , showsPrec        -- Custom Show instances deprecated.
  )
import qualified Base as PBase

-- Used for 'show', not exported.
import Data.String (String)
import Data.String as X (IsString)

-- Maybe'ized version of partial functions
import Safe as X
  ( headMay
  , headDef
  , initMay
  , initDef
  , initSafe
  , tailMay
  , tailDef
  , tailSafe
  , lastDef
  , lastMay
  , foldr1May
  , foldl1May
  , atMay
  , atDef
  )

-- Applicatives
import Control.Applicative as X
  ( Applicative(..)
  , Alternative(..)
  , Const(..)
  , ZipList(..)
  , (<**>)
  , liftA
  , liftA2
  , liftA3
  , optional
  )

-- Base typeclasses
import Data.Eq as X
import Data.Ord as X
import Data.Traversable as X
import Semiring as X
import Data.Functor.Identity as X

#if ( __GLASGOW_HASKELL__ >= 800 )
import Data.Monoid as X hiding ((<>))
import Data.Semigroup as X ( Semigroup(..) )
#else
import Data.Monoid as X
#endif

#if (__GLASGOW_HASKELL__ >= 710)
import Data.Bifunctor as X (Bifunctor(..))
#else
import Bifunctor as X (Bifunctor(..))
#endif

-- Deepseq
import Control.DeepSeq as X
  ( NFData(..)
  , ($!!)
  , deepseq
  , force
  )

-- Data structures
import Data.Tuple as X

import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)

#if ( __GLASGOW_HASKELL__ >= 710 )
import Data.Proxy as X
  ( Proxy(..)
  )

import Data.Typeable as X
  ( TypeRep
  , Typeable
  , typeRep
  , cast
  , eqT
  )

import Data.Type.Coercion as X
  ( Coercion(..)
  , coerceWith
  )

import Data.Type.Equality as X
  ( (:~:)(..)
  , type (==)
  , sym
  , trans
  , castWith
  , gcastWith
  )

import Data.Void as X
  ( Void
  , absurd
  , vacuous
  )
#endif

-- Monad transformers
import Control.Monad.State as X
  ( MonadState
  , State
  , StateT
  , put
  , get
  , gets
  , modify
  , state
  , withState

  , runState
  , execState
  , evalState

  , runStateT
  , execStateT
  , evalStateT
  )

import Control.Monad.Reader as X
  ( MonadReader
  , Reader
  , ReaderT
  , ask
  , asks
  , local
  , reader
  , runReader
  , runReaderT
  )

import Control.Monad.Except as X
  ( MonadError
  , Except
  , ExceptT
  , throwError
  , catchError
  , runExcept
  , runExceptT
  )

import Control.Monad.Trans as X
  ( MonadIO
  , lift
  , liftIO
  )

-- Base types
import Data.Int as X
import Data.Bits as X hiding
  ( unsafeShiftL
  , unsafeShiftR
  )
import Data.Word as X
import Data.Either as X
import Data.Complex as X
import Data.Char as X (chr)
import Data.Bool as X hiding (bool)
import Data.Maybe as X hiding (fromJust, mapMaybe)

-- Genericss
import GHC.Generics as X
  ( Generic(..)
  , Rep
  , K1(..)
  , M1(..)
  , U1(..)
  , V1
  , D1
  , C1
  , S1
  , (:+:)
  , (:*:)
  , Rec0
  , Constructor(..)
  , Selector(..)
  , Fixity(..)
#if ( __GLASGOW_HASKELL__ >= 800 )
  , Meta(..)
#endif
  )

-- ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString as X (ByteString)

-- Text
import Data.Text as X (Text)
import qualified Data.Text.Lazy

import Data.Text.IO as X
  ( getLine
  , getContents
  , interact
  , readFile
  , writeFile
  , appendFile
  )

import Data.Text.Lazy as X
  ( toStrict
  , fromStrict
  )

import Data.Text.Encoding as X
  ( encodeUtf8
  , decodeUtf8
  , decodeUtf8'
  , decodeUtf8With
  )

-- IO
import System.Exit as X
import System.Environment as X (getArgs)
import System.IO as X
  ( Handle
  , FilePath
  , IOMode(..)
  , stdin
  , stdout
  , stderr
  , withFile
  , openFile
  )

-- ST
import Control.Monad.ST as X

-- Concurrency and Parallelism
import Control.Exception as X hiding
  ( throw    -- Impure throw is forbidden.
  , throwIO
  , throwTo
  , assert
  , displayException
  , Handler(..)
  )

import qualified Control.Exception

import Control.Monad.STM as X
import Control.Concurrent as X hiding
  ( throwTo
  , yield
  )
import Control.Concurrent.Async as X

import Foreign.Storable as X (Storable)

-- Read instances hiding unsafe builtins (read)
import Text.Read as X
  ( Read
  , reads
  , readMaybe
  , readEither
  )

-- Type synonymss for lazy texts
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString

print :: (X.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

throwIO :: (X.MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Control.Exception.throwIO

throwTo :: (X.MonadIO m, Exception e) => ThreadId -> e -> m ()
throwTo tid e = liftIO (Control.Exception.throwTo tid e)

pass :: Applicative f => f ()
pass = pure ()

show :: (Show a, StringConv String b) => a -> b
show x = toS (PBase.show x)
{-# SPECIALIZE show :: Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> String  #-}

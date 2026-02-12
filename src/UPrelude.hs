{-# LANGUAGE ExplicitForAll, UnicodeSyntax #-}
module UPrelude
  ( module Prelude
  , module Prelude.Unicode
  , module UPrelude
  , module Control.Applicative.Unicode
  , module Control.Monad
  , module Control.Monad.Unicode
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Data.Bits
  , module Data.Maybe
  , module Data.String
  , module Data.Text
  , module Data.Word
  , module Foreign.C.String
  , module Foreign.Storable
  , module Foreign.Ptr
  , module Foreign.ForeignPtr
  , module Foreign.Marshal.Array
  , module Foreign.Marshal.Alloc ) where

-- This module re-exports Prelude and related libraries, along with custom operators
-- and utility functions. It consolidates commonly used imports and overrides certain
-- definitions for convenience, especially when working with Unicode operators.

import Prelude.Unicode
import qualified Prelude as P
import Prelude hiding ((>>=), (=<<))
import qualified Data.Text as T
import qualified Data.Bits as B
import qualified Data.Functor as F

-- Standard Haskell modules for string handling, word-sized integers, file paths, etc.
import Data.String (fromString)
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Text (Text)
import qualified System.FilePath as FP

-- Monad imports for advanced functional programming
import Control.Monad ( (>>=), (=<<), when, unless, void, forever, forM_, forM, mapM_, replicateM, replicateM_, guard, join)
import Control.Monad.Reader (ask, asks, local, ReaderT, runReaderT, Reader, runReader)
import Control.Monad.State (get, gets, modify)
import Control.Monad.Unicode ( (=≪), (↢), (↣), (≫), (≫=) )
import Control.Applicative.Unicode ( (∅), (⊛) )

-- Additional data utilities and bitwise operations
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing, catMaybes, listToMaybe, maybeToList)
import Data.Bits ((.&.), (.|.), zeroBits, testBit, setBit, clearBit, complement, shiftL, shiftR, rotateL, rotateR, bit, popCount, xor)

-- Foreign function interface modules for working with C strings and pointers
import Foreign.C.String (peekCString)
import Foreign.Storable (peek, Storable(..))
import Foreign.Ptr (castPtr, nullPtr, plusPtr, Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray, copyArray)

--------------------------------------------------------------------------------
-- Custom Symbols, Operators, and Utility Functions
--------------------------------------------------------------------------------

-- | 'flatten' converts a list of lists into a single list by concatenation.
--   Equivalent to 'concat' but uses a lambda for clarity.
flatten ∷ [[α]] → [α]
flatten xs = (\z n → foldr (\x y → foldr z y x) n xs) (:) []

-- Operator fixities for easy reference
infixl 7 ⌃
infixl 5 ⌄
infixl 1 ⌦
infixl 1 ⌫
infixl 4 ⚟
infixl 4 ⚞
infixl 4 ⊚
infixr 5 ⊘
infixr 7 ⊙

--------------------------------------------------------------------------------
-- Bitwise Operators
--------------------------------------------------------------------------------

-- | '⌃' is an alias for bitwise AND (.&.).
(⌃) ∷ B.Bits a ⇒ a → a → a
(⌃) = (B..&.)
{-# INLINE (⌃) #-}

-- | '⌄' is an alias for bitwise OR (.|.).
(⌄) ∷ B.Bits a ⇒ a → a → a
(⌄) = (B..|.)
{-# INLINE (⌄) #-}

--------------------------------------------------------------------------------
-- Functor Operators
--------------------------------------------------------------------------------

-- | '⚟' replaces the result of the given functor with a constant value.
(⚟) ∷ Functor f ⇒ a → f b → f a
(⚟) = (F.<$)
{-# INLINE (⚟) #-}

-- | '⚞' replaces the result of the given functor with a constant value (infix variation).
(⚞) ∷ Functor f ⇒ f a → b → f b
(⚞) = (F.$>)
{-# INLINE (⚞) #-}

-- | '⊚' is an alias for fmap, applying a function to the result of a functor.
(⊚) ∷ Functor f ⇒ (a → b) → f a → f b
(⊚) = (F.<$>)
{-# INLINE (⊚) #-}

--------------------------------------------------------------------------------
-- Monadic Operators
--------------------------------------------------------------------------------

-- | '⌦' is an alias for the standard (>>=) operator to chain monadic actions.
(⌦) ∷ Monad m ⇒ ∀ a b. m a → (a → m b) → m b
(⌦) = (P.>>=)
{-# INLINE (⌦) #-}

-- | '⌫' is an alias for the standard (=<<) operator, reversing the arguments of (>>=).
(⌫) ∷ Monad m ⇒ (a → m b) → m a → m b
(⌫) = (P.=<<)
{-# INLINE (⌫) #-}

--------------------------------------------------------------------------------
-- FilePath Operators
--------------------------------------------------------------------------------

-- | '⊘' joins two file path segments (similar to '</>' in System.FilePath).
(⊘) ∷ FilePath → FilePath → FilePath
(⊘) = (FP.</>)
{-# INLINE (⊘) #-}

-- | '⊙' adds or replaces the file extension of a path (similar to '<.>' in System.FilePath).
(⊙) ∷ FilePath → String → FilePath
(⊙) = (FP.<.>)
{-# INLINE (⊙) #-}

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

import Prelude.Unicode
import qualified Prelude as P
import Prelude hiding ((>>=), (=<<))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Bits as B
import qualified Data.Functor as F
import Data.Serialize (Serialize(..))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.Serialize as S

import Data.String (fromString)
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Text (Text)
import qualified System.FilePath as FP

import Control.Monad ( (>>=), (=<<), when, unless, void, forever, forM_, forM, mapM_, replicateM, replicateM_, guard, join)
import Control.Monad.Reader (ask, asks, local, ReaderT, runReaderT, Reader, runReader)
import Control.Monad.State (get, gets, modify)
import Control.Monad.Unicode ( (=≪), (↢), (↣), (≫), (≫=) )
import Control.Applicative.Unicode ( (∅), (⊛) )

import Data.Maybe (fromMaybe, isJust, fromJust, isNothing, catMaybes, listToMaybe, maybeToList)
import Data.Bits ((.&.), (.|.), zeroBits, testBit, setBit, clearBit, complement, shiftL, shiftR, rotateL, rotateR, bit, popCount, xor)

import Foreign.C.String (peekCString)
import Foreign.Storable (peek, Storable(..))
import Foreign.Ptr (castPtr, nullPtr, plusPtr, Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray, copyArray)

-- * Utility Functions

-- | Concatenate a list of lists (like 'concat', via explicit foldr).
flatten ∷ [[α]] → [α]
flatten xs = (\z n → foldr (\x y → foldr z y x) n xs) (:) []

infixl 7 ⌃
infixl 5 ⌄
infixl 1 ⌦
infixl 1 ⌫
infixl 4 ⚟
infixl 4 ⚞
infixl 4 ⊚
infixr 5 ⊘
infixr 7 ⊙

-- | Restrict a float to [0, 1].
clamp01 ∷ Float → Float
clamp01 x
    | x < 0    = 0
    | x > 1    = 1
    | otherwise = x

-- | Clamp a value to [minVal, maxVal].
clamp ∷ Ord a ⇒ a → a → a → a
clamp minVal maxVal x
    | x < minVal = minVal
    | x > maxVal = maxVal
    | otherwise  = x

-- * Bitwise Operators

-- | Bitwise AND (.&.).
(⌃) ∷ B.Bits a ⇒ a → a → a
(⌃) = (B..&.)
{-# INLINE (⌃) #-}

-- | Bitwise OR (.|.).
(⌄) ∷ B.Bits a ⇒ a → a → a
(⌄) = (B..|.)
{-# INLINE (⌄) #-}

-- * Functor Operators

-- | Const-replace: @a ⚟ fb = fmap (const a) fb@ (i.e. '<$').
(⚟) ∷ Functor f ⇒ a → f b → f a
(⚟) = (F.<$)
{-# INLINE (⚟) #-}

-- | Flipped const-replace: @fa ⚞ b = fmap (const b) fa@ (i.e. '$>').
(⚞) ∷ Functor f ⇒ f a → b → f b
(⚞) = (F.$>)
{-# INLINE (⚞) #-}

-- | fmap ('<$>').
(⊚) ∷ Functor f ⇒ (a → b) → f a → f b
(⊚) = (F.<$>)
{-# INLINE (⊚) #-}

-- * Monadic Operators

-- | Monadic bind ('>>=').
(⌦) ∷ Monad m ⇒ ∀ a b. m a → (a → m b) → m b
(⌦) = (P.>>=)
{-# INLINE (⌦) #-}

-- | Reverse bind ('=<<').
(⌫) ∷ Monad m ⇒ (a → m b) → m a → m b
(⌫) = (P.=<<)
{-# INLINE (⌫) #-}

-- * FilePath Operators

-- | FilePath join ('</>').
(⊘) ∷ FilePath → FilePath → FilePath
(⊘) = (FP.</>)
{-# INLINE (⊘) #-}

-- | FilePath extension ('<.>').
(⊙) ∷ FilePath → String → FilePath
(⊙) = (FP.<.>)
{-# INLINE (⊙) #-}

-- * Instances

instance Serialize Text where
    put txt = put $ T.encodeUtf8 txt
    get     = fmap T.decodeUtf8 S.get

instance (Serialize k, Serialize v, Eq k, Hashable k)
    ⇒ Serialize (HM.HashMap k v) where
    put = put . HM.toList
    get = HM.fromList <$> S.get



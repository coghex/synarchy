{-# LANGUAGE ExplicitForAll #-}
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
-- the prelude is modified for
-- custom symbols and typesynonyms,
-- no logic goes in here
import Prelude.Unicode
import qualified Prelude as P
import Prelude hiding ((>>=), (=<<))
import qualified Data.Text as T
import qualified Data.Bits as B
import qualified Data.Functor as F
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

-- this function should be in prelude
flatten ∷ [[α]] → [α]
flatten xs = (\z n → foldr (\x y → foldr z y x) n xs) (:) []

-- fixity
infixl 7 ⌃
infixl 5 ⌄
infixl 1 ⌦
infixl 1 ⌫
infixl 4 ⚟
infixl 4 ⚞
infixl 4 ⊚
infixr 5 ⊘
infixr 7 ⊙

-- bitwise operators
(⌃) ∷ B.Bits a ⇒ a → a → a
(⌃) = (B..&.)
{-# INLINE (⌃) #-}
(⌄) ∷ B.Bits a ⇒ a → a → a
(⌄) = (B..|.)
{-# INLINE (⌄) #-}
-- functor sequencing
(⚟) ∷ Functor f ⇒ a → f b → f a
(⚟) = (F.<$)
{-# INLINE (⚟) #-}
(⚞) ∷ Functor f ⇒ f a → b → f b
(⚞) = (F.$>)
{-# INLINE (⚞) #-}
(⊚) ∷ Functor f ⇒ (a → b) → f a → f b
(⊚) = (F.<$>)
{-# INLINE (⊚) #-}
-- shortens monadic sequencing
(⌦) ∷ Monad m ⇒ ∀ a b. m a → (a → m b) → m b
(⌦) = (P.>>=)
{-# INLINE (⌦) #-}
(⌫) ∷ Monad m ⇒ (a → m b) → m a → m b
(⌫) = (P.=<<)
{-# INLINE (⌫) #-}
-- filepath operators
(⊘) ∷ FilePath → FilePath → FilePath
(⊘) = (FP.</>)
{-# INLINE (⊘) #-}
(⊙) ∷ FilePath → String → FilePath
(⊙) = (FP.<.>)
{-# INLINE (⊙) #-}

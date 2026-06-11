{-# LANGUAGE UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'MaterialId' lives in its own deliberately NON-Strict module.
--
--   'derivingUnbox' splices a 'Data.Vector.Generic.elemseq' clause
--   that binds the vector proxy argument; under @{-# LANGUAGE
--   Strict #-}@ that binder gets an implicit bang, which forces the
--   @undefined@ proxy 'Data.Vector.Generic.replicate' / 'singleton'
--   pass — so e.g. @VU.replicate n (MaterialId 1)@ crashed with
--   @Prelude.undefined@ while the instance lived in the Strict
--   "World.Material". Keep this module lazy; the newtype has no
--   strictness of its own to lose.
module World.Material.Id
    ( MaterialId(..)
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData(..))
import Data.Vector.Unboxed.Deriving (derivingUnbox)

newtype MaterialId = MaterialId { unMaterialId ∷ Word8 }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NFData)
    deriving anyclass (Serialize)
    deriving stock (Generic)

derivingUnbox "MaterialId"
    [t| MaterialId -> Word8 |]
    [| unMaterialId |]
    [| MaterialId |]

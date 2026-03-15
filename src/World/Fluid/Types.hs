{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Types
    ( FluidType(..)
    , FluidCell(..)
    , IceCell(..)
    , IceMap
    , emptyIceMap
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.Vector as V

data FluidType = Ocean | Lake | River | Lava
    deriving (Show, Eq)
instance NFData FluidType where
    rnf Ocean = ()
    rnf Lake  = ()
    rnf River = ()
    rnf Lava  = ()

-- | Per-column fluid info, stored in LoadedChunk.
--   Only present for tiles that have fluid above them.
data FluidCell = FluidCell
    { fcType    ∷ !FluidType   -- ^ What kind of fluid
    , fcSurface ∷ !Int         -- ^ Z-level of the fluid surface
    } deriving (Show, Eq)
instance NFData FluidCell where
    rnf (FluidCell t s) = rnf t `seq` rnf s

-- | Per-column ice overlay, stored in LoadedChunk alongside FluidMap.
--   Ice sits on top of terrain or fluid (frozen ocean/lake).
--   Only present for tiles where summer mean temperature < 0°C.
data IceCell = IceCell
    { icSurface ∷ !Int   -- ^ Z-level of ice surface (top)
    } deriving (Show, Eq)
instance NFData IceCell where
    rnf (IceCell s) = rnf s

-- | Per-column ice overlay map, parallel to FluidMap.
type IceMap = V.Vector (Maybe IceCell)

-- | Empty ice map (no ice anywhere in this chunk).
emptyIceMap ∷ IceMap
emptyIceMap = V.replicate 256 Nothing

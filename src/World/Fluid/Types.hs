{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Fluid.Types
    ( FluidType(..)
    , FluidCell(..)
    , IceMode(..)
    , IceCell(..)
    , IceMap
    , emptyIceMap
    , IceLevelGrid(..)
    , emptyIceLevelGrid
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

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

-- | Ice deposition mode.
data IceMode = BasinIce   -- ^ Flat sheet filling a valley/basin
             | DrapeIce   -- ^ Thin coating on terrain above basin level
    deriving (Show, Eq)
instance NFData IceMode where
    rnf BasinIce = ()
    rnf DrapeIce = ()

-- | Per-column ice overlay, stored in LoadedChunk alongside FluidMap.
--   Ice sits on top of terrain or fluid (frozen ocean/lake).
data IceCell = IceCell
    { icSurface ∷ !Int      -- ^ Z-level of ice surface (top)
    , icMode    ∷ !IceMode  -- ^ Basin (flat fill) or drape (thin coat)
    } deriving (Show, Eq)
instance NFData IceCell where
    rnf (IceCell s m) = rnf s `seq` rnf m

-- | Per-column ice overlay map, parallel to FluidMap.
type IceMap = V.Vector (Maybe IceCell)

emptyIceMap ∷ IceMap
emptyIceMap = V.replicate (16 * 16) Nothing  -- chunkSize² (can't import Chunk.Types: circular)

-- | Coarse-resolution ice surface level grid, computed once during
--   timeline build using fillDepressions restricted to frozen cells.
--   Same geometry as ElevGrid (gridW, spacing).
data IceLevelGrid = IceLevelGrid
    { ilGridW   ∷ !Int              -- ^ Grid dimension (same as ElevGrid)
    , ilSpacing ∷ !Int              -- ^ Tile spacing between samples
    , ilLevel   ∷ !(VU.Vector Int)  -- ^ Ice fill level per sample (-1 = no basin)
    } deriving (Show, Eq, Generic, NFData)
instance Serialize IceLevelGrid where
    put (IceLevelGrid w s v) = do
        Serialize.put w
        Serialize.put s
        Serialize.put (VU.toList v)
    get = do
        w ← Serialize.get
        s ← Serialize.get
        xs ← Serialize.get
        pure (IceLevelGrid w s (VU.fromList xs))

emptyIceLevelGrid ∷ IceLevelGrid
emptyIceLevelGrid = IceLevelGrid 0 1 VU.empty

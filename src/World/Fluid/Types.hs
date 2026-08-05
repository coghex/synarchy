{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module World.Fluid.Types
    ( FluidType(..)
    , FluidCell(..)
    , IceMode(..)
    , IceCell(..)
    , IceMap
    , emptyIceMap
    , IceLevelGrid(..)
    , emptyIceLevelGrid
    , renderedSurfaceZ
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

data FluidType = Ocean | Lake | River | Lava
    deriving (Show, Eq, Generic, Serialize)
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

-- | THE rendered-surface rule (#1112): given a column's terrain top z
--   and whatever fluid cell sits over it, the z the column's surface
--   renders at.
--
--   River renders FLAT at the fluid surface, deliberately hiding a
--   terrain protrusion above it — the carved channel is allowed bumps
--   and the water plane must not break over them. Every other fluid
--   type renders at @max terrain fluid@; a dry column renders at its
--   terrain top.
--
--   This is the ONLY place the River-versus-other decision is written.
--   Callers: 'World.Generate.Chunk.Fluid.mkSurfaceMap' (generation),
--   'Sim.Thread.emitWorldDirtyFluids' (sim writeback), and
--   'World.Edit.Apply' (@WeDeleteTile@, @WeAddTile@, @WeSetFluidTile@,
--   @WeSetFluidSnapshot@, @recomputeColumnSurface@). Hand-written
--   copies used to disagree: the dig and carve paths applied a bare
--   @max@, so digging a River tile whose terrain protrudes above the
--   water rendered the protrusion, and a chunk-eviction replay wrote
--   that divergence back every time.
--
--   The terrain argument must be the TERRAIN top
--   (@lcTerrainSurfaceMap@), never a previously rendered surface —
--   feeding back a rendered value keeps a superseded fluid cell's
--   height alive.
renderedSurfaceZ ∷ Int → Maybe FluidCell → Int
renderedSurfaceZ terrainZ Nothing = terrainZ
renderedSurfaceZ terrainZ (Just fc)
    | fcType fc ≡ River = fcSurface fc
    | otherwise         = max terrainZ (fcSurface fc)

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

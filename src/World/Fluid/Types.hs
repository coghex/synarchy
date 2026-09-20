{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module World.Fluid.Types
    ( FluidType(..)
    , FluidCell(..)
    , fluidCellAtZ
    , fluidSurfaceCeilZ
    , fluidSurfaceFloorZ
    , fluidTopLevel
    , fluidVolumeOverTerrain
    , IceMode(..)
    , IceCell(..)
    , IceMap
    , emptyIceMap
    , IceLevelGrid(..)
    , renderedSurfaceZ
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Fluid.Exact
    ( exactSurfaceOfZ, exactSurfaceCeilZ, exactSurfaceFloorZ, exactTopLevel
    , exactVolumeOverTerrain )

data FluidType = Ocean | Lake | River | Lava
    deriving (Show, Eq, Generic, Serialize)
instance NFData FluidType where
    rnf Ocean = ()
    rnf Lake  = ()
    rnf River = ()
    rnf Lava  = ()

-- | Per-column fluid info, stored in LoadedChunk.
--   Only present for tiles that have fluid above them.
--
--   Since #2520 the height is THE authoritative fluid surface for every
--   'FluidType', Ocean included: a signed fixed-point ABSOLUTE surface
--   in eighths of a z-level ("World.Fluid.Exact"). There is no integer
--   surface beside it and no ocean-only branch; every integer consumer
--   reads one of the named compatibility views below.
data FluidCell = FluidCell
    { fcType         ∷ !FluidType
      -- ^ What kind of fluid.
    , fcExactSurface ∷ !Int
      -- ^ Absolute fluid surface in exact units
      --   ('World.Fluid.Exact.fluidUnitsPerZ' per z). A whole-z plane
      --   is @z * 8@; a partial one carries its own remainder and
      --   survives activation, writeback, deactivation, save and load
      --   without being rounded to a level.
    } deriving (Show, Eq)
instance NFData FluidCell where
    rnf (FluidCell t s) = rnf t `seq` rnf s

-- | A fluid cell whose surface is a WHOLE z — the brim of level @z@,
--   fill level 8, exact surface @z * 8@. Every generated plane, every
--   ocean fill and every whole-level player edit produces one of these;
--   only the simulation and the saves it writes carry a remainder.
fluidCellAtZ ∷ FluidType → Int → FluidCell
fluidCellAtZ t z = FluidCell t (exactSurfaceOfZ z)
{-# INLINE fluidCellAtZ #-}

-- | Compatibility view: the lowest whole z at or above this cell's
--   exact surface. THE integer height every pre-#2520 consumer reads,
--   so a partially filled top level still renders and gates as one
--   occupied z.
fluidSurfaceCeilZ ∷ FluidCell → Int
fluidSurfaceCeilZ = exactSurfaceCeilZ . fcExactSurface
{-# INLINE fluidSurfaceCeilZ #-}

-- | Compatibility view: the highest COMPLETELY filled whole z of this
--   cell, i.e. the floor of its exact surface.
fluidSurfaceFloorZ ∷ FluidCell → Int
fluidSurfaceFloorZ = exactSurfaceFloorZ . fcExactSurface
{-# INLINE fluidSurfaceFloorZ #-}

-- | How full this cell's TOP z is, in @1 .. 8@ (a whole-z plane is 8).
fluidTopLevel ∷ FluidCell → Int
fluidTopLevel = exactTopLevel . fcExactSurface
{-# INLINE fluidTopLevel #-}

-- | The exact same-footprint volume this cell holds over a terrain top.
fluidVolumeOverTerrain ∷ Int → FluidCell → Int
fluidVolumeOverTerrain terrainZ = exactVolumeOverTerrain terrainZ . fcExactSurface
{-# INLINE fluidVolumeOverTerrain #-}

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
--   Since #2520 the fluid height it reads is the INTEGER CEILING view
--   ('fluidSurfaceCeilZ') of the cell's exact surface. Rendering stays
--   whole-z in this slice (DFL-3/DFL-4 own fractional placement), and
--   the ceiling is what keeps a partially filled top level visible
--   instead of vanishing below its own brim.
renderedSurfaceZ ∷ Int → Maybe FluidCell → Int
renderedSurfaceZ terrainZ Nothing = terrainZ
renderedSurfaceZ terrainZ (Just fc)
    | fcType fc ≡ River = fluidSurfaceCeilZ fc
    | otherwise         = max terrainZ (fluidSurfaceCeilZ fc)

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

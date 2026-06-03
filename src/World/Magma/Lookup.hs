{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Magma.Lookup
    ( lookupNearSources
    , lavaAt
    , effectiveLavaAt
    , sourceContains
    , toChunkCoord
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Internal (wrapChunkCoordU)
import World.Magma.Types
    ( MagmaSource(..)
    , VolcanoCtx(..)
    , MagmaOverlay(..)
    )
import World.Magma.Field (mantleZ)
import World.Magma.Shape (pointInShape)

-- | Convert a global tile coordinate to its chunk coordinate. Uses
--   floor-division so negative coords land in the chunk on their own
--   side of the seam.
{-# INLINE toChunkCoord #-}
toChunkCoord ∷ Int → Int → ChunkCoord
toChunkCoord gx gy =
    ChunkCoord (gx `floorDiv` chunkSize) (gy `floorDiv` chunkSize)
  where
    floorDiv a b = let (q, r) = a `divMod` b
                   in if r < 0 then q - 1 else q

-- | Indices into 'vcSources' of every source whose bbox-padded
--   footprint covers the chunk containing @(gx, gy)@. Wraps the
--   chunk coord into the world's canonical u-axis range so off-world
--   query coords still hit the right index entry.
lookupNearSources ∷ VolcanoCtx → Int → Int → [Int]
lookupNearSources ctx gx gy =
    let cc = wrapChunkCoordU (vcWorldSize ctx) (toChunkCoord gx gy)
    in HM.lookupDefault [] cc (vcIndex ctx)

-- | Pure lava test: True iff @(gx, gy, z)@ is below the mantle
--   ceiling, OR inside any chamber/chute shape of a nearby source.
lavaAt ∷ VolcanoCtx → Int → Int → Int → Bool
lavaAt ctx gx gy z =
    z < mantleZ ctx gx gy
    ∨ any (sourceContains ctx gx gy z) (lookupNearSources ctx gx gy)

-- | True iff source at index @i@ contains @(gx, gy, z)@ in any of
--   its shapes. Used by 'lavaAt' and re-exported for
--   'World.Magma.Init.discoverChunkLava'.
sourceContains ∷ VolcanoCtx → Int → Int → Int → Int → Bool
sourceContains ctx gx gy z i =
    let s = vcSources ctx V.! i
        ws = vcWorldSize ctx
    in any (pointInShape ws gx gy z) (msShapes s)

-- | The only function the rest of the engine should call. Future
--   precedence layers (cooled basalt, sim writes, dig reveal) are
--   inserted here without touching call sites.
effectiveLavaAt ∷ MagmaOverlay → VolcanoCtx → Int → Int → Int → Bool
effectiveLavaAt overlay ctx gx gy z =
    case HM.lookup (gx, gy) (moSurface overlay) of
        Just _  → True
        Nothing → lavaAt ctx gx gy z

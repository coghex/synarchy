{-# LANGUAGE Strict #-}
module World.Magma.Lookup
    ( lookupNearSources
      -- Kept exported with no consumer yet (#1119): its own Haddock
      -- below names it the engine's lava-query entry point and states
      -- that the overlay parameter exists so a future precedence layer
      -- is inserted here rather than at every call site. Un-exporting
      -- it would cascade under -Werror into deleting lavaAt,
      -- sourceContains and, through them, World.Magma.Field's mantleZ
      -- and sumHotspots hotspot-uplift model — far past narrowing an
      -- export list.
    , effectiveLavaAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Coordinates (globalToChunk)
import World.Fluid.Internal (wrapChunkCoordU)
import World.Magma.Types
    ( MagmaSource(..)
    , VolcanoCtx(..)
    , MagmaOverlay
    )
import World.Magma.Field (mantleZ)
import World.Magma.Shape (pointInShape)

-- | Convert a global tile coordinate to its chunk coordinate.
--   Delegates to the shared 'globalToChunk', whose floor-division
--   lands negative coords in the chunk on their own side of the seam;
--   the local index it also returns is unused here.
{-# INLINE toChunkCoord #-}
toChunkCoord ∷ Int → Int → ChunkCoord
toChunkCoord gx gy = fst (globalToChunk gx gy)

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
--   its shapes. Used by @lavaAt@.
sourceContains ∷ VolcanoCtx → Int → Int → Int → Int → Bool
sourceContains ctx gx gy z i =
    let s = vcSources ctx V.! i
        ws = vcWorldSize ctx
    in any (pointInShape ws gx gy z) (msShapes s)

-- | The only function the rest of the engine should call. The overlay
--   currently contributes no precedence of its own, so this is exactly
--   @lavaAt@; it keeps the overlay parameter so future precedence
--   layers (cooled basalt, sim writes, dig reveal) are inserted here
--   without touching call sites.
effectiveLavaAt ∷ MagmaOverlay → VolcanoCtx → Int → Int → Int → Bool
effectiveLavaAt _overlay ctx gx gy z = lavaAt ctx gx gy z

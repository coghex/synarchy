{-# LANGUAGE Strict #-}
-- | Which flora instances a chunk contributes to a frame, and with
--   which texture (#1856).
--
-- The other half of the shared projection boundary
-- ('World.Render.FloraProjection' places a sprite; this decides which
-- sprites there are and what each is drawing). Both the renderer
-- ("World.Render.Quads") and the Chop selection oracle
-- ("World.Flora.HitTest") enumerate through here, so a picker can never
-- consider a plant the renderer skipped — or size one from a growth
-- stage the renderer is not showing.
--
-- Three live values are resolved here rather than taken on trust, all
-- of them from "World.Render.Quads"' own flora loop:
--
--   * the terrain-derived @actualZ@ (@findTopSolid@), which overrides
--     the stored 'fiZ' — a designation record's captured z is a marker
--     hint, never the placement;
--   * the harvested/depleted texture swap, read by INSTANCE id (#1854);
--   * the growth-stage texture for everything else.
module World.Render.FloraDraws
    ( FloraDraw(..)
    , chunkFloraDraws
    , findTopSolid
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import World.Chunk.Types (ChunkCoord, ColumnTiles(..), LoadedChunk(..)
                         , columnIndex)
import World.Flora.Harvest (FloraHarvests)
import World.Flora.Render (resolveFloraTexture)
import World.Flora.Types
import World.Generate (chunkToGlobal)

-- | One instance the renderer would draw this frame.
data FloraDraw = FloraDraw
    { fdInstance ∷ !FloraInstance
      -- ^ The instance with 'fiZ' already replaced by the LIVE
      --   terrain-derived surface z, which is what the sprite is placed
      --   at. Pass this to 'World.Render.FloraProjection.floraGeom'.
    , fdGX       ∷ !Int  -- ^ global tile x, in the chunk's own frame
    , fdGY       ∷ !Int  -- ^ global tile y
    , fdTexture  ∷ !TextureHandle
    }

-- | Every drawable flora instance in one loaded chunk, resolved exactly
--   as "World.Render.Quads" resolves them: empty columns skipped, the
--   depleted texture substituted for a harvested plant, and a species
--   with no art for its current state dropped (handle 0).
chunkFloraDraws
    ∷ FloraCatalog
    → Int                                 -- ^ days per year
    → Int                                 -- ^ absolute day
    → FloraHarvests                       -- ^ live regrowth timers
    → ChunkCoord
    → LoadedChunk
    → [FloraDraw]
chunkFloraDraws floraCat daysPerYear absDay harvests coord lc =
    [ FloraDraw { fdInstance = inst'
                , fdGX = gx, fdGY = gy, fdTexture = texHandle }
    | inst ← fcdInstances (lcFlora lc)
    , let tileX = fromIntegral (fiTileX inst)
          tileY = fromIntegral (fiTileY inst)
          col   = lcTiles lc V.! columnIndex tileX tileY
          actualZ = findTopSolid col
          inst' = inst { fiZ = actualZ }
          (gx, gy) = chunkToGlobal coord tileX tileY
          mHarvest = lookupSpecies (fiSpecies inst) floraCat ⌦ fsHarvest
          harvested = isJust mHarvest
                    ∧ HM.member (fiInstanceId inst) harvests
          texHandle = case (harvested, mHarvest) of
              (True, Just fh) → fhHarvestedTexture fh
              _ → resolveFloraTexture floraCat daysPerYear absDay inst'
    , actualZ > minBound
    , texHandle ≢ TextureHandle 0
    ]

-- | The topmost non-empty z in a column — the actual rendered surface,
--   never a surface map's cached opinion of it. Moved here from
--   "World.Render.Quads" (#1856) so the picker reads the same one.
findTopSolid ∷ ColumnTiles → Int
findTopSolid col =
    let mats = ctMats col
        len  = VU.length mats
        go i | i < 0                 = minBound
             | mats VU.! i ≢ 0       = ctStartZ col + i
             | otherwise             = go (i - 1)
    in go (len - 1)

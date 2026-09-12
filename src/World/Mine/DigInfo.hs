{-# LANGUAGE Strict #-}
-- | What a designated mine tile is made of, how fast each tool cuts
--   it, and whether the spoil it would produce has anywhere to go.
--
--   ONE implementation, two readers. @world.getDigInfoAt@ answers this
--   for a single tile the AI has already picked, and
--   @world.nearestWorkableMineDesignation@ (#2538) asks it of each
--   candidate while walking outward for the nearest designation a
--   worker can actually dig. The walk's whole contract is that it
--   applies the SAME rejection rules the per-tile query reports — a
--   worker must never select a tile the per-tile query would have
--   refused, nor skip one it would have accepted — so those rules live
--   here rather than being written twice.
--
--   Every read is of already-resident state: an unloaded chunk answers
--   'Nothing' rather than queueing a load, which is what keeps ranking
--   many candidates from becoming a chunk-load amplifier. An unloaded
--   candidate is therefore indistinguishable from an undesignated one
--   here, and both are simply not workable.
module World.Mine.DigInfo
    ( DigInfo(..)
    , digInfoAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ColumnTiles(..), LoadedChunk(..), columnIndex)
import World.Generate.Coordinates (canonicalTileFrame)
import World.Material
    (MaterialId(..), MaterialProps(..), MaterialRegistry, getMaterialProps
    , materialIdByName)
import World.Mine.Types (MineDesignation(..), MineDesignations)
import World.Spoil.Logic (spoilBlockedAt)
import World.Spoil.Types (SpoilPiles)
import World.Tile.Types (WorldTileData, lookupChunk)

-- | The dig-relevant facts about one designated tile.
data DigInfo = DigInfo
    { diMaterial     ∷ !Word8
      -- ^ Raw id of the material at the designation's z — the column
      --   can expose a different one as digging drops through strata,
      --   so this is a snapshot, not a property of the designation.
    , diPickSpeed    ∷ !Float
      -- ^ Dig-rate multiplier with a pick; 0 means a pick cannot work
      --   this material at all.
    , diShovelSpeed  ∷ !Float
      -- ^ The same for a shovel.
    , diSpoilBlocked ∷ !Bool
      -- ^ True when this material produces spoil and the piles around
      --   the tile have no room for it. The dig command would refuse
      --   every tick, so a worker must not take the job at all.
    } deriving (Show, Eq)

-- | 'DigInfo' for a global tile coord, which may be ANY u-alias of the
--   designated tile (#1175) — the designation map, the tile store and
--   the dig command itself all resolve the same stored key, and so does
--   this. 'Nothing' when the tile is not designated, its chunk is not
--   resident, or the designation's z has fallen outside the column.
digInfoAt ∷ MaterialRegistry → WorldTileData → MineDesignations → SpoilPiles
          → Int              -- ^ world size in chunks (0 = no wrap)
          → (Int, Int)       -- ^ global tile, any u-alias
          → Maybe DigInfo
digInfoAt registry td desigs piles worldSize (rawGX, rawGY) = do
    let (coord, (lx, ly), (dgx, dgy)) =
            canonicalTileFrame worldSize rawGX rawGY
        gx = rawGX + dgx
        gy = rawGY + dgy
    md ← HM.lookup (gx, gy) desigs
    lc ← lookupChunk coord td
    let col   = lcTiles lc V.! columnIndex lx ly
        relZ  = mdZ md - ctStartZ col
        digZ  = mdZ md
    if relZ < 0 ∨ relZ ≥ VU.length (ctMats col)
        then Nothing
        else do
            let matId = ctMats col VU.! relZ
                props = getMaterialProps registry (MaterialId matId)
            pure DigInfo
                { diMaterial     = matId
                , diPickSpeed    = mpPickSpeed props
                , diShovelSpeed  = mpShovelSpeed props
                  -- Blocked check from the tile CENTRE; the per-tick
                  -- gate re-checks with the digger's real position.
                , diSpoilBlocked = blocked props digZ (gx, gy)
                }
  where
    blocked props digZ tile = case mpDigSpoil props of
        Nothing        → False
        Just spoilName → case materialIdByName registry spoilName of
            Nothing      → False
            Just spoilId → spoilBlockedAt td desigs piles spoilId digZ
                               ( fromIntegral (fst tile) + 0.5
                               , fromIntegral (snd tile) + 0.5 )
                               tile

{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Interactive flora harvest state (#94).
--
--   Harvested tiles live in a WORLD-LEVEL sparse map, not in the chunk's
--   'lcFlora' — chunks are evicted and regenerated (flora placement is
--   deterministic), so mutable state kept there would be wiped by
--   eviction. This map is the same shape as mine/construct designations:
--   tile-keyed, written via the Lua API + regrowth tick, read by the
--   render pass and queries, persisted per page ('wpsFloraHarvests').
--
--   An entry (gx, gy) → t means "this tile was harvested; t GAME-seconds
--   of regrowth remain". When the timer runs out the entry is removed and
--   the tile's flora renders (and harvests) normally again. No entry =
--   harvestable (if the species is).
module World.Flora.Harvest
    ( FloraHarvests
    , emptyFloraHarvests
    , tickFloraHarvests
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM

-- | (gx, gy) → regrowth GAME-seconds remaining.
type FloraHarvests = HM.HashMap (Int, Int) Float

emptyFloraHarvests ∷ FloraHarvests
emptyFloraHarvests = HM.empty

-- | Advance every regrowth timer by @dtGame@ game-seconds, dropping the
--   entries that finished. Returns the new map and whether anything
--   regrew (the caller invalidates the quad cache so the plant's normal
--   texture comes back).
tickFloraHarvests ∷ Float → FloraHarvests → (FloraHarvests, Bool)
tickFloraHarvests dtGame hs
    | HM.null hs = (hs, False)
    | otherwise  =
        let hs' = HM.mapMaybe
                    (\t → let t' = t - dtGame
                          in if t' ≤ 0 then Nothing else Just t')
                    hs
        in (hs', HM.size hs' < HM.size hs)

{-# LANGUAGE Strict #-}
-- | Interactive flora harvest state (#94, re-keyed by #1854).
--
--   Harvest timers live in a WORLD-LEVEL sparse map, not in the chunk's
--   'lcFlora' — chunks are evicted and regenerated (flora placement is
--   deterministic), so mutable state kept there would be wiped by
--   eviction. Written via the Lua API + regrowth tick, read by the
--   render pass and queries, persisted per page ('wpsFloraHarvests').
--
--   #1854: keyed by 'World.Flora.Identity.FloraInstanceId', not by
--   tile. An entry @iid → t@ means "THIS plant was harvested; @t@
--   GAME-seconds of regrowth remain". When the timer runs out the entry
--   is removed and that plant renders (and harvests) normally again. No
--   entry = harvestable (if the species is). Under the old tile key one
--   berry bush's timer made every harvestable co-tenant on the tile
--   read and draw as depleted ("World.Render.Quads"); per-instance
--   keying is exactly what stops that.
--
--   'PendingFloraHarvests' is the ONE remaining tile-keyed shape, and
--   it is explicitly NOT a second authority (#1854 requirement 14): it
--   holds pre-#1854 saved timers whose chunk was not loaded when the
--   save was read. It is persisted so repeated save/load cannot
--   silently discard it, and "World.Flora.Designation" drains it into
--   the real map as each chunk arrives — expanding one legacy tile
--   timer onto EVERY harvestable instance on that tile with the same
--   remaining time, which is precisely the observable behaviour the
--   tile-keyed timer used to produce. Nothing may answer a harvest,
--   render or query decision from it.
module World.Flora.Harvest
    ( FloraHarvests
    , PendingFloraHarvests
    , emptyFloraHarvests
    , emptyPendingFloraHarvests
    , tickFloraHarvests
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Flora.Identity (FloraInstanceId)

-- | flora instance → regrowth GAME-seconds remaining.
type FloraHarvests = HM.HashMap FloraInstanceId Float

-- | Pre-#1854 tile-keyed timers awaiting the instances to expand onto
--   (see the module header). Deferred, never authoritative.
type PendingFloraHarvests = HM.HashMap (Int, Int) Float

emptyFloraHarvests ∷ FloraHarvests
emptyFloraHarvests = HM.empty

emptyPendingFloraHarvests ∷ PendingFloraHarvests
emptyPendingFloraHarvests = HM.empty

-- | Advance every regrowth timer by @dtGame@ game-seconds, dropping the
--   entries that finished. Returns the new map and whether anything
--   regrew (the caller invalidates the quad cache so the plant's normal
--   texture comes back).
--
--   Pending legacy entries are deliberately NOT ticked: an unresolved
--   entry must expand onto its instances with the remaining time it was
--   saved with, and a timer that quietly ran down while its chunk was
--   unloaded would be a runtime decision taken from tile-keyed data.
tickFloraHarvests ∷ Float → FloraHarvests → (FloraHarvests, Bool)
tickFloraHarvests dtGame hs
    | HM.null hs = (hs, False)
    | otherwise  =
        let hs' = HM.mapMaybe
                    (\t → let t' = t - dtGame
                          in if t' ≤ 0 then Nothing else Just t')
                    hs
        in (hs', HM.size hs' < HM.size hs)

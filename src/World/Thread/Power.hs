{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-page power-network tick (#360).
--
--   Runs beside the world clock ('World.Thread.Time.tickWorldTime'), once
--   per visible page per world-thread iteration, in GAME-seconds — the
--   same clock flora regrowth and item cooling follow, so a network's
--   charge tracks a "simulated day" under a time-scale fast-forward and
--   freezes with the pause flag. The issue's own text calls out living
--   "on the sim thread beside the fluid sim", but that thread
--   ('Sim.Thread') mirrors per-chunk terrain/fluid CELL data into its own
--   state specifically for fluid's per-cell throughput needs — power only
--   needs the sparse, already-authoritative wire-tile set and the small
--   node registry the world thread already owns, so ticking here avoids
--   rebuilding that mirroring machinery for a much smaller problem.
--
--   Every tracked power node lives on exactly one page (see
--   Building.Types.biPage), so the wire tiles + nodes gathered here are
--   already page-scoped — no cross-page filtering needed beyond that.
module World.Thread.Power
    ( tickPowerNetworks
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Power.Types (PowerNodes(..))
import Power.Network (tickPowerNodes, wireTilesOn, positionsOf)
import World.Time.Types (WorldTime, worldTimeToSunAngle)
import World.Types (WorldPageId, WorldState(..))

-- | Advance one page's power networks by @dtGame@ game-seconds. No-op
--   when the page has no power nodes at all (the common case while the
--   power epic is unused) — cheap enough to check every tick without a
--   full wire-tile scan.
tickPowerNetworks ∷ EngineEnv → WorldPageId → WorldState → Float → IO ()
tickPowerNetworks env pageId ws dtGame = do
    nodes ← readIORef (wsPowerNodesRef ws)
    when (not (HM.null (pnsNodes nodes))) $ do
        wt  ← readIORef (wsTimeRef ws)
        td  ← readIORef (wsTilesRef ws)
        bm  ← readIORef (buildingManagerRef env)
        let sunAngle    = worldTimeToSunAngle (wt ∷ WorldTime)
            wireTiles   = wireTilesOn td
            positions   = positionsOf pageId bm nodes
            drainByNode = HM.empty
              -- ^ #361's real requires_power consumers aren't wired up
              --   yet; tickPowerNodes already accepts this map so the
              --   balance math (charge/hold/brownout) is fully covered
              --   by Test.Headless.Power.Network today.
            nodes'      = tickPowerNodes sunAngle drainByNode dtGame
                                          wireTiles positions nodes
        atomicModifyIORef' (wsPowerNodesRef ws) (const (nodes', ()))

{-# LANGUAGE Strict #-}
-- | The building command drain. Narrowed to the @units-buildings-combat@
--   building half (#896, epic #537): it takes 'BuildingCapability' plus
--   the strictly narrower values it needs (the shared logger, the
--   world/sim view and — since #1087 — the content registries) rather
--   than an 'Engine.Core.State.EngineEnv',
--   so its only caller — "Unit.Thread", which runs this drain on the
--   unit thread because there is no building thread
--   ('docs/engineenv_capability_inventory.md' §2.2) — no longer hands
--   its whole environment across the boundary.
module Building.Thread.Command
    ( processAllBuildingCommands
    , applyBuildingSpawn
    ) where

import UPrelude
import Engine.Core.Capability.Building (BuildingCapability(..))
import Engine.Core.Capability.ContentRegistriesView (ContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import World.State.Types (WorldManager(..))
import World.Page.Types (WorldPageId)
import Building.Types
import Building.Knowledge (SeedTrigger(..), seedTriggerFor)
import Building.Knowledge.Live
    ( containerObserver, forgetAllContainers, forgetContainerEverywhere
    , markPendingSeed, sweepPendingSeeds )
import Building.Command.Types (BuildingCommand(..))
import Power.Live (retirePowerNodeEverywhere)

-- | Drain the building command queue in one pass. Called from the
--   unit thread's tick so we don't need a dedicated building thread —
--   buildings don't have per-tick logic, just point-in-time spawns
--   and destroys.
--
--   Takes the content-registries view as well since #1087: an
--   instant-built storage building seeds its container-knowledge record
--   when it reaches Built, and weighing that observation needs the item
--   defs. This module names no registry accessor itself — it carries
--   the record only to build 'Building.Knowledge.Live.containerObserver'
--   — so #1896 changed its type here without changing what it does.
processAllBuildingCommands ∷ IORef LoggerState → WorldSimCapability
                           → ContentRegistriesViewCapability
                           → BuildingCapability → IO ()
processAllBuildingCommands logRef sim reg bld = do
    drain
    -- #1087: after the queue is empty, give every container THIS
    -- SESSION placed and is still watching a chance to have reached
    -- Built. Scoped to that session-local set, never to every
    -- already-built container, so a loaded one is never seeded as
    -- though the player had just watched it go up.
    sweepPendingSeeds (containerObserver bld sim reg)
  where
    drain = do
        mCmd ← Q.tryReadQueue (bcBuildingQueue bld)
        case mCmd of
            Just cmd → handleBuildingCommand logRef sim reg bld cmd >> drain
            Nothing  → return ()

handleBuildingCommand ∷ IORef LoggerState → WorldSimCapability
                      → ContentRegistriesViewCapability → BuildingCapability
                      → BuildingCommand → IO ()
handleBuildingCommand logRef sim reg bld
                      (BuildingSpawn bid defName gx gy gz pageId) = do
    logger ← readIORef logRef
    applyBuildingSpawn logger sim reg bld bid defName gx gy gz pageId

handleBuildingCommand _ sim _ bld (BuildingDestroy bid) = do
    atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm →
        let cleared = if bmSelected bm ≡ Just bid
                      then Nothing
                      else bmSelected bm
        in (bm { bmInstances = HM.delete bid (bmInstances bm)
               , bmSelected  = cleared }, ())
    -- #1087: demolishing a container drops the player's memory of it,
    -- so nothing can later inherit that record — and so the container
    -- reads as never-inspected again rather than as a permanently
    -- stale ghost with no surface to clear it.
    forgetContainerEverywhere (wsWorldManagerRef sim) bid
    -- #1206: same reasoning for the power registry, which declares the
    -- building manager the authority for a node's lifetime. Demolition
    -- has to honour that HERE, in the live transaction, because a node
    -- has no cancel surface of its own: unlike a demolished station's
    -- craft bills (which linger deliberately, visible + cancellable —
    -- the #758 tolerance), nothing the player can reach could ever
    -- clear a node whose building is gone, and load staging restores
    -- such a row verbatim, so it would persist forever.
    retirePowerNodeEverywhere (wsWorldManagerRef sim) bid

handleBuildingCommand _ sim _ bld BuildingClearAll = do
    -- Queue-ordered wipe (runs after any pending BuildingSpawns), #58.
    atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm →
        (bm { bmInstances = HM.empty, bmSelected = Nothing }, ())
    forgetAllContainers (wsWorldManagerRef sim)

-- | Insert one spawned building into the manager. Shared by the drain
--   above and by 'World.Thread.Command.BoundSpawn' (#1602): a PAGE-BOUND
--   placement is applied by the WORLD thread instead, because that is
--   the thread page selection belongs to, and its binding check would be
--   meaningless if the insertion it guards happened somewhere else
--   afterwards. One body, two callers — the two can never diverge on
--   what a spawn actually does.
applyBuildingSpawn ∷ LoggerState → WorldSimCapability
                   → ContentRegistriesViewCapability → BuildingCapability
                   → BuildingId → Text → Int → Int → Int → WorldPageId
                   → IO ()
applyBuildingSpawn logger sim reg bld bid defName gx gy gz pageId = do
    bm ← readIORef (bcBuildingManagerRef bld)
    -- Drop the spawn if its world is gone — a spawn queued before
    -- world.destroyAll would otherwise re-insert an orphan building into
    -- the cleared manager after teardown (#58).
    wmgr ← readIORef (wsWorldManagerRef sim)
    let worldGone = pageId `notElem` map fst (wmWorlds wmgr)
    case HM.lookup defName (bmDefs bm) of
        _ | worldGone → pure ()
        Nothing →
            logWarn logger CatThread $
                "BuildingSpawn: unknown def '" <> defName <> "'"
        Just def → do
            -- Game-clock so the appear-anim countdown freezes on pause.
            now ← readIORef (wsGameTimeRef sim)
            let inst = BuildingInstance
                    { biDefName   = defName
                    , biPage      = pageId
                    , biTexture   = bdSouthTexture def
                    , biAnchorX   = gx
                    , biAnchorY   = gy
                    , biGridZ     = gz
                    , biSpawnedAt = now
                    , biTileW     = bdTileW def
                    , biTileH     = bdTileH def
                    , biSpawnRemaining = -1  -- sentinel: "Lua hasn't seeded
                                            -- this from a spawn-config yet."
                                            -- ensureState in building_spawn.lua
                                            -- treats -1 as fresh, 0+ as
                                            -- a real (possibly depleted)
                                            -- count from a prior save.
                    , biBuildProgress      = 0
                    , biMaterialsDelivered = HM.empty
                    , biStorage            = []
                    }
            atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm' →
                (bm' { bmInstances = HM.insert bid inst (bmInstances bm') }, ())
            -- #1087: an INSTANT-BUILT storage building (bdBuildWork ==
            -- 0) reaches Built on currentActivity's TIME-BASED arm —
            -- immediately when it declares no appearing animation, or
            -- once that animation's duration has elapsed. Nothing calls
            -- building.addBuildProgress for it, so the transition is
            -- watched from HERE: the drain's own sweep below re-checks
            -- each marked container every tick and seeds the moment it
            -- genuinely flips. Seeding at placement outright would be
            -- wrong for an animated def, which is still Appearing then.
            -- A WORKER-BUILT one (SeedAtBuildCompletion) is untouched:
            -- it is created at zero progress and seeds from its own
            -- progress crossing.
            when (seedTriggerFor def ≡ SeedWhenBuilt) $
                markPendingSeed (containerObserver bld sim reg) bid

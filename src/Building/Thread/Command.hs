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
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import World.Generate.Types (WorldGenParams(..))
import World.State.Types (WorldManager(..), WorldState(..))
import World.Page.Types (WorldPageId(..))
import Building.Types
import Building.Destruction
    ( captureDestructionEffect, destructionExpired
    , pruneExpiredDestructions )
import Building.Knowledge (SeedTrigger(..), seedTriggerFor)
import Building.Reservation
    (clearReservations, commitFootprint, releaseReservation)
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
    pruneDestructions
    -- #1087: after the queue is empty, give every container THIS
    -- SESSION placed and is still watching a chance to have reached
    -- Built. Scoped to that session-local set, never to every
    -- already-built container, so a loaded one is never seeded as
    -- though the player had just watched it go up.
    sweepPendingSeeds (containerObserver bld sim reg)
  where
    -- Stops at the Exit-to-Menu boundary marker (#2291) rather than
    -- draining past it, for the same reason the unit drain does
    -- ('Unit.Thread.Command.processAllUnitCommands'): the tick resets
    -- the game clock right after this pass, so a command queued BEHIND
    -- the boundary must not be applied — and stamped with the outgoing
    -- session's clock — on the near side of that reset. The marker
    -- carries no work, so consuming it is the whole of handling it.
    drain = do
        mCmd ← Q.tryReadQueue (bcBuildingQueue bld)
        case mCmd of
            Just BuildingEndSession → return ()
            Just cmd → handleBuildingCommand logRef sim reg bld cmd >> drain
            Nothing  → return ()
    -- #2091: expire destruction presentations HERE, against the game
    -- clock, not in the render pass. The unit tick runs this drain
    -- every tick regardless of pause while advancing the game clock
    -- only when unpaused, so a paused effect stays frozen at its phase
    -- and a hidden, culled or never-drawn one still expires on time.
    -- Read-then-write on purpose: the render thread reads this ref
    -- every frame, so an idle tick (no effect, or none expired) must
    -- not pay for a manager write it has no reason to make.
    pruneDestructions = do
        bm ← readIORef (bcBuildingManagerRef bld)
        unless (HM.null (bmDestructions bm)) $ do
            now ← readIORef (wsGameTimeRef sim)
            when (any (destructionExpired now) (HM.elems (bmDestructions bm))) $
                atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm' →
                    ( bm' { bmDestructions =
                                pruneExpiredDestructions now (bmDestructions bm') }
                    , () )

handleBuildingCommand ∷ IORef LoggerState → WorldSimCapability
                      → ContentRegistriesViewCapability → BuildingCapability
                      → BuildingCommand → IO ()
handleBuildingCommand logRef sim reg bld
                      (BuildingSpawn bid defName gx gy gz pageId) = do
    logger ← readIORef logRef
    applyBuildingSpawn logger sim reg bld bid defName gx gy gz pageId

handleBuildingCommand logRef sim _ bld (BuildingDestroy bid) = do
    -- #2091: the destruction presentation's frame zero. Same clock as
    -- biSpawnedAt, read BEFORE the transaction so the capture below is
    -- pure and the whole removal is one manager transition.
    now ← readIORef (wsGameTimeRef sim)
    invalidClip ← atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm →
        let cleared = if bmSelected bm ≡ Just bid
                      then Nothing
                      else bmSelected bm
            -- Only a LIVE instance can leave an effect behind
            -- (requirement 4): an unknown or already-demolished id
            -- finds nothing here, so it can neither start nor restart
            -- playback. The def missing from the manager (a save
            -- naming a removed definition) has no role to resolve and
            -- is removed silently, like a def with no destruction role.
            captured = case HM.lookup bid (bmInstances bm) of
                Nothing   → Right Nothing
                Just inst → case HM.lookup (biDefName inst) (bmDefs bm) of
                    Nothing  → Right Nothing
                    Just def → captureDestructionEffect now bid inst def
            effects = case captured of
                Right (Just eff) → HM.insert bid eff (bmDestructions bm)
                _                → bmDestructions bm
        in ( bm { bmInstances    = HM.delete bid (bmInstances bm)
                , bmSelected     = cleared
                , bmDestructions = effects }
           , either Just (const Nothing) captured )
    -- An invalid destruction declaration degrades to "no visual" and
    -- is reported with its building/animation context; it never
    -- blocks, skips or crashes the removal above or the cleanup below.
    forM_ invalidClip $ \err → do
        logger ← readIORef logRef
        logWarn logger CatThread $
            "BuildingDestroy: no destruction presentation for building id "
                <> tshow (unBuildingId bid) <> ": " <> err
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
    -- Bulk removal is immediate and SILENT (#2091): every outstanding
    -- destruction effect goes with the instances, and none is spawned.
    atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm →
        -- #2326: outstanding footprint reservations go with the
        -- instances. This clear is enqueued BEHIND every pending spawn
        -- (#58), so anything still holding tiles here was admitted for a
        -- session that is being torn down and can never commit.
        ( clearReservations bm
            { bmInstances = HM.empty, bmSelected = Nothing
            , bmDestructions = HM.empty }
        , () )
    forgetAllContainers (wsWorldManagerRef sim)

-- The session boundary (#2291) is a queue POSITION, not work: 'drain'
-- takes it off the queue and stops there, so it never reaches this
-- dispatch. Matched anyway, and only to keep the dispatch total.
handleBuildingCommand _ _ _ _ BuildingEndSession = return ()

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
    let mPage     = lookup pageId (wmWorlds wmgr)
        worldGone = isNothing mPage
    case HM.lookup defName (bmDefs bm) of
        -- #2326: every path that drops the spawn retires its footprint
        -- reservation on the way out, so a claim can outlive the request
        -- that took it only for as long as that request is in flight.
        _ | worldGone → releaseClaim
        Nothing → do
            releaseClaim
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
            -- #2326: the footprint is decided HERE, in the same
            -- transition that inserts. 'commitFootprint' verifies this
            -- request still OWNS the reservation its admission took and
            -- that the tiles are still free of committed instances on
            -- this page; a request that holds neither inserts nothing.
            -- Nothing between the two reads can interleave, which is
            -- exactly what the admitting thread could not promise.
            worldSize ← maybe (pure 0)
                (\ws → maybe 0 wgpWorldSize <$> readIORef (wsGenParamsRef ws))
                mPage
            committed ← atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm' →
                let (retired, accepted) =
                        commitFootprint worldSize pageId bid gx gy
                                        (bdTileW def) (bdTileH def) bm'
                in if accepted
                   then ( retired { bmInstances =
                                        HM.insert bid inst (bmInstances retired) }
                        , True )
                   -- The claim, if this request held one, is retired
                   -- here too: a refused commit must not leave tiles
                   -- claimed for a building that will never appear.
                   else (retired, False)
            -- A rejected commit performs NO winner-only follow-up: no
            -- instance, and none of the container seeding below.
            unless committed $
                logDebug logger CatThread $
                    "BuildingSpawn dropped: footprint no longer claimed by "
                    <> "building id " <> tshow (unBuildingId bid) <> " ('"
                    <> defName <> "' at " <> tshow gx <> "," <> tshow gy
                    <> " on " <> unWorldPageId pageId <> ")"
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
            when (committed ∧ seedTriggerFor def ≡ SeedWhenBuilt) $
                markPendingSeed (containerObserver bld sim reg) bid
  where
    -- #2326: retire this request's footprint claim. Spelled out here,
    -- through the capability accessor, rather than behind a helper
    -- taking the handle: that is what keeps the write attributable to
    -- this module in the SS5 writing-module map.
    releaseClaim = atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm' →
        (releaseReservation bid bm', ())

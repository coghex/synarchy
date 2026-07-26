{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The building command drain. Narrowed to the @units-buildings-combat@
--   building half (#896, epic #537): it takes 'BuildingCapability' plus
--   the two strictly narrower values it needs (the shared logger and
--   the world/sim view) rather than an 'Engine.Core.State.EngineEnv',
--   so its only caller — "Unit.Thread", which runs this drain on the
--   unit thread because there is no building thread
--   ('docs/engineenv_capability_inventory.md' §2.2) — no longer hands
--   its whole environment across the boundary.
module Building.Thread.Command
    ( processAllBuildingCommands
    ) where

import UPrelude
import Engine.Core.Capability.Building (BuildingCapability(..))
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import World.State.Types (WorldManager(..))
import Building.Types
import Building.Command.Types (BuildingCommand(..))

-- | Drain the building command queue in one pass. Called from the
--   unit thread's tick so we don't need a dedicated building thread —
--   buildings don't have per-tick logic, just point-in-time spawns
--   and destroys.
processAllBuildingCommands ∷ IORef LoggerState → WorldSimCapability
                           → BuildingCapability → IO ()
processAllBuildingCommands logRef sim bld = do
    mCmd ← Q.tryReadQueue (bcBuildingQueue bld)
    case mCmd of
        Just cmd → do
            handleBuildingCommand logRef sim bld cmd
            processAllBuildingCommands logRef sim bld
        Nothing → return ()

handleBuildingCommand ∷ IORef LoggerState → WorldSimCapability
                      → BuildingCapability → BuildingCommand → IO ()
handleBuildingCommand logRef sim bld
                      (BuildingSpawn bid defName gx gy gz pageId) = do
    bm ← readIORef (bcBuildingManagerRef bld)
    -- Drop the spawn if its world is gone — a spawn queued before
    -- world.destroyAll would otherwise re-insert an orphan building into
    -- the cleared manager after teardown (#58).
    wmgr ← readIORef (wsWorldManagerRef sim)
    let worldGone = pageId `notElem` map fst (wmWorlds wmgr)
    case HM.lookup defName (bmDefs bm) of
        _ | worldGone → pure ()
        Nothing → do
            logger ← readIORef logRef
            logWarn logger CatThread $
                "BuildingSpawn: unknown def '" <> defName <> "'"
        Just def → do
            -- Game-clock so the appear-anim countdown freezes on pause.
            now ← readIORef (wsGameTimeRef sim)
            let inst = BuildingInstance
                    { biDefName   = defName
                    , biPage      = pageId
                    , biTexture   = bdTexture def
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

handleBuildingCommand _ _ bld (BuildingDestroy bid) =
    atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm →
        let cleared = if bmSelected bm ≡ Just bid
                      then Nothing
                      else bmSelected bm
        in (bm { bmInstances = HM.delete bid (bmInstances bm)
               , bmSelected  = cleared }, ())

handleBuildingCommand _ _ bld BuildingClearAll =
    -- Queue-ordered wipe (runs after any pending BuildingSpawns), #58.
    atomicModifyIORef' (bcBuildingManagerRef bld) $ \bm →
        (bm { bmInstances = HM.empty, bmSelected = Nothing }, ())

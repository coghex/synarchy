{-# LANGUAGE Strict, UnicodeSyntax #-}
module Building.Thread.Command
    ( processAllBuildingCommands
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
import World.State.Types (WorldManager(..))
import Building.Types
import Building.Command.Types (BuildingCommand(..))

-- | Drain the building command queue in one pass. Called from the
--   unit thread's tick so we don't need a dedicated building thread —
--   buildings don't have per-tick logic, just point-in-time spawns
--   and destroys.
processAllBuildingCommands ∷ EngineEnv → IO ()
processAllBuildingCommands env = do
    mCmd ← Q.tryReadQueue (buildingQueue env)
    case mCmd of
        Just cmd → do
            handleBuildingCommand env cmd
            processAllBuildingCommands env
        Nothing → return ()

handleBuildingCommand ∷ EngineEnv → BuildingCommand → IO ()
handleBuildingCommand env (BuildingSpawn bid defName gx gy gz pageId) = do
    bm ← readIORef (buildingManagerRef env)
    -- Drop the spawn if its world is gone — a spawn queued before
    -- world.destroyAll would otherwise re-insert an orphan building into
    -- the cleared manager after teardown (#58).
    wmgr ← readIORef (worldManagerRef env)
    let worldGone = pageId `notElem` map fst (wmWorlds wmgr)
    case HM.lookup defName (bmDefs bm) of
        _ | worldGone → pure ()
        Nothing → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatThread $
                "BuildingSpawn: unknown def '" <> defName <> "'"
        Just def → do
            -- Game-clock so the appear-anim countdown freezes on pause.
            now ← readIORef (gameTimeRef env)
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
            atomicModifyIORef' (buildingManagerRef env) $ \bm' →
                (bm' { bmInstances = HM.insert bid inst (bmInstances bm') }, ())

handleBuildingCommand env (BuildingDestroy bid) =
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        let cleared = if bmSelected bm ≡ Just bid
                      then Nothing
                      else bmSelected bm
        in (bm { bmInstances = HM.delete bid (bmInstances bm)
               , bmSelected  = cleared }, ())

handleBuildingCommand env BuildingClearAll =
    -- Queue-ordered wipe (runs after any pending BuildingSpawns), #58.
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmInstances = HM.empty, bmSelected = Nothing }, ())

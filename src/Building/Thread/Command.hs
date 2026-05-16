{-# LANGUAGE Strict, UnicodeSyntax #-}
module Building.Thread.Command
    ( processAllBuildingCommands
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logWarn, LogCategory(..))
import qualified Engine.Core.Queue as Q
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
handleBuildingCommand env (BuildingSpawn bid defName gx gy gz) = do
    bm ← readIORef (buildingManagerRef env)
    case HM.lookup defName (bmDefs bm) of
        Nothing → do
            logger ← readIORef (loggerRef env)
            logWarn logger CatThread $
                "BuildingSpawn: unknown def '" <> defName <> "'"
        Just def → do
            -- Game-clock so the appear-anim countdown freezes on pause.
            now ← readIORef (gameTimeRef env)
            let inst = BuildingInstance
                    { biDefName   = defName
                    , biTexture   = bdTexture def
                    , biAnchorX   = gx
                    , biAnchorY   = gy
                    , biGridZ     = gz
                    , biSpawnedAt = now
                    , biTileW     = bdTileW def
                    , biTileH     = bdTileH def
                    }
            atomicModifyIORef' (buildingManagerRef env) $ \bm' →
                (bm' { bmInstances = HM.insert bid inst (bmInstances bm') }, ())

handleBuildingCommand env (BuildingDestroy bid) =
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmInstances = HM.delete bid (bmInstances bm) }, ())

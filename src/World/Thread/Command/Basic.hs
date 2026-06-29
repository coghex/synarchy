module World.Thread.Command.Basic
    ( handleWorldTickCommand
    , handleWorldSetCameraCommand
    , handleWorldDestroyCommand
    , handleWorldDestroyAllCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Unit.Command.Types (UnitCommand(..))
import Building.Command.Types (BuildingCommand(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Constants (seaLevel)
import World.Generate (generateChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology (buildTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, isOceanChunk)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCache)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)

handleWorldTickCommand ∷ EngineEnv → LoggerState → Double → IO ()
handleWorldTickCommand _ _ _ = return ()

handleWorldSetCameraCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Float → Float → IO ()
handleWorldSetCameraCommand env logger pageId x y = do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCameraRef worldState) $ \_ →
                        (WorldCamera x y, ())
                Nothing → 
                    logDebug logger CatWorld $ 
                        "World not found for camera update: " <> unWorldPageId pageId

handleWorldDestroyCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldDestroyCommand env logger pageId = do
    logInfo logger CatWorld $ "Destroying world: " <> unWorldPageId pageId

    -- Tear down this world's simulation state too — destroy used to drop
    -- the page from wmWorlds/wmVisible while leaving its sim chunks behind
    -- forever (#61). SimDropWorld discards them (unlike hide, which keeps
    -- them for a later re-show); only this world's sim is touched.
    Q.writeQueue (simQueue env) (SimDropWorld pageId)

    -- Remove from visible list
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmVisible = filter (/= pageId) (wmVisible mgr)
             , wmWorlds  = filter ((/= pageId) . fst) (wmWorlds mgr)
             }, ())

    -- Clear world quads so renderer stops drawing the old world
    writeIORef (worldQuadsRef env) V.empty

    logInfo logger CatWorld $ "World destroyed: " <> unWorldPageId pageId

-- | Tear down EVERY world (Exit to Menu). Destroying only the "current"
--   world left hidden ones (e.g. a leftover test arena) in wmWorlds, and
--   resolveActiveWorld's head-fallback then kept resolving one as the
--   implicit active world behind the menu (#58). Clearing wmWorlds makes
--   the resolver return Nothing (menu state). Also sim-deactivates each
--   page and resets the global entity managers so no units/buildings from
--   the old session linger as orphans into the next game.
handleWorldDestroyAllCommand ∷ EngineEnv → LoggerState → IO ()
handleWorldDestroyAllCommand env logger = do
    logInfo logger CatWorld "Destroying all worlds (Exit to Menu)"
    mgr ← readIORef (worldManagerRef env)
    -- Drop (not just deactivate) each world's sim state — every world is
    -- being destroyed, so its chunks are gone for good (#58/#61).
    forM_ (map fst (wmWorlds mgr)) $ \pid →
        Q.writeQueue (simQueue env) (SimDropWorld pid)
    atomicModifyIORef' (worldManagerRef env) $ \m →
        (m { wmWorlds = [], wmVisible = [] }, ())
    -- Forget the previous game's save-load page set: the next game's first
    -- load must not treat a stale synthetic id from this game as reusable,
    -- which could otherwise let it clobber an unrelated page (#214).
    writeIORef (lastLoadPagesRef env) HS.empty
    writeIORef (worldQuadsRef env) V.empty
    -- Reset the entity managers via the UNIT/BUILDING queues, not directly:
    -- those threads keep draining their queues through the teardown, so
    -- clearing the managers here would race any in-flight spawns and let
    -- them re-insert orphans afterwards. Enqueuing the clears makes them
    -- run in order, AFTER every pending spawn (#58). The wmWorlds clear
    -- above also makes the spawn handlers drop late spawns outright.
    Q.writeQueue (unitQueue env) UnitClearAll
    Q.writeQueue (buildingQueue env) BuildingClearAll
    logInfo logger CatWorld "All worlds destroyed"

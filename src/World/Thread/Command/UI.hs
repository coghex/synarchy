module World.Thread.Command.UI
    ( handleWorldShowCommand
    , handleWorldHideCommand
    , handleWorldSetMapModeCommand
    , handleWorldSetToolModeCommand
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
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
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

handleWorldShowCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldShowCommand env logger pageId = do
    logDebug logger CatWorld $ "Showing world: " <> unWorldPageId pageId

    -- Only worlds that actually exist may enter wmVisible. Inserting a
    -- nonexistent pageId would poison getActiveWorldId() (which reads the
    -- head of wmVisible) and silently retarget every current-world API.
    -- atomicModifyIORef' returns whether the world was found so the
    -- existence check and the visible-list mutation share one consistent
    -- snapshot of the manager.
    found ← atomicModifyIORef' (worldManagerRef env) $ \mgr →
        case lookup pageId (wmWorlds mgr) of
            Nothing → (mgr, False)
            Just _
                | pageId `elem` wmVisible mgr → (mgr, True)
                | otherwise →
                    (mgr { wmVisible = pageId : wmVisible mgr }, True)

    if not found
    then logWarn logger CatWorld $
        "Ignoring world.show for nonexistent world: " <> unWorldPageId pageId
    else do
        mgr ← readIORef (worldManagerRef env)
        logDebug logger CatWorld $
            "Visible worlds after show: " <> T.pack (show $ length $ wmVisible mgr)

        -- Activate world in sim thread. The sim no longer holds the tile
        -- ref — it emits WorldApplyFluids back to the world thread (the sole
        -- writer of wsTilesRef) — so this is just an "is active" signal.
        Q.writeQueue (simQueue env) SimActivateWorld

handleWorldHideCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldHideCommand env logger pageId = do
    logDebug logger CatWorld $ "Hiding world: " <> unWorldPageId pageId

    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmVisible = filter (/= pageId) (wmVisible mgr) }, ())

    -- Deactivate sim thread
    Q.writeQueue (simQueue env) SimDeactivateWorld

handleWorldSetMapModeCommand ∷ EngineEnv → LoggerState → WorldPageId
    → ZoomMapMode → IO ()
handleWorldSetMapModeCommand env logger pageId mode = do
    logDebug logger CatWorld $
        "Setting map mode for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show mode)
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            writeIORef (wsMapModeRef worldState) mode
            logInfo logger CatWorld $
                "Map mode updated for world: " <> unWorldPageId pageId
                <> ", new mode: " <> T.pack (show mode)
        Nothing →
            logDebug logger CatWorld $
                "World not found for map mode update: " <> unWorldPageId pageId

handleWorldSetToolModeCommand ∷ EngineEnv → LoggerState → WorldPageId → ToolMode → IO ()
handleWorldSetToolModeCommand env logger pagedId mode = do
        mgr ← readIORef (worldManagerRef env)
        case lookup pagedId (wmWorlds mgr) of
            Just worldState → do
                writeIORef (wsToolModeRef worldState) mode
                logInfo logger CatWorld $
                    "Tool mode updated for world: " <> unWorldPageId pagedId
                    <> ", new mode: " <> T.pack (show mode)
            Nothing →
                logDebug logger CatWorld $
                    "World not found for tool mode update: " <> unWorldPageId pagedId

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
import World.Save.Serialize (saveWorld)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)

handleWorldShowCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldShowCommand env logger pageId = do
    logDebug logger CatWorld $ "Showing world: " <> unWorldPageId pageId
    
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        if pageId `elem` wmVisible mgr
        then (mgr, ())
        else (mgr { wmVisible = pageId : wmVisible mgr }, ())
    
    mgr ← readIORef (worldManagerRef env)
    logDebug logger CatWorld $ 
        "Visible worlds after show: " <> T.pack (show $ length $ wmVisible mgr)
        
handleWorldHideCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldHideCommand env logger pageId = do
    logDebug logger CatWorld $ "Hiding world: " <> unWorldPageId pageId
    
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmVisible = filter (/= pageId) (wmVisible mgr) }, ())

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

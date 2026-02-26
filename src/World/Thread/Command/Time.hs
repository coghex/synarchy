module World.Thread.Command.Time
    ( handleWorldSetTimeCommand
    , handleWorldSetDateCommand
    , handleWorldSetTimeScaleCommand
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

handleWorldSetTimeCommand ∷ EngineEnv → LoggerState → WorldPageId → Int → Int → IO ()
handleWorldSetTimeCommand env logger pageId hour minute = do
    logDebug logger CatWorld $
        "Setting time for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show hour) <> ":" <> T.pack (show minute)
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            let clampedH = max 0 (min 23 hour)
                clampedM = max 0 (min 59 minute)
            atomicModifyIORef' (wsTimeRef worldState) $ \_ →
                (WorldTime clampedH clampedM, ())
        Nothing →
            logDebug logger CatWorld $
                "World not found for time update: " <> unWorldPageId pageId


handleWorldSetDateCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → IO ()
handleWorldSetDateCommand env logger pageId year month day = do
    logDebug logger CatWorld $
        "Setting date for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show year) <> "-"
        <> T.pack (show month) <> "-" <> T.pack (show day)
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsDateRef worldState) $ \_ →
                (WorldDate year month day, ())
        Nothing →
            logDebug logger CatWorld $
                "World not found for date update: " <> unWorldPageId pageId

handleWorldSetTimeScaleCommand ∷ EngineEnv → LoggerState → WorldPageId → Float → IO ()
handleWorldSetTimeScaleCommand env logger pageId scale = do
    logDebug logger CatWorld $
        "Setting time scale for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show scale) <> " game-min/real-sec"
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            writeIORef (wsTimeScaleRef worldState) scale
        Nothing →
            logDebug logger CatWorld $
                "World not found for time scale update: " <> unWorldPageId pageId

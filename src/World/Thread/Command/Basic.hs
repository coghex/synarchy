module World.Thread.Command.Basic
    ( handleWorldTickCommand
    , handleWorldSetCameraCommand
    , handleWorldDestroyCommand
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
    
    -- Remove from visible list
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmVisible = filter (/= pageId) (wmVisible mgr)
             , wmWorlds  = filter ((/= pageId) . fst) (wmWorlds mgr)
             }, ())
    
    -- Clear world quads so renderer stops drawing the old world
    writeIORef (worldQuadsRef env) V.empty
    
    logInfo logger CatWorld $ "World destroyed: " <> unWorldPageId pageId

{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command
    ( handleWorldCommand
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
import World.Thread.Command.Basic (handleWorldTickCommand
                                  , handleWorldSetCameraCommand
                                  , handleWorldDestroyCommand)
import World.Thread.Command.Init (handleWorldInitCommand)
import World.Thread.Command.Cursor (handleWorldSetZoomCursorHoverCommand
                                   , handleWorldSetZoomCursorSelectCommand
                                   , handleWorldSetZoomCursorDeselectCommand
                                   , handleWorldSetZoomCursorSelectTextureCommand
                                   , handleWorldSetZoomCursorHoverTextureCommand
                                   , handleWorldSetWorldCursorHoverCommand
                                   , handleWorldSetWorldCursorSelectCommand
                                   , handleWorldSetWorldCursorDeselectCommand
                                   , handleWorldSetWorldCursorSelectTextureCommand
                                   , handleWorldSetWorldCursorHoverTextureCommand
                                   , handleWorldSetWorldCursorSelectBgTextureCommand
                                   , handleWorldSetWorldCursorHoverBgTextureCommand)
import World.Thread.Command.Texture (handleWorldSetTextureCommand)
import World.Thread.Command.Time (handleWorldSetTimeCommand
                                 , handleWorldSetDateCommand
                                 , handleWorldSetTimeScaleCommand)
import World.Thread.Command.Save (handleWorldSaveCommand
                                 , handleWorldLoadSaveCommand)
import World.Thread.Command.UI (handleWorldShowCommand, handleWorldHideCommand
                               , handleWorldSetMapModeCommand
                               , handleWorldSetToolModeCommand)

-----------------------------------------------------------
-- Command Handler
-----------------------------------------------------------

handleWorldCommand ∷ EngineEnv → LoggerState → WorldCommand → IO ()
handleWorldCommand env logger (WorldInit pageId seed worldSize placeCount)
  = handleWorldInitCommand env logger pageId seed worldSize placeCount
handleWorldCommand env logger (WorldSetTexture pageId texType texHandle)
  = handleWorldSetTextureCommand env logger pageId texType texHandle
handleWorldCommand env logger (WorldShow pageId)
  = handleWorldShowCommand env logger pageId
handleWorldCommand env logger (WorldHide pageId)
  = handleWorldHideCommand env logger pageId
handleWorldCommand env logger (WorldSetMapMode pageId mapMode)
  = handleWorldSetMapModeCommand env logger pageId mapMode
handleWorldCommand env logger (WorldSetToolMode pageId toolMode)
  = handleWorldSetToolModeCommand env logger pageId toolMode
handleWorldCommand env logger (WorldTick dt)
  = handleWorldTickCommand env logger dt
handleWorldCommand env logger (WorldSetCamera pageId x y)
  = handleWorldSetCameraCommand env logger pageId x y
handleWorldCommand env logger (WorldSetTime pageId hour minute)
  = handleWorldSetTimeCommand env logger pageId hour minute
handleWorldCommand env logger (WorldSetDate pageId year month day)
  = handleWorldSetDateCommand env logger pageId year month day
handleWorldCommand env logger (WorldSetTimeScale pageId scale)
  = handleWorldSetTimeScaleCommand env logger pageId scale
handleWorldCommand env logger (WorldSetZoomCursorHover pageId x y)
  = handleWorldSetZoomCursorHoverCommand env logger pageId x y
handleWorldCommand env logger (WorldSetZoomCursorSelect pageId)
  = handleWorldSetZoomCursorSelectCommand env logger pageId
handleWorldCommand env logger (WorldSetZoomCursorDeselect pageId)
  = handleWorldSetZoomCursorDeselectCommand env logger pageId
handleWorldCommand env logger (WorldSetZoomCursorSelectTexture pageId texHandle)
  = handleWorldSetZoomCursorSelectTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetZoomCursorHoverTexture pageId texHandle)
  = handleWorldSetZoomCursorHoverTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHover pageId x y)
  = handleWorldSetWorldCursorHoverCommand env logger pageId x y
handleWorldCommand env logger (WorldSetWorldCursorSelect pageId)
  = handleWorldSetWorldCursorSelectCommand env logger pageId
handleWorldCommand env logger (WorldSetWorldCursorDeselect pageId)
  = handleWorldSetWorldCursorDeselectCommand env logger pageId
handleWorldCommand env logger (WorldSetWorldCursorSelectTexture pageId texHandle)
  = handleWorldSetWorldCursorSelectTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHoverTexture pageId texHandle)
  = handleWorldSetWorldCursorHoverTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorSelectBgTexture pageId texHandle)
  = handleWorldSetWorldCursorSelectBgTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHoverBgTexture pageId texHandle)
  = handleWorldSetWorldCursorHoverBgTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSave pageId saveName)
  = handleWorldSaveCommand env logger pageId saveName
handleWorldCommand env logger (WorldLoadSave pageId saveName)
  = handleWorldLoadSaveCommand env logger pageId saveName
handleWorldCommand env logger (WorldDestroy pageId)
  = handleWorldDestroyCommand env logger pageId


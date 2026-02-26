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
import World.Thread.Command.Basic (handleWorldTickCommand)
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
handleWorldCommand env logger cmd = do
    case cmd of
        WorldSetCamera pageId x y → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCameraRef worldState) $ \_ →
                        (WorldCamera x y, ())
                Nothing → 
                    logDebug logger CatWorld $ 
                        "World not found for camera update: " <> unWorldPageId pageId
        -- ── Save: snapshot the live WorldState and write to disk ──
        WorldSave pageId saveName → do
            logInfo logger CatWorld $ "Saving world: " <> unWorldPageId pageId
                <> " as \"" <> saveName <> "\""

            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing →
                    logWarn logger CatWorld $
                        "World not found for save: " <> unWorldPageId pageId
                Just worldState → do
                    -- Read every IORef we care about (we're on the
                    -- world thread, so no races with worldLoop writes)
                    mParams   ← readIORef (wsGenParamsRef worldState)
                    WorldCamera cx cy ← readIORef (wsCameraRef worldState)
                    WorldTime h m     ← readIORef (wsTimeRef worldState)
                    WorldDate y mo d  ← readIORef (wsDateRef worldState)
                    tScale    ← readIORef (wsTimeScaleRef worldState)
                    mapMode   ← readIORef (wsMapModeRef worldState)
                    toolMode  ← readIORef (wsToolModeRef worldState)
                    climate   ← readIORef (wsClimateRef worldState)
                    riverFlow ← readIORef (wsRiverFlowRef worldState)

                    case mParams of
                        Nothing →
                            logWarn logger CatWorld
                                "Cannot save: world has no gen params"
                        Just params → do
                            let meta = SaveMetadata
                                    { smName       = saveName
                                    , smSeed       = wgpSeed params
                                    , smWorldSize  = wgpWorldSize params
                                    , smPlateCount = wgpPlateCount params
                                    , smTimestamp  = ""  -- filled below
                                    }
                                sd = SaveData
                                    { sdMetadata   = meta
                                    , sdGenParams  = params
                                    , sdCameraX    = cx
                                    , sdCameraY    = cy
                                    , sdTimeHour   = h
                                    , sdTimeMinute = m
                                    , sdDateYear   = y
                                    , sdDateMonth  = mo
                                    , sdDateDay    = d
                                    , sdTimeScale  = tScale
                                    , sdMapMode    = mapMode
                                    , sdToolMode   = toolMode
                                    , sdClimate    = climate
                                    , sdRiverFlow  = riverFlow
                                    }
                            result ← saveWorld saveName sd
                            case result of
                                Right () → do
                                    logInfo logger CatWorld $
                                        "World saved successfully: " <> saveName
                                    sendGenLog env $
                                        "Game saved: " <> saveName
                                Left err → do
                                    logError logger CatWorld $
                                        "Failed to save world: " <> err
                                    sendGenLog env $
                                        "Save failed: " <> err

        -- ── Load: reconstruct WorldState from SaveData ──
        --   This is essentially WorldInit but skipping the expensive
        --   buildTimeline / computeOceanMap / initEarlyClimate steps
        --   because those are already baked into sdGenParams.
        WorldLoadSave pageId saveData → do
            logInfo logger CatWorld $ "Loading save into world: "
                <> unWorldPageId pageId

            let params    = sdGenParams saveData
                seed      = wgpSeed params
                worldSize = wgpWorldSize params

            worldState ← emptyWorldState
            let phaseRef = wsLoadPhaseRef worldState
                totalSteps = 4

            -- 1. Restore gen params (the big one — plates, timeline,
            --    ocean map, climate are all inside here)
            writeIORef phaseRef (LoadPhase1 1 totalSteps)
            sendGenLog env "Loading saved world state..."
            writeIORef (wsGenParamsRef worldState) (Just params)

            -- 2. Restore mutable game state from the save
            writeIORef (wsCameraRef worldState)
                (WorldCamera (sdCameraX saveData) (sdCameraY saveData))
            writeIORef (wsTimeRef worldState)
                (WorldTime (sdTimeHour saveData) (sdTimeMinute saveData))
            writeIORef (wsDateRef worldState)
                (WorldDate (sdDateYear saveData)
                           (sdDateMonth saveData)
                           (sdDateDay saveData))
            writeIORef (wsTimeScaleRef worldState) (sdTimeScale saveData)
            writeIORef (wsMapModeRef worldState) (sdMapMode saveData)
            writeIORef (wsToolModeRef worldState) (sdToolMode saveData)
            writeIORef (wsClimateRef worldState)  (sdClimate saveData)
            writeIORef (wsRiverFlowRef worldState) (sdRiverFlow saveData)

            -- 3. Rebuild derived caches (cheap compared to worldgen)
            writeIORef phaseRef (LoadPhase1 2 totalSteps)
            sendGenLog env "Building zoom cache..."
            let zoomCache = buildZoomCache params
            writeIORef (wsZoomCacheRef worldState) zoomCache

            sendGenLog env "Rendering world preview..."
            let preview = buildPreviewImage params zoomCache
            writeIORef (worldPreviewRef env) $
                Just (piWidth preview, piHeight preview, piData preview)

            -- 4. Generate center chunk synchronously for immediate display
            writeIORef phaseRef (LoadPhase1 3 totalSteps)
            sendGenLog env "Generating initial chunks..."
            let centerCoord = ChunkCoord 0 0
                (ct, cs, cterrain, cf) = generateChunk params centerCoord
                centerChunk = LoadedChunk
                    { lcCoord             = centerCoord
                    , lcTiles             = ct
                    , lcSurfaceMap        = cs
                    , lcTerrainSurfaceMap = cterrain
                    , lcFluidMap          = cf
                    , lcModified          = False
                    }
            atomicModifyIORef' (wsTilesRef worldState) $ \_ →
                (WorldTileData { wtdChunks    = HM.singleton centerCoord centerChunk
                               , wtdMaxChunks = 200 }, ())

            -- 5. Queue remaining initial chunks for progressive loading
            writeIORef phaseRef (LoadPhase1 4 totalSteps)
            let remainingCoords =
                    [ ChunkCoord cx cy
                    | cx ← [-chunkLoadRadius .. chunkLoadRadius]
                    , cy ← [-chunkLoadRadius .. chunkLoadRadius]
                    , not (cx ≡ 0 ∧ cy ≡ 0)
                    ]
            writeIORef (wsInitQueueRef worldState) remainingCoords

            -- 6. Register world in the manager
            atomicModifyIORef' (worldManagerRef env) $ \mgr →
                (mgr { wmWorlds = (pageId, worldState) : wmWorlds mgr }, ())

            -- 7. Set camera z-slice from saved camera position
            let (surfaceElev, _mat) =
                    elevationAtGlobal seed (wgpPlates params) worldSize 0 0
                startZSlice = surfaceElev + surfaceHeadroom
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZSlice = startZSlice, camZTracking = True }, ())

            let totalInitialChunks =
                    (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)

            writeIORef phaseRef (LoadPhase2 (length remainingCoords) totalInitialChunks)
            sendGenLog env $ "Save loaded: "
                <> T.pack (show totalInitialChunks) <> " chunks queued"

            logInfo logger CatWorld $ "Save loaded: "
                <> T.pack (show totalInitialChunks) <> " chunks, "
                <> "surface at z=" <> T.pack (show surfaceElev)
                <> ": " <> unWorldPageId pageId
        WorldDestroy pageId → do
            logInfo logger CatWorld $ "Destroying world: " <> unWorldPageId pageId
            
            -- Remove from visible list
            atomicModifyIORef' (worldManagerRef env) $ \mgr →
                (mgr { wmVisible = filter (/= pageId) (wmVisible mgr)
                     , wmWorlds  = filter ((/= pageId) . fst) (wmWorlds mgr)
                     }, ())
            
            -- Clear world quads so renderer stops drawing the old world
            writeIORef (worldQuadsRef env) V.empty
            
            logInfo logger CatWorld $ "World destroyed: " <> unWorldPageId pageId

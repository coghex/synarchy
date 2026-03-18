{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command.Save
    ( handleWorldSaveCommand
    , handleWorldLoadSaveCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
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
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Save.Serialize (saveWorld)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)


-- | Save: snapshot the live WorldState and write to disk ──logInfo logger CatWorld $ "Saving world: " <> unWorldPageId pageId
handleWorldSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → Text → IO ()
handleWorldSaveCommand env logger pageId saveName = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for save: " <> unWorldPageId pageId
        Just worldState → do
            -- Read every IORef we care about (we're on the
            -- world thread, so no races with worldLoop writes)
            mParams   ← readIORef (wsGenParamsRef worldState)
            cam       ← readIORef (cameraRef env)
            let (cx, cy) = camPosition cam
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
                            , sdCameraZoom = camZoom cam
                            , sdCameraFacing = camFacing cam
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

-- | Load: reconstruct WorldState from SaveData ──
--   This is essentially WorldInit but skipping the expensive
--   buildTimeline / computeOceanMap / initEarlyClimate steps
--   because those are already baked into sdGenParams.
handleWorldLoadSaveCommand ∷ EngineEnv → LoggerState → WorldPageId → SaveData → IO ()
handleWorldLoadSaveCommand env logger pageId saveData = do
    logInfo logger CatWorld $ "Loading save into world: "
        <> unWorldPageId pageId

    let params    = sdGenParams saveData
        seed      = wgpSeed params
        worldSize = wgpWorldSize params

    worldState ← emptyWorldState
    let phaseRef = wsLoadPhaseRef worldState
        totalSteps = 4

    -- Register early so the render thread can find this world
    -- when uploading the zoom atlas (same as init path)
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmWorlds = (pageId, worldState) : wmWorlds mgr }, ())

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

    -- 3. Rebuild zoom cache with per-chunk textures (matches init path)
    writeIORef phaseRef (LoadPhase1 2 totalSteps)
    sendGenLog env "Building zoom color palette..."
    registry ← readIORef (materialRegistryRef env)
    let !_ = registry `seq` ()
    palette ← buildColorPalette logger "data/materials" "data/vegetation"
    _ ← evaluate (force palette)

    sendGenLog env "Building zoom cache with per-chunk textures..."
    let (zoomCache, chunkPixels) = buildZoomCacheWithPixels params registry palette
    _ ← evaluate (force zoomCache)
    _ ← evaluate (force chunkPixels)
    writeIORef (wsZoomCacheRef worldState) zoomCache

    sendGenLog env "Assembling zoom texture atlas..."
    let atlas = buildZoomAtlas (V.length zoomCache) chunkPixels
    _ ← evaluate (force atlas)
    writeIORef (zoomAtlasDataRef env) $
        Just (zadWidth atlas, zadHeight atlas, zadPixelData atlas)
    writeIORef (wsZoomAtlasRef worldState) Nothing

    sendGenLog env "Rendering world preview..."
    let preview = buildPreviewFromPixels params zoomCache chunkPixels
    _ ← evaluate (force preview)
    writeIORef (worldPreviewRef env) $
        Just (piWidth preview, piHeight preview, piData preview)

    -- 4. Generate center chunk synchronously for immediate display
    writeIORef phaseRef (LoadPhase1 3 totalSteps)
    sendGenLog env "Generating initial chunks..."
    catalog ← readIORef (floraCatalogRef env)
    let camCX = floor (sdCameraX saveData) `div` chunkSize
        camCY = floor (sdCameraY saveData) `div` chunkSize
        centerCoord = ChunkCoord camCX camCY
        (ct, cs, cterrain, cf, cice, cflora) = generateChunk registry catalog params centerCoord
        centerChunk = LoadedChunk
            { lcCoord             = centerCoord
            , lcTiles             = ct
            , lcSurfaceMap        = cs
            , lcTerrainSurfaceMap = cterrain
            , lcFluidMap          = cf
            , lcIceMap            = cice
            , lcFlora             = cflora
            , lcSideDeco          = VU.replicate (chunkSize * chunkSize) 0
            , lcModified          = False
            }
    atomicModifyIORef' (wsTilesRef worldState) $ \_ →
        (WorldTileData { wtdChunks    = HM.singleton centerCoord centerChunk
                       , wtdMaxChunks = 200 }, ())

    -- 5. Queue remaining initial chunks for progressive loading
    writeIORef phaseRef (LoadPhase1 4 totalSteps)
    let remainingCoords =
            [ ChunkCoord cx cy
            | cx ← [camCX - chunkLoadRadius .. camCX + chunkLoadRadius]
            , cy ← [camCY - chunkLoadRadius .. camCY + chunkLoadRadius]
            , not (cx ≡ camCX ∧ cy ≡ camCY)
            ]
    writeIORef (wsInitQueueRef worldState) remainingCoords

    -- 6. Set camera z-slice from saved camera position
    let camGX = round (sdCameraX saveData) ∷ Int
        camGY = round (sdCameraY saveData) ∷ Int
        (surfaceElev, _mat) =
            elevationAtGlobal seed (wgpPlates params) worldSize camGX camGY
        startZSlice = surfaceElev + surfaceHeadroom
    atomicModifyIORef' (cameraRef env) $ \cam →
        (cam { camPosition = (sdCameraX saveData, sdCameraY saveData)
             , camZoom     = sdCameraZoom saveData
             , camFacing   = sdCameraFacing saveData
             , camZSlice   = startZSlice
             , camZTracking = True }, ())

    let totalInitialChunks =
            (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)

    writeIORef phaseRef (LoadPhase2 (length remainingCoords) totalInitialChunks)
    sendGenLog env $ "Save loaded: "
        <> T.pack (show totalInitialChunks) <> " chunks queued"

    logInfo logger CatWorld $ "Save loaded: "
        <> T.pack (show totalInitialChunks) <> " chunks, "
        <> "surface at z=" <> T.pack (show surfaceElev)
        <> ": " <> unWorldPageId pageId

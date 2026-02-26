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
import World.Generate (generateChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology (buildTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, isOceanChunk, seaLevel)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCache)
import World.Save.Serialize (saveWorld)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)

-----------------------------------------------------------
-- Command Handler
-----------------------------------------------------------

handleWorldCommand ∷ EngineEnv → LoggerState → WorldCommand → IO ()
handleWorldCommand env logger cmd = do
    logDebug logger CatWorld $ "Processing world command: " <> T.pack (show cmd)
    
    case cmd of
        WorldInit pageId seed worldSize placeCount → do
            logDebug logger CatWorld $ "Initializing world: " <> unWorldPageId pageId
                <> " (seed=" <> T.pack (show seed)
                <> ", size=" <> T.pack (show worldSize)
                <> ", places=" <> T.pack (show placeCount) <> ")"
            
            sendGenLog env "Initializing world state..."
            
            worldState ← emptyWorldState
            let phaseRef = wsLoadPhaseRef worldState
                totalSteps = 8

            -- register early so lua can read the loading phase
            atomicModifyIORef' (worldManagerRef env) $ \mgr →
                (mgr { wmWorlds = (pageId, worldState) : wmWorlds mgr }, ())
            
            -- Step 1: Timeline
            writeIORef phaseRef (LoadPhase1 1 totalSteps)
            sendGenLog env "Building geological timeline..."
            let timeline = buildTimeline seed worldSize placeCount
            _ ← evaluate (force timeline)  -- do the work now
            let plateLines = formatPlatesSummary seed worldSize placeCount
            forM_ plateLines $ \line → do
                logInfo logger CatWorld line
                sendGenLog env line
            
            -- Step 2: Ocean map
            writeIORef phaseRef (LoadPhase1 2 totalSteps)
            sendGenLog env "Computing ocean map..."
            let plates = generatePlates seed worldSize placeCount
            _ ← evaluate (force plates)  -- do the work now
            let applyTL gx gy base = applyTimelineFast timeline worldSize gx gy base
                oceanMap = computeOceanMap seed worldSize placeCount plates applyTL
            _ <- evaluate (force oceanMap)  -- do the work now
            
            sendGenLog env $ "Ocean flood fill complete: "
                <> T.pack (show (HS.size oceanMap)) <> " ocean chunks"
            
            -- Step 3: Climate
            writeIORef phaseRef (LoadPhase1 3 totalSteps)
            sendGenLog env "Initializing early climate state..."
            let climateState = initEarlyClimate worldSize oceanMap
            _ ← evaluate (force climateState)  -- do the work now
            
            let weatherLines = formatWeather climateState
            forM_ weatherLines $ \line → do
                logInfo logger CatWorld line
                sendGenLog env line
            
            let params = defaultWorldGenParams
                    { wgpSeed        = seed
                    , wgpWorldSize   = worldSize
                    , wgpPlateCount  = placeCount
                    , wgpPlates      = plates
                    , wgpGeoTimeline = timeline
                    , wgpOceanMap    = oceanMap
                    , wgpClimateState = climateState
                    , wgpClimateParams = defaultClimateParams
                    }
            
            writeIORef (wsGenParamsRef worldState) (Just params)
            
            -- Step 4: Zoom cache
            writeIORef phaseRef (LoadPhase1 4 totalSteps)
            sendGenLog env "Building zoom cache..."
            let zoomCache = buildZoomCache params
            _ <- evaluate (force zoomCache)  -- do the work now
            writeIORef (wsZoomCacheRef worldState) zoomCache
            
            -- Step 5: Preview
            writeIORef phaseRef (LoadPhase1 5 totalSteps)
            sendGenLog env "Rendering world preview..."
            let preview = buildPreviewImage params zoomCache
            _ <- evaluate (force preview)  -- do the work now
            writeIORef (worldPreviewRef env) $
                Just (piWidth preview, piHeight preview, piData preview)
            sendGenLog env "World preview ready."
            
            -- Step 6: Center chunk
            writeIORef phaseRef (LoadPhase1 6 totalSteps)
            let radius = chunkLoadRadius
                totalInitialChunks = (2 * radius + 1) * (2 * radius + 1)
            sendGenLog env $ "Generating initial chunks ("
                <> T.pack (show totalInitialChunks) <> ")..."
            
            let centerCoord = ChunkCoord 0 0
                (ct, cs, cterrain, cf) = generateChunk params centerCoord
                centerChunk = LoadedChunk
                    { lcCoord      = centerCoord
                    , lcTiles      = ct
                    , lcSurfaceMap = cs
                    , lcTerrainSurfaceMap = cterrain
                    , lcFluidMap   = cf
                    , lcModified   = False
                    }
            
            atomicModifyIORef' (wsTilesRef worldState) $ \_ →
                (WorldTileData { wtdChunks = HM.singleton centerCoord centerChunk
                               , wtdMaxChunks = 200 }, ())
            
            -- Step 7: Queue remaining chunks
            writeIORef phaseRef (LoadPhase1 7 totalSteps)
            let remainingCoords = [ ChunkCoord cx cy
                                  | cx ← [-chunkLoadRadius .. chunkLoadRadius]
                                  , cy ← [-chunkLoadRadius .. chunkLoadRadius]
                                  , not (cx ≡ 0 ∧ cy ≡ 0)
                                  ]
            writeIORef (wsInitQueueRef worldState) remainingCoords
            
            -- Now switch to Phase 2 tracking
            writeIORef phaseRef (LoadPhase2 (length remainingCoords) totalInitialChunks)
            
            sendGenLog env "Calculating surface elevation..."
            let (surfaceElev, _mat) = elevationAtGlobal seed (wgpPlates params)
                                                        worldSize 0 0
                startZSlice = surfaceElev + surfaceHeadroom
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZSlice = startZSlice, camZTracking = True }, ())
            
            sendGenLog env $ "World initialized: "
                <> T.pack (show totalInitialChunks) <> " chunks queued"
            
            logInfo logger CatWorld $ "World initialized: " 
                <> T.pack (show totalInitialChunks) <> " chunks, "
                <> "surface at z=" <> T.pack (show surfaceElev)
                <> ": " <> unWorldPageId pageId
        
        WorldShow pageId → do
            logDebug logger CatWorld $ "Showing world: " <> unWorldPageId pageId
            
            atomicModifyIORef' (worldManagerRef env) $ \mgr →
                if pageId `elem` wmVisible mgr
                then (mgr, ())
                else (mgr { wmVisible = pageId : wmVisible mgr }, ())
            
            mgr ← readIORef (worldManagerRef env)
            logDebug logger CatWorld $ 
                "Visible worlds after show: " <> T.pack (show $ length $ wmVisible mgr)
        
        WorldHide pageId → do
            logDebug logger CatWorld $ "Hiding world: " <> unWorldPageId pageId
            
            atomicModifyIORef' (worldManagerRef env) $ \mgr →
                (mgr { wmVisible = filter (/= pageId) (wmVisible mgr) }, ())
        
        WorldTick dt → do
            return ()
        
        WorldSetTexture pageId texType texHandle → do
            logDebug logger CatWorld $ 
                "Setting texture for world: " <> unWorldPageId pageId 
                <> ", type: " <> T.pack (show texType)
                <> ", handle: " <> T.pack (show texHandle)
            
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState → do
                    let updateTextures wt = case texType of
                          GraniteTexture      → wt { wtGraniteTexture   = texHandle }
                          DioriteTexture      → wt { wtDioriteTexture   = texHandle }
                          GabbroTexture       → wt { wtGabbroTexture    = texHandle }
                          OceanTexture        → wt { wtOceanTexture     = texHandle }
                          GlacierTexture      → wt { wtGlacierTexture   = texHandle }
                          LavaTexture         → wt { wtLavaTexture      = texHandle }
                          BlankTexture        → wt { wtBlankTexture     = texHandle }
                          NoTexture           → wt { wtNoTexture        = texHandle }
                          IsoFaceMap          → wt { wtIsoFaceMap       = texHandle }
                          SlopeFaceMapN       → wt { wtSlopeFaceMapN    = texHandle }
                          SlopeFaceMapE       → wt { wtSlopeFaceMapE    = texHandle }
                          SlopeFaceMapNE      → wt { wtSlopeFaceMapNE   = texHandle }
                          SlopeFaceMapS       → wt { wtSlopeFaceMapS    = texHandle }
                          SlopeFaceMapNS      → wt { wtSlopeFaceMapNS   = texHandle }
                          SlopeFaceMapES      → wt { wtSlopeFaceMapES   = texHandle }
                          SlopeFaceMapNES     → wt { wtSlopeFaceMapNES  = texHandle }
                          SlopeFaceMapW       → wt { wtSlopeFaceMapW    = texHandle }
                          SlopeFaceMapNW      → wt { wtSlopeFaceMapNW   = texHandle }
                          SlopeFaceMapEW      → wt { wtSlopeFaceMapEW   = texHandle }
                          SlopeFaceMapNEW     → wt { wtSlopeFaceMapNEW  = texHandle }
                          SlopeFaceMapSW      → wt { wtSlopeFaceMapSW   = texHandle }
                          SlopeFaceMapNSW     → wt { wtSlopeFaceMapNSW  = texHandle }
                          SlopeFaceMapESW     → wt { wtSlopeFaceMapESW  = texHandle }
                          SlopeFaceMapNESW    → wt { wtSlopeFaceMapNESW = texHandle }
                          NoFaceMap           → wt { wtNoFaceMap        = texHandle }
                          ZoomGraniteTexture  → wt { wtZoomGranite      = texHandle }
                          ZoomDioriteTexture  → wt { wtZoomDiorite      = texHandle }
                          ZoomGabbroTexture   → wt { wtZoomGabbro       = texHandle }
                          ZoomOceanTexture    → wt { wtZoomOcean        = texHandle }
                          ZoomGlacierTexture  → wt { wtZoomGlacier      = texHandle }
                          ZoomLavaTexture      → wt { wtZoomLava         = texHandle }
                          BgGraniteTexture    → wt { wtBgGranite        = texHandle }
                          BgGabbroTexture     → wt { wtBgGabbro         = texHandle }
                          BgDioriteTexture    → wt { wtBgDiorite        = texHandle }
                          BgOceanTexture      → wt { wtBgOcean          = texHandle }
                          BgGlacierTexture    → wt { wtBgGlacier        = texHandle }
                          BgLavaTexture       → wt { wtBgLava           = texHandle }
                          BasaltTexture       → wt { wtBasaltTexture    = texHandle }
                          ObsidianTexture     → wt { wtObsidianTexture  = texHandle }
                          SandstoneTexture    → wt { wtSandstoneTexture = texHandle }
                          LimestoneTexture    → wt { wtLimestoneTexture = texHandle }
                          ShaleTexture        → wt { wtShaleTexture     = texHandle }
                          ImpactiteTexture    → wt { wtImpactiteTexture = texHandle }
                          IronTexture         → wt { wtIronTexture      = texHandle }
                          OlivineTexture      → wt { wtOlivineTexture   = texHandle }
                          PyroxeneTexture     → wt { wtPyroxeneTexture  = texHandle }
                          FeldsparTexture     → wt { wtFeldsparTexture  = texHandle }
                          ZoomBasaltTexture   → wt { wtZoomBasalt       = texHandle }
                          ZoomObsidianTexture → wt { wtZoomObsidian     = texHandle }
                          ZoomImpactiteTexture → wt { wtZoomImpactite   = texHandle }
                          BgBasaltTexture     → wt { wtBgBasalt         = texHandle }
                          BgImpactiteTexture  → wt { wtBgImpactite      = texHandle }
                          BgObsidianTexture    → wt { wtBgObsidian       = texHandle }
                          ZoomSandstoneTexture  → wt { wtZoomSandstone  = texHandle }
                          ZoomLimestoneTexture  → wt { wtZoomLimestone  = texHandle }
                          ZoomShaleTexture      → wt { wtZoomShale      = texHandle }
                          ZoomIronTexture       → wt { wtZoomIron       = texHandle }
                          ZoomOlivineTexture    → wt { wtZoomOlivine    = texHandle }
                          ZoomPyroxeneTexture   → wt { wtZoomPyroxene   = texHandle }
                          ZoomFeldsparTexture   → wt { wtZoomFeldspar   = texHandle }
                          BgSandstoneTexture    → wt { wtBgSandstone    = texHandle }
                          BgLimestoneTexture    → wt { wtBgLimestone    = texHandle }
                          BgShaleTexture        → wt { wtBgShale        = texHandle }
                          BgIronTexture         → wt { wtBgIron         = texHandle }
                          BgOlivineTexture      → wt { wtBgOlivine      = texHandle }
                          BgPyroxeneTexture     → wt { wtBgPyroxene     = texHandle }
                          BgFeldsparTexture     → wt { wtBgFeldspar     = texHandle }
                    atomicModifyIORef' (wsTexturesRef worldState) 
                        (\wt → (updateTextures wt, ()))
                    logDebug logger CatWorld $ 
                        "Texture updated for world: " <> unWorldPageId pageId
                Nothing → 
                    logDebug logger CatWorld $ 
                        "World not found for texture update: " <> unWorldPageId pageId
        
        WorldSetCamera pageId x y → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCameraRef worldState) $ \_ →
                        (WorldCamera x y, ())
                Nothing → 
                    logDebug logger CatWorld $ 
                        "World not found for camera update: " <> unWorldPageId pageId

        WorldSetTime pageId hour minute → do
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

        WorldSetDate pageId year month day → do
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

        WorldSetTimeScale pageId scale → do
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
        WorldSetMapMode pageId mode → do
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
        WorldSetZoomCursorHover pageId x y → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { zoomCursorPos = Just (x, y) }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for cursor hover update: " <> unWorldPageId pageId
        WorldSetZoomCursorSelect pageId → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                        (cs { zoomSelectNow = True }, ())
                Nothing → pure ()
        WorldSetZoomCursorDeselect pageId → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                        (cs { zoomSelectedPos = Nothing, zoomSelectNow = False }, ())
                Nothing → pure ()
        WorldSetZoomCursorSelectTexture pageId tid → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { zoomCursorTexture = Just tid }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for zoom cursor texture update: "
                            <> unWorldPageId pageId
        WorldSetZoomCursorHoverTexture pageId tid → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { zoomHoverTexture = Just tid }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for zoom cursor hover texture update: "
                            <> unWorldPageId pageId
        WorldSetWorldCursorHover pageId x y → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { worldCursorPos = Just (x, y) }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for cursor hover update: " <> unWorldPageId pageId
        WorldSetWorldCursorSelect pageId → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                        (cs { worldSelectNow = True }, ())
                Nothing → pure ()
        WorldSetWorldCursorDeselect pageId → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                        (cs { worldSelectedTile = Nothing, worldSelectNow = False }, ())
                Nothing → pure ()
        WorldSetWorldCursorSelectTexture pageId tid → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { worldCursorTexture = Just tid }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for cursor texture update: "
                            <> unWorldPageId pageId
        WorldSetWorldCursorHoverTexture pageId tid → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState → do
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { worldHoverTexture = Just tid }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for cursor hover texture update: "
                            <> unWorldPageId pageId
        WorldSetWorldCursorSelectBgTexture pageId tid → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState →
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { worldCursorBgTexture = Just tid }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for cursor texture update: "
                            <> unWorldPageId pageId
        WorldSetWorldCursorHoverBgTexture pageId tid → do
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState → do
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                      (cs { worldHoverBgTexture = Just tid }, ())
                Nothing → 
                    logWarn logger CatWorld $ 
                        "World not found for cursor hover texture update: "
                            <> unWorldPageId pageId
        WorldSetToolMode pagedId mode → do
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

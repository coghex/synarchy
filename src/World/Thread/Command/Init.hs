module World.Thread.Command.Init
    ( handleWorldInitCommand
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

handleWorldInitCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Word64 → Int → Int → IO ()
handleWorldInitCommand env logger pageId seed worldSize placeCount = do
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

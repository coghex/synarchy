module World.Thread.Command.Init
    ( handleWorldInitCommand
    , handleWorldInitArenaCommand
    , handleWorldInitArenaDoneCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import System.Random
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Constants (seaLevel)
import World.Generate (generateChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology (buildTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Material (getMaterialProps, MaterialProps(..))
import World.Fluids (computeOceanMap, isOceanChunk)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Save.Serialize (saveWorld)
import World.Weather (initEarlyClimate, formatWeather)
import World.Weather.Types (ClimateState(..))
import World.Generate.Config (WorldGenConfig(..), ClimateYaml(..)
                             , CalendarYaml(..), SunYaml(..), MoonYaml(..)
                             , applyConfigToParams)
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
    
    -- Step 1: Timeline (now co-evolves climate)
    writeIORef phaseRef (LoadPhase1 1 totalSteps)
    sendGenLog env "Building geological timeline..."
    worldGenCfg0 ← readIORef (worldGenConfigRef env)
    let erosionIntensity = wgcErosionIntensity worldGenCfg0
        volcanicActivity = wgcVolcanicActivity worldGenCfg0
    let (timeline, timelineClimate) = buildTimeline seed worldSize placeCount erosionIntensity volcanicActivity
    _ ← evaluate (force timeline)
    _ ← evaluate (force timelineClimate)
    registry ← readIORef (materialRegistryRef env)
    let !_ = registry `seq` ()  -- ensure registry is read before logging timeline info
    let plateLines = formatPlatesSummary seed worldSize placeCount registry
    forM_ plateLines $ \line → do
        logInfo logger CatWorld line
        sendGenLog env line

    -- Step 2: Ocean map
    writeIORef phaseRef (LoadPhase1 2 totalSteps)
    sendGenLog env "Computing ocean map..."
    let plates = generatePlates seed worldSize placeCount
    _ ← evaluate (force plates)
    let applyTL gx gy base = applyTimelineFast timeline worldSize gx gy
                               (mpHardness
                                 (getMaterialProps registry (snd base)))
                               base
        oceanMap = computeOceanMap seed worldSize placeCount plates applyTL
    _ ← evaluate (force oceanMap)

    sendGenLog env $ "Ocean flood fill complete: "
        <> T.pack (show (HS.size oceanMap)) <> " ocean chunks"

    -- Step 3: Climate — refine the timeline's co-evolved climate
    --   with the precise chunk-resolution ocean map
    writeIORef phaseRef (LoadPhase1 3 totalSteps)
    sendGenLog env "Refining climate with ocean data..."
    let climateState = initEarlyClimate worldSize oceanMap timeline
    -- Carry forward CO2 and global temp from timeline evolution
    let climateState' = climateState
            { csGlobalCO2  = csGlobalCO2 timelineClimate
            , csGlobalTemp = csGlobalTemp timelineClimate
            , csSolarConst = csSolarConst timelineClimate
            }
    _ ← evaluate (force climateState')

    let weatherLines = formatWeather climateState'
    forM_ weatherLines $ \line → do
        logInfo logger CatWorld line
        sendGenLog env line

    floraCat ← readIORef (floraCatalogRef env)
    logInfo logger CatWorld $ "Flora catalog snapshot: "
        <> T.pack (show (HM.size (fcSpecies floraCat))) <> " species, "
        <> T.pack (show (HM.size (fcWorldGen floraCat))) <> " worldgen entries"

    -- Use world gen config (already read for erosion intensity)
    let baseParams = applyConfigToParams worldGenCfg0
        params = baseParams
            { wgpSeed        = seed
            , wgpWorldSize   = worldSize
            , wgpPlateCount  = placeCount
            , wgpPlates      = plates
            , wgpGeoTimeline = timeline
            , wgpOceanMap    = oceanMap
            , wgpClimateState = climateState'
            }
    
    writeIORef (wsGenParamsRef worldState) (Just params)
    
    -- Step 4: Zoom cache + texture atlas
    writeIORef phaseRef (LoadPhase1 4 totalSteps)
    sendGenLog env "Building zoom color palette..."
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
    -- Store atlas metadata (chunksPerRow) for UV computation during baking
    writeIORef (wsZoomAtlasRef worldState) Nothing  -- will be filled after GPU upload
    -- Store chunksPerRow for later use
    logInfo logger CatWorld $ "Zoom atlas: "
        <> T.pack (show (zadWidth atlas)) <> "×"
        <> T.pack (show (zadHeight atlas)) <> " ("
        <> T.pack (show (V.length zoomCache)) <> " chunks)"
    
    -- Step 5: Preview
    writeIORef phaseRef (LoadPhase1 5 totalSteps)
    sendGenLog env "Rendering world preview..."
    let preview = buildPreviewFromPixels params zoomCache chunkPixels
    _ ← evaluate (force preview)
    writeIORef (worldPreviewRef env) $
        Just (piWidth preview, piHeight preview, piData preview)
    sendGenLog env "World preview ready."
    
    -- Step 6: Center chunk
    writeIORef phaseRef (LoadPhase1 6 totalSteps)
    let radius = chunkLoadRadius
        totalInitialChunks = (2 * radius + 1) * (2 * radius + 1)
    sendGenLog env $ "Generating initial chunks ("
        <> T.pack (show totalInitialChunks) <> ")..."
    
    catalog ← readIORef (floraCatalogRef env)
    let centerCoord = ChunkCoord 0 0
        (ct, cs, cterrain, cf, cice, cflora) = generateChunk registry catalog params centerCoord
        centerChunk = LoadedChunk
            { lcCoord      = centerCoord
            , lcTiles      = ct
            , lcSurfaceMap = cs
            , lcTerrainSurfaceMap = cterrain
            , lcFluidMap   = cf
            , lcIceMap     = cice
            , lcFlora      = cflora
            , lcSideDeco   = VU.replicate (chunkSize * chunkSize) 0
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

handleWorldInitArenaCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldInitArenaCommand env logger pageId = do
    logInfo logger CatWorld $ "Initializing test arena: " <> unWorldPageId pageId

    worldState ← emptyWorldState
    gen ← newStdGen

    -- Register early so textures sent after this command are routed correctly
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (mgr { wmWorlds = (pageId, worldState) : wmWorlds mgr }, ())

    -- Arena parameters
    let arenaRadius = 2           -- 5×5 chunks
        loamId     = 56 ∷ Word8  -- matLoam = MaterialId 56
        grassId    = 5 ∷ Word8  -- matGrass = MaterialId 5
        arenaZ     = seaLevel    -- z = 0
        chunkArea  = chunkSize * chunkSize  -- 256

        generateChunk ∷ Int → V.Vector ColumnTiles → StdGen → V.Vector ColumnTiles
        generateChunk 0    init g = init
        generateChunk area init g = generateChunk (area - 1)
                                      (V.cons newelem init) newg
            where (rand, newg) = randomR (0, 3) g
                  actualId = grassId + rand
                  newelem = (ColumnTiles
                             { ctStartZ = arenaZ
                             , ctMats   = VU.singleton loamId
                             , ctSlopes = VU.singleton 0
                             , ctVeg    = VU.singleton actualId
                             })
        flatSurfaceMap = VU.replicate chunkArea arenaZ
        flatFluidMap   = V.replicate chunkArea Nothing
        flatFlora      = emptyFloraChunkData
        flatChunk      = generateChunk chunkArea V.empty gen

        mkChunk cx cy = LoadedChunk
            { lcCoord             = ChunkCoord cx cy
            , lcTiles             = flatChunk
            , lcSurfaceMap        = flatSurfaceMap
            , lcTerrainSurfaceMap = flatSurfaceMap
            , lcFluidMap          = flatFluidMap
            , lcIceMap            = emptyIceMap
            , lcFlora             = flatFlora
            , lcSideDeco          = VU.replicate (chunkSize * chunkSize) 0
            , lcModified          = False
            }

        allChunks = [ mkChunk cx cy
                    | cx ← [-arenaRadius .. arenaRadius]
                    , cy ← [-arenaRadius .. arenaRadius]
                    ]

        chunkMap = HM.fromList [ (lcCoord c, c) | c ← allChunks ]

    -- Write tile data
    atomicModifyIORef' (wsTilesRef worldState) $ \_ →
        (WorldTileData { wtdChunks = chunkMap, wtdMaxChunks = 100 }, ())

    -- Minimal WorldGenParams so the render pipeline doesn't bail on Nothing
    let arenaParams = defaultWorldGenParams
            { wgpSeed      = 0
            , wgpWorldSize = 100000 -- arena is very big
            }
    writeIORef (wsGenParamsRef worldState) (Just arenaParams)

    -- Mark as fully loaded immediately (no progressive loading needed)
    writeIORef (wsLoadPhaseRef worldState) LoadDone

    -- Set camera z-slice to just above the surface
    atomicModifyIORef' (cameraRef env) $ \cam →
        (cam { camZSlice = arenaZ-- + surfaceHeadroom
             , camZTracking = False--True
             , camPosition = (0, 0)
             , camZoom = 0.5
             }, ())

    let totalChunks = length allChunks
    logInfo logger CatWorld $ "Test arena initialized: "
        <> T.pack (show totalChunks)
        <> " flat loam chunks at z=" <> T.pack (show arenaZ)

handleWorldInitArenaDoneCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldInitArenaDoneCommand env logger pageId = do
    logInfo logger CatWorld $ "Arena textures ready, showing: " <> unWorldPageId pageId
    
    -- Now safe to make visible — all texture commands have been processed
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        if pageId `elem` wmVisible mgr
        then (mgr, ())
        else (mgr { wmVisible = pageId : wmVisible mgr }, ())
    
    -- Broadcast to Lua that the arena is ready to display
    let lteq = luaQueue env
    Q.writeQueue lteq (LuaArenaReady (unWorldPageId pageId))

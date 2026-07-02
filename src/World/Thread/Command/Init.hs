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
import Engine.Asset.YamlTextures (MaterialDef(..), loadMaterialDirectory)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logWarn, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Material (MaterialProps(..), registerMaterial
                      , emptyMaterialRegistry)
import World.Types
import Structure.Types (emptyChunkStructures)
import World.Generate (generateChunk)
import World.Generate.Arena (generateArenaChunks)
import World.Generate.Constants (chunkLoadRadius)
import World.Geology (buildTimeline)
import World.Geology.Log (formatPlatesSummary)
import World.Plate (generatePlates, elevationAtGlobal)
import Location.Types (allLocations)
import Location.Overlay (computeLocationOverlay)
import World.Preview (buildPreviewFromPixels, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCacheWithPixels)
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.ZoomMap.ChunkTexture (buildZoomAtlas, ZoomAtlasData(..))
import World.Weather (initEarlyClimate, formatWeather)
import World.Weather.Types (ClimateState(..))
import World.Generate.Config (WorldGenConfig(..)
                              , ResourcesYaml(..)
                              , applyConfigToParams
                              , timelineParamsOf
                              , minimumWorldSize
                              , normalizeWorldGenInputs)
import World.Geology.Ore.Types (OreLevers(..))
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (dispatchLocationStamps)

handleWorldInitCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Word64 → Int → Int → IO ()
handleWorldInitCommand env logger pageId seed rawWorldSize rawPlaceCount = do
    let (worldSize, placeCount) =
            normalizeWorldGenInputs rawWorldSize rawPlaceCount
    when (worldSize ≠ rawWorldSize ∨ placeCount ≠ rawPlaceCount) $ do
        let msg = "Normalized worldgen inputs: worldSize "
                <> T.pack (show rawWorldSize) <> " → "
                <> T.pack (show worldSize) <> ", plateCount "
                <> T.pack (show rawPlaceCount) <> " → "
                <> T.pack (show placeCount)
                <> " (worldSize minimum/multiple "
                <> T.pack (show minimumWorldSize)
                <> ", plateCount min 1)."
        logWarn logger CatWorld msg
        sendGenLog env msg
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
        -- Dedup by page id: re-initialising an existing page (the common
        -- "main_world" reuse after Exit to Menu) must REPLACE its entry,
        -- not stack a second one in wmWorlds (#58).
        (mgr { wmWorlds = (pageId, worldState)
                        : filter ((/= pageId) . fst) (wmWorlds mgr) }, ())
    -- A freshly-generated page under this id is NOT any prior load's page, so
    -- drop it from the save-load provenance — otherwise a later load could
    -- treat this new world as load-owned and clobber it (#214).
    atomicModifyIORef' (loadProvenanceRef env) $ \m →
        (HM.map (HS.delete pageId) m, ())

    -- Step 0.5: Populate the material registry from data/materials/*.yaml.
    -- The registry was initialized empty at engine startup; without this
    -- pass every material would use defaultMaterialProps (uniform
    -- hardness/density/drainage), making per-material differentiation
    -- in erosion / water-table / etc. a no-op. Idempotent — reloading on
    -- successive world inits just rewrites the same data.
    sendGenLog env "Loading material registry from data/materials..."
    matDefs ← loadMaterialDirectory logger "data/materials"
    let populatedReg = foldl' (\r def →
            registerMaterial (mdId def)
                (MaterialProps (mdName def)
                               (mdHardness def)
                               (mdDensity def)
                               (mdAlbedo def)
                               (mdDrainage def)
                               (mdPickSpeed def)
                               (mdShovelSpeed def)
                               (mdDigSpoil def)
                               (mdDigBulking def)
                               (mdDigChunk def)
                               (mdDigGems def)
                               (mdMoveCost def))
                r
            ) emptyMaterialRegistry matDefs
    writeIORef (materialRegistryRef env) populatedReg

    -- Step 1: Timeline (now co-evolves climate)
    writeIORef phaseRef (LoadPhase1 1 totalSteps)
    sendGenLog env "Building geological timeline..."
    worldGenCfg0 ← readIORef (worldGenConfigRef env)
    let erosionIntensity = wgcErosionIntensity worldGenCfg0
        volcanicActivity = wgcVolcanicActivity worldGenCfg0
        lavaPoolDepth    = wgcLavaPoolDepth worldGenCfg0
        lavaPoolRadius   = wgcLavaPoolRadius worldGenCfg0
        waterfallQuantum = wgcWaterfallQuantum worldGenCfg0
        resourcesCfg     = wgcResources worldGenCfg0
        oreLevers        = OreLevers
            { olGlobal = ryOreAbundance resourcesCfg
            , olIron   = ryIronAbundance resourcesCfg
            , olCopper = ryCopperAbundance resourcesCfg
            }
    let (timeline, timelineClimate, borderedCache, oceanMap, oceanDist) = buildTimeline populatedReg seed worldSize placeCount erosionIntensity volcanicActivity lavaPoolDepth lavaPoolRadius waterfallQuantum oreLevers (timelineParamsOf worldGenCfg0)
    _ ← evaluate (force timeline)
    _ ← evaluate (force timelineClimate)
    _ ← evaluate (force borderedCache)
    registry ← readIORef (materialRegistryRef env)
    let !_ = registry `seq` ()  -- ensure registry is read before logging timeline info
    let plateLines = formatPlatesSummary seed worldSize placeCount registry
    forM_ plateLines $ \line → do
        logInfo logger CatWorld line
        sendGenLog env line

    -- Step 2: Ocean map — reuse the map buildTimeline already
    -- computed (and that the lake/seabed passes used), so every
    -- consumer shares ONE chunk-level ocean classification.
    writeIORef phaseRef (LoadPhase1 2 totalSteps)
    let plates = generatePlates seed worldSize placeCount
    _ ← evaluate (force plates)
    sendGenLog env $ "Ocean flood fill: "
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

    -- Use world gen config (already read for erosion intensity).
    -- 'withVolcanoCtx' populates the Magma context now that
    -- gtFeatures is final, so chunk-gen sees a built spatial index.
    let baseParams = applyConfigToParams worldGenCfg0
        params0 = withVolcanoCtx $ baseParams
            { wgpSeed        = seed
            , wgpWorldSize   = worldSize
            , wgpPlateCount  = placeCount
            , wgpPlates      = plates
            , wgpGeoTimeline = timeline
            , wgpOceanMap    = oceanMap
            , wgpOceanDist   = oceanDist
            , wgpClimateState = climateState'
            }

    -- Location overlay (#89): deterministically choose which chunks
    -- host the registered locations, from the just-finalised plates +
    -- ocean + lake/river data (locations keep clear of water, #414).
    -- Empty (and skipped) when no defs are loaded — the common
    -- headless-dump path stays byte-identical and zero-cost.
    locDefs ← allLocations <$> readIORef (locationDefsRef env)
    let params = params0
            { wgpLocationOverlay =
                computeLocationOverlay seed worldSize plates oceanMap oceanDist
                    (gtWorldLakes timeline) (gtWorldRivers timeline) locDefs
            }
    _ ← evaluate (force (wgpLocationOverlay params))

    writeIORef (wsGenParamsRef worldState) (Just params)
    
    -- Step 4: Zoom cache + texture atlas
    writeIORef phaseRef (LoadPhase1 4 totalSteps)
    sendGenLog env "Building zoom color palette..."
    palette ← buildColorPalette logger "data/materials" "data/vegetation"
    _ ← evaluate (force palette)

    sendGenLog env "Building zoom cache with per-chunk textures..."
    let (zoomCache, chunkPixels) =
            buildZoomCacheWithPixels params registry palette
                                     (Just borderedCache)
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
        (ct, cs, cterrain, cf, cice, cflora, cwt, cmagma) = generateChunk registry catalog params centerCoord
        seededSurf = VU.imap (\idx surfZ →
            case cf V.! idx of
                Just fc → max surfZ (fcSurface fc)
                Nothing → surfZ
            ) cs
        centerChunk = LoadedChunk
            { lcCoord      = centerCoord
            , lcTiles      = ct
            , lcSurfaceMap = seededSurf
            , lcTerrainSurfaceMap = cterrain
            , lcFluidMap   = cf
            , lcIceMap     = cice
            , lcFlora      = cflora
            , lcSideDeco   = VU.replicate (chunkSize * chunkSize) 0
            , lcWaterTableMap = cwt
            , lcMagma      = cmagma
            , lcStructures = emptyChunkStructures
            }

    atomicModifyIORef' (wsTilesRef worldState) $ \_ →
        (WorldTileData { wtdChunks = HM.singleton centerCoord centerChunk
                       , wtdMaxChunks = 200 }, ())

    -- Stamp any placed location on the synchronously-generated centre
    -- chunk (#89). It is written straight to wsTilesRef and excluded from
    -- the init queue, so the chunk-loading dispatch never sees it.
    dispatchLocationStamps env params pageId [centerChunk]

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
        -- Dedup by page id: re-initialising an existing page (the common
        -- "main_world" reuse after Exit to Menu) must REPLACE its entry,
        -- not stack a second one in wmWorlds (#58).
        (mgr { wmWorlds = (pageId, worldState)
                        : filter ((/= pageId) . fst) (wmWorlds mgr) }, ())
    -- Fresh arena under this id is not any prior load's page (see #214).
    atomicModifyIORef' (loadProvenanceRef env) $ \m →
        (HM.map (HS.delete pageId) m, ())

    -- Arena chunk set: shared with the save-load restore path (#365) so a
    -- loaded arena page is rebuilt exactly like a fresh one.
    let arenaZ    = seaLevel    -- z = 0 (surface)
        allChunks = generateArenaChunks gen
        chunkMap  = HM.fromList [ (lcCoord c, c) | c ← allChunks ]

    -- Write tile data
    atomicModifyIORef' (wsTilesRef worldState) $ \_ →
        (WorldTileData { wtdChunks = chunkMap, wtdMaxChunks = 100 }, ())

    -- Force the arena chunks to NF so the LoadDone below is honest (same
    -- contract as the progressive loader). Tiny 5×5 arena, negligible cost.
    _ ← evaluate (force allChunks)

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
        (cam { camZSlice = arenaZ + surfaceHeadroom
             , camZTracking = True
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

{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread
    ( startWorldThread
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Data.List (partition, sortOn)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef', newIORef)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.Concurrent (forkIO, threadDelay)
import Control.DeepSeq (NFData, rnf, force)
import Control.Exception (SomeException, catch, evaluate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Generate
import World.Grid (zoomFadeEnd, worldToGrid)
import World.Geology (buildTimeline, logTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, computeChunkFluid, isOceanChunk, seaLevel)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom, updateWorldTiles)
import World.ZoomMap (buildZoomCache)
import World.Save.Serialize (saveWorld)
import World.Slope (recomputeNeighborSlopes)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Weather.Types (ClimateCoord(..), ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , OceanGrid(..), OceanCell(..)
                           , climateRegionSize)
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..))

-----------------------------------------------------------
-- Helper: send a progress message to Lua
-----------------------------------------------------------

sendGenLog ∷ EngineEnv → Text → IO ()
sendGenLog env msg = Q.writeQueue (luaQueue env) (LuaWorldGenLog msg)

-- | info message to lua's hud
sendHudInfo ∷ EngineEnv → Text → Text → IO ()
sendHudInfo env msgbas msgadv = Q.writeQueue (luaQueue env)
                                  (LuaHudLogInfo msgbas msgadv)

-- | send weather info to lua's hud
sendHudWeatherInfo ∷ EngineEnv → Text → IO ()
sendHudWeatherInfo env weatherText = Q.writeQueue (luaQueue env)
                                            (LuaHudLogWeatherInfo weatherText)

-----------------------------------------------------------
-- Start World Thread
-----------------------------------------------------------

startWorldThread ∷ EngineEnv → IO ThreadState
startWorldThread env = do
    logger ← readIORef (loggerRef env)
    stateRef ← newIORef ThreadRunning
    threadId ← catch
        (do
            logInfo logger CatWorld "Starting world thread..."
            lastTimeRef ← getPOSIXTime ⌦ newIORef . realToFrac
            tid ← forkIO $ worldLoop env stateRef lastTimeRef
            logInfo logger CatWorld "World thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatWorld $ "Failed starting world thread: " <> T.pack (show e)
            error "World thread start failure."
        )
    
    return $ ThreadState stateRef threadId

-----------------------------------------------------------
-- World Loop
-----------------------------------------------------------

worldLoop ∷ EngineEnv → IORef ThreadControl → IORef Double → IO ()
worldLoop env stateRef lastTimeRef = do
    control ← readIORef stateRef
    logger ← readIORef (loggerRef env)
    
    case control of
        ThreadStopped → do
            logDebug logger CatWorld "World thread stopping..."
            pure ()
        ThreadPaused → do
            threadDelay 100000
            worldLoop env stateRef lastTimeRef
        ThreadRunning → do
            now ← realToFrac ⊚ getPOSIXTime
            lastTime ← readIORef lastTimeRef
            let dt = now - lastTime ∷ Double
            writeIORef lastTimeRef now
            
            processAllCommands env logger
            
            -- Drain initial chunk queues (progressive loading)
            drainInitQueues env logger
            
            tickWorldTime env (realToFrac dt)
            updateChunkLoading env logger
            pollCursorInfo env

            camera ← readIORef (cameraRef env)
            allQuads ← updateWorldTiles env
            writeIORef (worldQuadsRef env) allQuads
            threadDelay 16666
            worldLoop env stateRef lastTimeRef

-- | Drain all pending commands from the queue
processAllCommands ∷ EngineEnv → LoggerState → IO ()
processAllCommands env logger = do
    mCmd ← Q.tryReadQueue (worldQueue env)
    case mCmd of
        Just cmd → do
            handleWorldCommand env logger cmd
            processAllCommands env logger  -- drain all pending
        Nothing → return ()

-----------------------------------------------------------
-- Cursor Info Polling
-----------------------------------------------------------

-- | Poll cursor state for all visible worlds. When the selected
--   tile/chunk changes (or is deselected), send the appropriate
--   info text to the HUD. Runs every world-loop tick.
pollCursorInfo ∷ EngineEnv → IO ()
pollCursorInfo env = do
    manager ← readIORef (worldManagerRef env)
    forM_ (wmVisible manager) $ \pageId →
        case lookup pageId (wmWorlds manager) of
            Nothing → return ()
            Just worldState → do
                cs   ← readIORef (wsCursorRef worldState)
                snap ← readIORef (wsCursorSnapshotRef worldState)
                mParams ← readIORef (wsGenParamsRef worldState)

                let curZoom  = zoomSelectedPos cs
                    curWorld = worldSelectedTile cs
                    oldZoom  = csZoomSel snap
                    oldWorld = csWorldSel snap

                -- Zoom-level chunk selection changed
                when (curZoom ≢ oldZoom) $ do
                    case curZoom of
                        Nothing → sendHudInfo env "" ""
                        Just (baseGX, baseGY) →
                            sendChunkInfo env worldState mParams baseGX baseGY

                -- World-level tile selection changed
                when (curWorld ≢ oldWorld) $ do
                    case curWorld of
                        Nothing → sendHudInfo env "" ""
                        Just (gx, gy, z) →
                            sendTileInfo env worldState mParams gx gy z

                -- Update snapshot
                let newSnap = CursorSnapshot curZoom curWorld
                when (newSnap ≢ snap) $
                    writeIORef (wsCursorSnapshotRef worldState) newSnap

-----------------------------------------------------------
-- sendChunkInfo: zoom-level (chunk) selection
-----------------------------------------------------------

-- | Format and send HUD info for a selected chunk (zoomed-out view).
--   baseGX/baseGY are the chunk's global grid origin (i.e. chunkX * chunkSize).
sendChunkInfo ∷ EngineEnv → WorldState → Maybe WorldGenParams
              → Int → Int → IO ()
sendChunkInfo env worldState mParams baseGX baseGY = do
    let cx = if baseGX >= 0 then baseGX `div` chunkSize
             else -(((-baseGX) + chunkSize - 1) `div` chunkSize)
        cy = if baseGY >= 0 then baseGY `div` chunkSize
             else -(((-baseGY) + chunkSize - 1) `div` chunkSize)
        coord = ChunkCoord cx cy

    -- Try to find this chunk's zoom cache entry for material/elevation
    zoomCache ← readIORef (wsZoomCacheRef worldState)
    let mEntry = V.find (\e → zceChunkX e ≡ cx ∧ zceChunkY e ≡ cy) zoomCache

    let basicLines = T.unlines $ filter (not . T.null)
            [ "Chunk (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
            , case mEntry of
                Just entry →
                    let props = getMaterialProps (MaterialId (zceTexIndex entry))
                    in "Material: " <> matName props
                     <> "\nElevation: " <> T.pack (show (zceElev entry))
                     <> (if zceIsOcean entry then "\nOcean" else "")
                     <> (if zceHasLava entry then "\nLava" else "")
                Nothing → ""
            ]

    let advLines = T.unlines $ filter (not . T.null)
            [ "Grid origin: (" <> T.pack (show baseGX)
              <> ", " <> T.pack (show baseGY) <> ")"
            , case mEntry of
                Just entry →
                    "MatID: " <> T.pack (show (zceTexIndex entry))
                Nothing → ""
            , case mParams of
                Just params →
                    let ocean = isOceanChunk (wgpOceanMap params) coord
                    in "Ocean map: " <> T.pack (show ocean)
                Nothing → ""
            ]
        weatherInfo = case mParams of
            Just params → chunkWeatherInfo params cx cy
            Nothing → ""

    sendHudInfo env basicLines advLines
    sendHudWeatherInfo env weatherInfo

-----------------------------------------------------------
-- chunkWeatherInfo: format weather for a chunk's climate region
-----------------------------------------------------------

-- | Given a chunk's (cx, cy) and the world gen params, look up the
--   climate region that contains this chunk and format it as a
--   multi-line Text for the HUD weather tab.
chunkWeatherInfo ∷ WorldGenParams → Int → Int → Text
chunkWeatherInfo params cx cy =
    let worldSize  = wgpWorldSize params
        halfChunks = worldSize `div` 2

        -- Convert (cx, cy) → (u, v) chunk space
        chunkU = cx - cy
        chunkV = cx + cy

        -- Climate region indices (same formula as ZoomMap / initEarlyClimate)
        ru = (chunkU + halfChunks) `div` climateRegionSize
        rv = (chunkV + halfChunks) `div` climateRegionSize
        coord = ClimateCoord ru rv

        climateGrid = cgRegions (csClimate (wgpClimateState params))
        oceanGrid   = ogCells   (csOcean   (wgpClimateState params))

    in case HM.lookup coord climateGrid of
        Nothing → "No climate data"
        Just rc →
            let showF1 f =
                    let whole = floor f ∷ Int
                        frac  = abs (round ((f - fromIntegral whole) * 10.0) ∷ Int)
                    in T.pack (show whole) <> "." <> T.pack (show frac)
                showSeas (SeasonalClimate s w) =
                    showF1 s <> " / " <> showF1 w

                -- Ocean cell info (if this region is ocean)
                oceanLine = case HM.lookup coord oceanGrid of
                    Nothing → ""
                    Just oc →
                        "SST: " <> showSeas (ocTemperature oc)
                        <> "\nSalinity: " <> showF1 (ocSalinity oc)
                        <> "\nIce cover: " <> showF1 (ocIceCover oc)
                        <> "\nUpwelling: " <> showF1 (ocUpwelling oc)

            in T.unlines $ filter (not . T.null)
                [ "Region (" <> T.pack (show ru) <> ", " <> T.pack (show rv) <> ")"
                , "Air temp (S/W): " <> showSeas (rcAirTemp rc) <> " C"
                , "Humidity: " <> showF1 (rcHumidity rc)
                , "Precip (S/W): " <> showSeas (rcPrecipitation rc)
                , "Cloud cover: " <> showF1 (rcCloudCover rc)
                , "Pressure: " <> showF1 (rcPressure rc)
                , "Wind: " <> showF1 (rcWindSpeed rc)
                  <> " @ " <> showF1 (rcWindDir rc) <> " rad"
                , "Continentality: " <> showF1 (rcContinentality rc)
                , "Albedo: " <> showF1 (rcAlbedo rc)
                , "Elev avg: " <> T.pack (show (rcElevAvg rc))
                , oceanLine
                ]

-----------------------------------------------------------
-- sendTileInfo: world-level (tile) selection
-----------------------------------------------------------

-- | Format and send HUD info for a selected tile (zoomed-in view).
--   gx/gy are global grid coords, z is the z-level the cursor hit.
sendTileInfo ∷ EngineEnv → WorldState → Maybe WorldGenParams
             → Int → Int → Int → IO ()
sendTileInfo env worldState _mParams gx gy z = do
    tileData ← readIORef (wsTilesRef worldState)

    let (coord, (lx, ly)) = globalToChunk gx gy
        mChunk = lookupChunk coord tileData
        colIdx = columnIndex lx ly

    let (matText, surfText, fluidText) = case mChunk of
            Nothing → ("(unloaded)", "", "")
            Just lc →
                let col  = (lcTiles lc) V.! colIdx
                    surfZ = (lcSurfaceMap lc) VU.! colIdx
                    -- Material at the SELECTED z-level, not the surface
                    relZ = z - ctStartZ col
                    selectedMat =
                        if relZ >= 0 && relZ < VU.length (ctMats col)
                        then ctMats col VU.! relZ
                        else 0
                    props = getMaterialProps (MaterialId selectedMat)
                    -- Fluid info
                    mFluid = (lcFluidMap lc) V.! colIdx
                    fluidStr = case mFluid of
                        Nothing → ""
                        Just fc → "Fluid: " <> T.pack (show (fcType fc))
                                <> " (surface z=" <> T.pack (show (fcSurface fc)) <> ")"
                in ( matName props
                   , T.pack (show surfZ)
                   , fluidStr
                   )

    let basicLines = T.unlines $ filter (not . T.null)
            [ "Tile (" <> T.pack (show gx) <> ", " <> T.pack (show gy) <> ")"
            , "Material: " <> matText
            , "Surface: " <> surfText
            , "Z: " <> T.pack (show z)
            , fluidText
            ]

    let ChunkCoord ccx ccy = coord
        advLines = T.unlines $ filter (not . T.null)
            [ "Chunk: (" <> T.pack (show ccx) <> ", " <> T.pack (show ccy) <> ")"
            , "Local: (" <> T.pack (show lx) <> ", " <> T.pack (show ly) <> ")"
            , "Column start Z: " <> case mChunk of
                  Nothing → "?"
                  Just lc → let col = (lcTiles lc) V.! colIdx
                             in T.pack (show (ctStartZ col))
            ]

    sendHudInfo env basicLines advLines

-----------------------------------------------------------
-- World Time Tick
-----------------------------------------------------------

-- | Advance time for all visible worlds, write sun angle to the shared ref.
--   If multiple worlds are visible, the first one's time wins for sun angle.
tickWorldTime ∷ EngineEnv → Float → IO ()
tickWorldTime env dt = do
    manager ← readIORef (worldManagerRef env)
    
    -- Tick each visible world's time
    forM_ (wmVisible manager) $ \pageId →
        case lookup pageId (wmWorlds manager) of
            Nothing → return ()
            Just worldState → do
                timeScale ← readIORef (wsTimeScaleRef worldState)
                atomicModifyIORef' (wsTimeRef worldState) $ \wt →
                    (advanceWorldTime timeScale dt wt, ())
    
    -- Write sun angle from the first visible world
    case wmVisible manager of
        (pageId:_) → case lookup pageId (wmWorlds manager) of
            Just worldState → do
                wt ← readIORef (wsTimeRef worldState)
                let sunAngle = worldTimeToSunAngle wt
                atomicModifyIORef' (sunAngleRef env) $ \_ → (sunAngle, ())
            Nothing → return ()
        [] → return ()

-----------------------------------------------------------
-- Chunk Loading
-----------------------------------------------------------

-- | Maximum chunks to generate per world loop iteration.
--   Keeps the world thread responsive so quad rendering
--   isn't starved during camera panning.
maxChunksPerTick ∷ Int
maxChunksPerTick = 4

updateChunkLoading ∷ EngineEnv → LoggerState → IO ()
updateChunkLoading env logger = do
    camera ← readIORef (cameraRef env)
    let zoom = camZoom camera

    when (zoom < (zoomFadeEnd + 0.5)) $ do
        manager ← readIORef (worldManagerRef env)

        let (camX, camY) = camPosition camera
            facing = camFacing camera
            camChunk = cameraChunkCoord facing camX camY
            ChunkCoord ccx ccy = camChunk

            neededCoords = [ ChunkCoord (ccx + dx) (ccy + dy)
                           | dx ← [-chunkLoadRadius .. chunkLoadRadius]
                           , dy ← [-chunkLoadRadius .. chunkLoadRadius]
                           ]

        forM_ (wmVisible manager) $ \pageId →
            case lookup pageId (wmWorlds manager) of
                Nothing → return ()
                Just worldState → do
                    mParams ← readIORef (wsGenParamsRef worldState)
                    case mParams of
                        Nothing → return ()
                        Just params → do
                            tileData ← readIORef (wsTilesRef worldState)
                            

                            let halfSize = wgpWorldSize params `div` 2
                                -- Wrap chunk coords in u-space (ccx - ccy),
                                -- not just ccx alone
                                wrapChunkU (ChunkCoord cx cy) =
                                    let w = halfSize * 2
                                        u = cx - cy
                                        v = cx + cy
                                        halfW = w `div` 2
                                        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
                                        cx' = (wrappedU + v) `div` 2
                                        cy' = (v - wrappedU) `div` 2
                                    in ChunkCoord cx' cy'
                                inBoundsV (ChunkCoord cx cy) =
                                    let v = cx + cy
                                        halfTiles = halfSize * chunkSize
                                    in abs (v * chunkSize) ≤ halfTiles
                                validCoords = map wrapChunkU $ filter inBoundsV neededCoords

                            let (_toPromote, toGenerate) = partitionChunks validCoords tileData
                            
                            -- Only generate a limited batch per tick
                            let toGenerateSorted = sortOn (\(ChunkCoord cx cy) →
                                    abs (cx - ccx) + abs (cy - ccy)) toGenerate
                                batch = take maxChunksPerTick toGenerateSorted

                            -- force chunk eval here
                            when (not $ null batch) $ do
                                let seed = wgpSeed params
                                let !newChunks = parMap rdeepseq (\coord →
                                        let (chunkTiles, surfMap, tMap, fluidMap) = generateChunk params coord
                                        in LoadedChunk
                                            { lcCoord      = coord
                                            , lcTiles      = chunkTiles
                                            , lcSurfaceMap = surfMap
                                            , lcTerrainSurfaceMap = tMap
                                            , lcFluidMap   = fluidMap
                                            , lcModified   = False
                                            }) batch
                                -- Insert new chunks, then recompute slopes
                                -- for the new chunks + their existing neighbors
                                atomicModifyIORef' (wsTilesRef worldState) $ \td →
                                    let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks
                                        td'' = evictDistantChunks camChunk chunkLoadRadius td'
                                        td''' = recomputeNeighborSlopes seed
                                                  (map lcCoord newChunks) td''
                                    in (td''', ())

                                -- Invalidate render caches so new chunks appear
                                writeIORef (wsQuadCacheRef worldState) Nothing
                                writeIORef (wsZoomQuadCacheRef worldState) Nothing
                                writeIORef (wsBgQuadCacheRef worldState) Nothing

-- | Partition needed chunk coords into those already loaded (promote)
--   and those that need generation.
partitionChunks ∷ [ChunkCoord] → WorldTileData → ([ChunkCoord], [ChunkCoord])
partitionChunks coords tileData =
    partition (\coord → HM.member coord (wtdChunks tileData)) coords

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

unWorldPageId ∷ WorldPageId → Text
unWorldPageId (WorldPageId t) = t

-- | Generate a limited batch of chunks from each world's init queue.
--   Runs every world tick until all initial chunks are loaded.
drainInitQueues ∷ EngineEnv → LoggerState → IO ()
drainInitQueues env logger = do
    manager ← readIORef (worldManagerRef env)
    forM_ (wmWorlds manager) $ \(pageId, worldState) → do
        remaining ← readIORef (wsInitQueueRef worldState)
        case remaining of
            [] → return ()
            _  → do
                mParams ← readIORef (wsGenParamsRef worldState)
                case mParams of
                    Nothing → return ()
                    Just params → do
                        let batch = take maxChunksPerTick remaining
                            rest  = drop maxChunksPerTick remaining
                            seed  = wgpSeed params
                        
                        let newChunks = parMap rdeepseq (\coord →
                                let (chunkTiles, surfMap, tMap, fluidMap) = generateChunk params coord
                                in LoadedChunk
                                    { lcCoord      = coord
                                    , lcTiles      = chunkTiles
                                    , lcSurfaceMap = surfMap
                                    , lcTerrainSurfaceMap = tMap
                                    , lcFluidMap   = fluidMap
                                    , lcModified   = False
                                    }) batch
                        
                        -- Insert new chunks, then recompute slopes
                        -- for the new chunks + their existing neighbors
                        atomicModifyIORef' (wsTilesRef worldState) $ \td →
                            let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks
                                td'' = recomputeNeighborSlopes seed
                                         (map lcCoord newChunks) td'
                            in (td'', ())
                        
                        -- Invalidate all render caches so new chunks appear immediately
                        writeIORef (wsQuadCacheRef worldState) Nothing
                        writeIORef (wsZoomQuadCacheRef worldState) Nothing
                        writeIORef (wsBgQuadCacheRef worldState) Nothing
                        
                        writeIORef (wsInitQueueRef worldState) rest
                        
                        -- Update phase 2 progress
                        let totalChunks = (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)
                        writeIORef (wsLoadPhaseRef worldState)
                            (if null rest
                             then LoadDone
                             else LoadPhase2 (length rest) totalChunks)
                        
                        when (null rest) $
                            logDebug logger CatWorld $
                                "Initial chunk loading complete for: "
                                <> unWorldPageId pageId

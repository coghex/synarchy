{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread
    ( startWorldThread
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.List (partition)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef', newIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Generate
import World.Grid (zoomFadeEnd)
import World.Geology (buildTimeline, logTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, computeChunkFluid)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCache)

-----------------------------------------------------------
-- Helper: send a progress message to Lua
-----------------------------------------------------------

sendGenLog ∷ EngineEnv → Text → IO ()
sendGenLog env msg = Q.writeQueue (luaQueue env) (LuaWorldGenLog msg)

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
            -- Calculate delta time
            now ← realToFrac ⊚ getPOSIXTime
            lastTime ← readIORef lastTimeRef
            let dt = now - lastTime ∷ Double
            writeIORef lastTimeRef now
            
            -- Process all pending commands
            processAllCommands env logger
            
            -- Tick world time for all visible worlds and update sunAngleRef
            tickWorldTime env (realToFrac dt)
            
            -- Check chunk loading for all visible worlds
            updateChunkLoading env logger
            
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

-- | Check camera position and load/promote chunks as needed.
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
                                wrapChunkX cx =
                                    let wrapped = ((cx + halfSize) `mod` (halfSize * 2) + (halfSize * 2))
                                                  `mod` (halfSize * 2) - halfSize
                                    in wrapped
                                inBoundsY (ChunkCoord _ cy) =
                                    cy ≥ -halfSize ∧ cy < halfSize
                                wrapCoord (ChunkCoord cx cy) = ChunkCoord (wrapChunkX cx) cy
                                validCoords = map wrapCoord $ filter inBoundsY neededCoords

                            let (toPromote, toGenerate) = partitionChunks validCoords tileData

                            when (not $ null toGenerate) $ do
                                let newChunks = map (\coord →
                                        let (chunkTiles, surfMap, fluidMap) = generateChunk params coord
                                        in LoadedChunk
                                            { lcCoord      = coord
                                            , lcTiles      = chunkTiles
                                            , lcSurfaceMap = surfMap
                                            , lcFluidMap   = fluidMap
                                            , lcModified   = False
                                            }) toGenerate

                                atomicModifyIORef' (wsTilesRef worldState) $ \td →
                                    let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks
                                        td'' = evictDistantChunks camChunk chunkLoadRadius td'
                                    in (td'', ())

                                logDebug logger CatWorld $
                                    "Loaded " <> T.pack (show $ length toGenerate)
                                    <> " new chunks around " <> T.pack (show camChunk)
                                    <> " (" <> T.pack (show $ chunkCount tileData + length toGenerate)
                                    <> " total)"
                        
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
            
            -- Log the geological timeline to both stdout and Lua panel
            sendGenLog env "Building geological timeline..."
            let timeline = buildTimeline seed worldSize placeCount
                params = defaultWorldGenParams
                    { wgpSeed        = seed
                    , wgpWorldSize   = worldSize
                    , wgpPlateCount  = placeCount
                    , wgpGeoTimeline = timeline
                    }

            -- Log plates first (not stored in timeline, computed from seed)
            let plateLines = formatPlatesSummary seed worldSize placeCount
            forM_ plateLines $ \line → do
                logInfo logger CatWorld line
                sendGenLog env line

            -- Log the full chronological timeline
            let timelineLines = formatTimeline timeline
            forM_ timelineLines $ \line → do
                logInfo logger CatWorld line
                sendGenLog env line

            -- Store gen params for on-demand chunk loading
            writeIORef (wsGenParamsRef worldState) (Just params)

            sendGenLog env "Computing ocean map..."
            let plates = generatePlates seed worldSize placeCount
                oceanMap = computeOceanMap seed worldSize placeCount plates
            
            sendGenLog env $ "Ocean flood fill complete: "
                <> T.pack (show (HS.size oceanMap)) <> " ocean chunks"

            let params = defaultWorldGenParams
                    { wgpSeed        = seed
                    , wgpWorldSize   = worldSize
                    , wgpPlateCount  = placeCount
                    , wgpGeoTimeline = timeline
                    , wgpOceanMap    = oceanMap
                    }

            -- Build zoom map cache (one-time computation)
            sendGenLog env "Generating tectonic plates..."
            camera ← readIORef (cameraRef env)
            let facing = camFacing camera

            sendGenLog env "Building zoom cache..."
            let zoomCache = buildZoomCache facing params
            writeIORef (wsZoomCacheRef worldState) zoomCache

            -- Build world preview image for the create-world UI
            sendGenLog env "Rendering world preview..."
            let preview = buildPreviewImage params zoomCache
            writeIORef (worldPreviewRef env) $
                Just (piWidth preview, piHeight preview, piData preview)
            sendGenLog env "World preview ready."
            
            -- Generate the initial 5×5 chunk grid around (0,0)
            let initialCoords = [ ChunkCoord cx cy
                                | cx ← [-chunkLoadRadius .. chunkLoadRadius]
                                , cy ← [-chunkLoadRadius .. chunkLoadRadius]
                                ]
                totalInitialChunks = length initialCoords

            sendGenLog env $ "Generating initial chunks ("
                <> T.pack (show totalInitialChunks) <> ")..."

            let initialChunks = map (\coord →
                    let (chunkTiles, surfMap, fluidMap) = generateChunk params coord
                    in LoadedChunk
                        { lcCoord      = coord
                        , lcTiles      = chunkTiles
                        , lcSurfaceMap = surfMap
                        , lcFluidMap   = fluidMap
                        , lcModified   = False
                        }) initialCoords
            
            atomicModifyIORef' (wsTilesRef worldState) $ \_ →
                (WorldTileData { wtdChunks = HM.fromList [(lcCoord lc, lc) | lc ← initialChunks]
                               , wtdMaxChunks = 200 }, ())
            
            atomicModifyIORef' (worldManagerRef env) $ \mgr →
                (mgr { wmWorlds = (pageId, worldState) : wmWorlds mgr }, ())
            
            -- Set the camera z-slice to just above the surface at (0,0)
            sendGenLog env "Calculating surface elevation..."
            let plates = generatePlates seed worldSize (wgpPlateCount params)
                (surfaceElev, _mat) = elevationAtGlobal seed plates worldSize 0 0
                startZSlice = surfaceElev + surfaceHeadroom
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZSlice = startZSlice, camZTracking = True }, ())
            
            let totalTiles = sum $ map (HM.size . lcTiles) initialChunks
            let summaryMsg = T.pack (show $ length initialChunks) <> " chunks, "
                          <> T.pack (show totalTiles) <> " tiles"
            sendGenLog env $ "World initialized: " <> summaryMsg

            logInfo logger CatWorld $ "World initialized: " 
                <> T.pack (show $ length initialChunks) <> " chunks, "
                <> T.pack (show totalTiles) <> " tiles, "
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

unWorldPageId ∷ WorldPageId → Text
unWorldPageId (WorldPageId t) = t

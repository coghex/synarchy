{-# LANGUAGE Strict #-}
module World.Thread
    ( startWorldThread
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (partition)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef', newIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Exception (SomeException, catch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Engine.Core.Queue as Q
import World.Types
import World.Generate
import World.Plate (generatePlates, elevationAtGlobal)
import World.ZoomMap (buildZoomCache)

-----------------------------------------------------------
-- Start World Thread
-----------------------------------------------------------

startWorldThread :: EngineEnv -> IO ThreadState
startWorldThread env = do
    logger <- readIORef (loggerRef env)
    stateRef <- newIORef ThreadRunning
    threadId <- catch
        (do
            logInfo logger CatWorld "Starting world thread..."
            lastTimeRef <- getPOSIXTime >>= newIORef . realToFrac
            tid <- forkIO $ worldLoop env stateRef lastTimeRef
            logInfo logger CatWorld "World thread started"
            return tid
        )
        (\(e :: SomeException) -> do
            logError logger CatWorld $ "Failed starting world thread: " <> T.pack (show e)
            error "World thread start failure."
        )
    
    return $ ThreadState stateRef threadId

-----------------------------------------------------------
-- World Loop
-----------------------------------------------------------

worldLoop :: EngineEnv -> IORef ThreadControl -> IORef Double -> IO ()
worldLoop env stateRef lastTimeRef = do
    control <- readIORef stateRef
    logger <- readIORef (loggerRef env)
    
    case control of
        ThreadStopped -> do
            logDebug logger CatWorld "World thread stopping..."
            pure ()
        ThreadPaused -> do
            threadDelay 100000
            worldLoop env stateRef lastTimeRef
        ThreadRunning -> do
            -- Calculate delta time
            now <- realToFrac <$> getPOSIXTime
            lastTime <- readIORef lastTimeRef
            let dt = now - lastTime :: Double
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
processAllCommands :: EngineEnv -> LoggerState -> IO ()
processAllCommands env logger = do
    mCmd <- Q.tryReadQueue (worldQueue env)
    case mCmd of
        Just cmd -> do
            handleWorldCommand env logger cmd
            processAllCommands env logger  -- drain all pending
        Nothing -> return ()

-----------------------------------------------------------
-- World Time Tick
-----------------------------------------------------------

-- | Advance time for all visible worlds, write sun angle to the shared ref.
--   If multiple worlds are visible, the first one's time wins for sun angle.
tickWorldTime :: EngineEnv -> Float -> IO ()
tickWorldTime env dt = do
    manager <- readIORef (worldManagerRef env)
    
    -- Tick each visible world's time
    forM_ (wmVisible manager) $ \pageId ->
        case lookup pageId (wmWorlds manager) of
            Nothing -> return ()
            Just worldState -> do
                timeScale <- readIORef (wsTimeScaleRef worldState)
                atomicModifyIORef' (wsTimeRef worldState) $ \wt ->
                    (advanceWorldTime timeScale dt wt, ())
    
    -- Write sun angle from the first visible world
    case wmVisible manager of
        (pageId:_) -> case lookup pageId (wmWorlds manager) of
            Just worldState -> do
                wt <- readIORef (wsTimeRef worldState)
                let sunAngle = worldTimeToSunAngle wt
                atomicModifyIORef' (sunAngleRef env) $ \_ -> (sunAngle, ())
            Nothing -> return ()
        [] -> return ()

-----------------------------------------------------------
-- Chunk Loading
-----------------------------------------------------------

-- | Check camera position and load/promote chunks as needed.
--   Runs every tick on the world thread.
updateChunkLoading :: EngineEnv -> LoggerState -> IO ()
updateChunkLoading env logger = do
    manager <- readIORef (worldManagerRef env)
    camera  <- readIORef (cameraRef env)
    
    let (camX, camY) = camPosition camera
        camChunk = cameraChunkCoord camX camY
        ChunkCoord ccx ccy = camChunk
        
        neededCoords = [ ChunkCoord (ccx + dx) (ccy + dy)
                       | dx <- [-chunkLoadRadius .. chunkLoadRadius]
                       , dy <- [-chunkLoadRadius .. chunkLoadRadius]
                       ]
    
    forM_ (wmVisible manager) $ \pageId ->
        case lookup pageId (wmWorlds manager) of
            Nothing -> return ()
            Just worldState -> do
                mParams <- readIORef (wsGenParamsRef worldState)
                case mParams of
                    Nothing -> return ()
                    Just params -> do
                        tileData <- readIORef (wsTilesRef worldState)
                        
                        let halfSize = wgpWorldSize params `div` 2
                            -- Y is clamped (glacier), X wraps
                            wrapChunkX cx =
                                let wrapped = ((cx + halfSize) `mod` (halfSize * 2) + (halfSize * 2))
                                              `mod` (halfSize * 2) - halfSize
                                in wrapped
                            inBoundsY (ChunkCoord _ cy) =
                                cy >= -halfSize && cy < halfSize
                            wrapCoord (ChunkCoord cx cy) = ChunkCoord (wrapChunkX cx) cy
                            validCoords = map wrapCoord $ filter inBoundsY neededCoords
                        
                        let (toPromote, toGenerate) = partitionChunks validCoords tileData
                        
                        when (not $ null toGenerate) $ do
                            let newChunks = map (\coord ->
                                    let (chunkTiles, surfMap) = generateChunk params coord
                                    in LoadedChunk
                                        { lcCoord      = coord
                                        , lcTiles      = chunkTiles
                                        , lcSurfaceMap = surfMap
                                        , lcModified   = False
                                        }) toGenerate
                            
                            atomicModifyIORef' (wsTilesRef worldState) $ \td ->
                                let td' = foldl' (\acc lc -> insertChunk lc acc) td newChunks
                                    td'' = foldl' (\acc coord -> promoteChunk coord acc) td' toPromote
                                in (td'', ())
                            
                            logDebug logger CatWorld $
                                "Loaded " <> T.pack (show $ length toGenerate)
                                <> " new chunks around " <> T.pack (show camChunk)
                                <> " (" <> T.pack (show $ chunkCount tileData + length toGenerate)
                                <> " total)"
                        
                        when (not (null toPromote) && null toGenerate) $ do
                            atomicModifyIORef' (wsTilesRef worldState) $ \td ->
                                let td' = foldl' (\acc coord -> promoteChunk coord acc) td toPromote
                                in (td', ())

-- | Partition needed chunk coords into those already loaded (promote)
--   and those that need generation.
partitionChunks :: [ChunkCoord] -> WorldTileData -> ([ChunkCoord], [ChunkCoord])
partitionChunks coords tileData =
    let isLoaded coord = case lookupChunk coord tileData of
            Just _  -> True
            Nothing -> False
    in partition isLoaded coords

-----------------------------------------------------------
-- Command Handler
-----------------------------------------------------------

handleWorldCommand :: EngineEnv -> LoggerState -> WorldCommand -> IO ()
handleWorldCommand env logger cmd = do
    logDebug logger CatWorld $ "Processing world command: " <> T.pack (show cmd)
    
    case cmd of
        WorldInit pageId seed worldSize -> do
            logDebug logger CatWorld $ "Initializing world: " <> unWorldPageId pageId
                <> " (seed=" <> T.pack (show seed)
                <> ", size=" <> T.pack (show worldSize) <> " chunks)"
            
            worldState <- emptyWorldState
            
            let params = defaultWorldGenParams
                    { wgpSeed      = seed
                    , wgpWorldSize = worldSize
                    }
            
            -- Store gen params for on-demand chunk loading
            writeIORef (wsGenParamsRef worldState) (Just params)

            -- Build zoom map cache (one-time computation)
            let zoomCache = buildZoomCache params
            writeIORef (wsZoomCacheRef worldState) zoomCache
            
            -- Generate the initial 5×5 chunk grid around (0,0)
            let initialCoords = [ ChunkCoord cx cy
                                | cx <- [-chunkLoadRadius .. chunkLoadRadius]
                                , cy <- [-chunkLoadRadius .. chunkLoadRadius]
                                ]
                initialChunks = map (\coord ->
                    let (chunkTiles, surfMap) = generateChunk params coord
                    in LoadedChunk
                        { lcCoord      = coord
                        , lcTiles      = chunkTiles
                        , lcSurfaceMap = surfMap
                        , lcModified   = False
                        }) initialCoords
            
            atomicModifyIORef' (wsTilesRef worldState) $ \_ ->
                (WorldTileData { wtdChunks = initialChunks }, ())
            
            atomicModifyIORef' (worldManagerRef env) $ \mgr ->
                (mgr { wmWorlds = (pageId, worldState) : wmWorlds mgr }, ())
            
            -- Set the camera z-slice to just above the surface at (0,0)
            let plates = generatePlates seed worldSize (wgpPlateCount params)
                (surfaceElev, _mat) = elevationAtGlobal seed plates worldSize 0 0
                startZSlice = surfaceElev + 3
            atomicModifyIORef' (cameraRef env) $ \cam ->
                (cam { camZSlice = startZSlice }, ())
            
            let totalTiles = sum $ map (HM.size . lcTiles) initialChunks
            logInfo logger CatWorld $ "World initialized: " 
                <> T.pack (show $ length initialChunks) <> " chunks, "
                <> T.pack (show totalTiles) <> " tiles, "
                <> "surface at z=" <> T.pack (show surfaceElev)
                <> ", zSlice set to " <> T.pack (show startZSlice)
                <> ": " <> unWorldPageId pageId
        
        WorldShow pageId -> do
            logDebug logger CatWorld $ "Showing world: " <> unWorldPageId pageId
            
            atomicModifyIORef' (worldManagerRef env) $ \mgr ->
                if pageId `elem` wmVisible mgr
                then (mgr, ())
                else (mgr { wmVisible = pageId : wmVisible mgr }, ())
            
            mgr <- readIORef (worldManagerRef env)
            logDebug logger CatWorld $ 
                "Visible worlds after show: " <> T.pack (show $ length $ wmVisible mgr)
        
        WorldHide pageId -> do
            logDebug logger CatWorld $ "Hiding world: " <> unWorldPageId pageId
            
            atomicModifyIORef' (worldManagerRef env) $ \mgr ->
                (mgr { wmVisible = filter (/= pageId) (wmVisible mgr) }, ())
        
        WorldTick dt -> do
            return ()
        
        WorldSetTexture pageId texType texHandle -> do
            logDebug logger CatWorld $ 
                "Setting texture for world: " <> unWorldPageId pageId 
                <> ", type: " <> T.pack (show texType)
                <> ", handle: " <> T.pack (show texHandle)
            
            mgr <- readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState -> do
                    let updateTextures wt = case texType of
                            GraniteTexture -> wt { wtGraniteTexture = texHandle }
                            DioriteTexture -> wt { wtDioriteTexture = texHandle }
                            GabbroTexture  -> wt { wtGabbroTexture  = texHandle }
                            GlacierTexture -> wt { wtGlacierTexture = texHandle }
                            BlankTexture   -> wt { wtBlankTexture   = texHandle }
                            NoTexture      -> wt { wtNoTexture      = texHandle }
                            IsoFaceMap     -> wt { wtIsoFaceMap     = texHandle }
                            NoFaceMap      -> wt { wtNoFaceMap      = texHandle }
                            ZoomGraniteTexture  -> wt { wtZoomGranite    = texHandle }
                            ZoomDioriteTexture  -> wt { wtZoomDiorite    = texHandle }
                            ZoomGabbroTexture   -> wt { wtZoomGabbro     = texHandle }
                            ZoomOceanTexture    -> wt { wtZoomOcean      = texHandle }
                            ZoomGlacierTexture  -> wt { wtZoomGlacier     = texHandle }
                    atomicModifyIORef' (wsTexturesRef worldState) 
                        (\wt -> (updateTextures wt, ()))
                    logDebug logger CatWorld $ 
                        "Texture updated for world: " <> unWorldPageId pageId
                Nothing -> 
                    logDebug logger CatWorld $ 
                        "World not found for texture update: " <> unWorldPageId pageId
        
        WorldSetCamera pageId x y -> do
            mgr <- readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState ->
                    atomicModifyIORef' (wsCameraRef worldState) $ \_ ->
                        (WorldCamera x y, ())
                Nothing -> 
                    logDebug logger CatWorld $ 
                        "World not found for camera update: " <> unWorldPageId pageId

        WorldSetTime pageId hour minute -> do
            logDebug logger CatWorld $
                "Setting time for world: " <> unWorldPageId pageId
                <> " to " <> T.pack (show hour) <> ":" <> T.pack (show minute)
            mgr <- readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState -> do
                    let clampedH = max 0 (min 23 hour)
                        clampedM = max 0 (min 59 minute)
                    atomicModifyIORef' (wsTimeRef worldState) $ \_ ->
                        (WorldTime clampedH clampedM, ())
                Nothing ->
                    logDebug logger CatWorld $
                        "World not found for time update: " <> unWorldPageId pageId

        WorldSetDate pageId year month day -> do
            logDebug logger CatWorld $
                "Setting date for world: " <> unWorldPageId pageId
                <> " to " <> T.pack (show year) <> "-"
                <> T.pack (show month) <> "-" <> T.pack (show day)
            mgr <- readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState ->
                    atomicModifyIORef' (wsDateRef worldState) $ \_ ->
                        (WorldDate year month day, ())
                Nothing ->
                    logDebug logger CatWorld $
                        "World not found for date update: " <> unWorldPageId pageId

        WorldSetTimeScale pageId scale -> do
            logDebug logger CatWorld $
                "Setting time scale for world: " <> unWorldPageId pageId
                <> " to " <> T.pack (show scale) <> " game-min/real-sec"
            mgr <- readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just worldState ->
                    writeIORef (wsTimeScaleRef worldState) scale
                Nothing ->
                    logDebug logger CatWorld $
                        "World not found for time scale update: " <> unWorldPageId pageId

unWorldPageId :: WorldPageId -> Text
unWorldPageId (WorldPageId t) = t

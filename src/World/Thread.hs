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
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Engine.Core.Queue as Q
import World.Types
import World.Generate

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
            tid <- forkIO $ worldLoop env stateRef
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

worldLoop :: EngineEnv -> IORef ThreadControl -> IO ()
worldLoop env stateRef = do
    control <- readIORef stateRef
    logger <- readIORef (loggerRef env)
    
    case control of
        ThreadStopped -> do
            logDebug logger CatWorld "World thread stopping..."
            pure ()
        ThreadPaused -> do
            threadDelay 100000
            worldLoop env stateRef
        ThreadRunning -> do
            -- Process all pending commands
            mCmd <- Q.tryReadQueue (worldQueue env)
            case mCmd of
                Just cmd -> handleWorldCommand env logger cmd
                Nothing -> return ()
            
            -- Check chunk loading for all visible worlds
            updateChunkLoading env logger
            
            threadDelay 16666
            worldLoop env stateRef

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
        
        -- All chunks that should be loaded
        neededCoords = [ ChunkCoord (ccx + dx) (ccy + dy)
                       | dx <- [-chunkLoadRadius .. chunkLoadRadius]
                       , dy <- [-chunkLoadRadius .. chunkLoadRadius]
                       ]
    
    -- Process each visible world
    forM_ (wmVisible manager) $ \pageId ->
        case lookup pageId (wmWorlds manager) of
            Nothing -> return ()
            Just worldState -> do
                mParams <- readIORef (wsGenParamsRef worldState)
                case mParams of
                    Nothing -> return ()  -- world not initialized yet
                    Just params -> do
                        tileData <- readIORef (wsTilesRef worldState)
                        
                        -- Clamp to world bounds
                        let halfSize = wgpWorldSize params `div` 2
                            inBounds (ChunkCoord cx cy) =
                                cx >= -halfSize && cx < halfSize &&
                                cy >= -halfSize && cy < halfSize
                            validCoords = filter inBounds neededCoords
                        
                        -- Find which chunks need loading vs promoting
                        let (toPromote, toGenerate) = partitionChunks validCoords tileData
                        
                        -- Only do work if there's something to generate
                        when (not $ null toGenerate) $ do
                            let newChunks = map (\coord -> LoadedChunk
                                    { lcCoord    = coord
                                    , lcTiles    = generateChunk params coord
                                    , lcModified = False
                                    }) toGenerate
                            
                            atomicModifyIORef' (wsTilesRef worldState) $ \td ->
                                let td' = foldl' (\acc lc -> insertChunk lc acc) td newChunks
                                    -- Promote all existing needed chunks to MRU
                                    td'' = foldl' (\acc coord -> promoteChunk coord acc) td' toPromote
                                in (td'', ())
                            
                            logDebug logger CatWorld $
                                "Loaded " <> T.pack (show $ length toGenerate)
                                <> " new chunks around " <> T.pack (show camChunk)
                                <> " (" <> T.pack (show $ chunkCount tileData + length toGenerate)
                                <> " total)"
                        
                        -- Promote existing chunks even when nothing new is generated
                        -- (keeps MRU order accurate for future unloading)
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
            
            -- Generate the initial 5×5 chunk grid around (0,0)
            let initialCoords = [ ChunkCoord cx cy
                                | cx <- [-chunkLoadRadius .. chunkLoadRadius]
                                , cy <- [-chunkLoadRadius .. chunkLoadRadius]
                                ]
                initialChunks = map (\coord -> LoadedChunk
                    { lcCoord    = coord
                    , lcTiles    = generateChunk params coord
                    , lcModified = False
                    }) initialCoords
            
            atomicModifyIORef' (wsTilesRef worldState) $ \_ ->
                (WorldTileData { wtdChunks = initialChunks }, ())
            
            atomicModifyIORef' (worldManagerRef env) $ \mgr ->
                (mgr { wmWorlds = (pageId, worldState) : wmWorlds mgr }, ())
            
            let totalTiles = sum $ map (HM.size . lcTiles) initialChunks
            logInfo logger CatWorld $ "World initialized: " 
                <> T.pack (show $ length initialChunks) <> " chunks, "
                <> T.pack (show totalTiles) <> " tiles: " 
                <> unWorldPageId pageId
        
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
                            GrassTexture -> wt { wtGrassTexture = texHandle }
                            GrassFaceMap -> wt { wtGrassFaceMap = texHandle }
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

unWorldPageId :: WorldPageId -> Text
unWorldPageId (WorldPageId t) = t

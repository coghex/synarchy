{-# LANGUAGE Strict #-}
module World.Thread
    ( startWorldThread
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef', newIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Exception (SomeException, catch)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import World.Types

-----------------------------------------------------------
-- Start World Thread
-----------------------------------------------------------

startWorldThread :: EngineEnv -> IO ThreadState
startWorldThread env = do
    logger <- readIORef (loggerRef env)
    stateRef <- newIORef ThreadRunning
    threadId <- catch
        (do
            logInfo logger CatSystem "Starting world thread..."
            tid <- forkIO $ worldLoop env stateRef
            logInfo logger CatSystem "World thread started"
            return tid
        )
        (\(e :: SomeException) -> do
            logError logger CatSystem $ "Failed starting world thread: " <> T.pack (show e)
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
            logDebug logger CatSystem "World thread stopping..."
            pure ()
        ThreadPaused -> do
            threadDelay 100000  -- 100ms pause check
            worldLoop env stateRef
        ThreadRunning -> do
            -- Try to read a command (non-blocking)
            mCmd <- Q.tryReadQueue (worldQueue env)
            case mCmd of
                Just cmd -> handleWorldCommand env logger cmd
                Nothing -> return ()
            
            threadDelay 16666  -- ~60 FPS
            worldLoop env stateRef

-----------------------------------------------------------
-- Command Handler
-----------------------------------------------------------

handleWorldCommand :: EngineEnv -> LoggerState -> WorldCommand -> IO ()
handleWorldCommand env logger cmd = do
    logDebug logger CatSystem $ "Processing world command: " <> T.pack (show cmd)
    
    case cmd of
        WorldInit pageId -> do
            logDebug logger CatSystem $ "Initializing world: " <> unWorldPageId pageId
            
            -- Create empty world state
            worldState <- emptyWorldState
            
            -- Add one grass tile at (0,0) for testing
            atomicModifyIORef' (wsTilesRef worldState) $ \tileData ->
                let newTiles = HM.insert (0, 0, 0) (Tile 1)
                             $ HM.insert (1, 0, 0) (Tile 1)
                             $ HM.insert (0, 1, 0) (Tile 1)
                             $ HM.insert (0, 0, 1) (Tile 1)
                             $ (wtdTiles tileData)
                in (tileData { wtdTiles = newTiles }, ())
            
            -- Add to world manager
            atomicModifyIORef' (worldManagerRef env) $ \manager ->
                (manager { wmWorlds = (pageId, worldState) : wmWorlds manager }, ())
            
            logInfo logger CatSystem $ "World initialized: " <> unWorldPageId pageId
        
        WorldShow pageId -> do
            logDebug logger CatSystem $ "Showing world: " <> unWorldPageId pageId
            
            atomicModifyIORef' (worldManagerRef env) $ \manager ->
                if pageId `elem` wmVisible manager
                then (manager, ())
                else (manager { wmVisible = pageId : wmVisible manager }, ())
            
            -- Log current state
            manager <- readIORef (worldManagerRef env)
            logDebug logger CatSystem $ 
                "Visible worlds after show: " <> T.pack (show $ length $ wmVisible manager)
        
        WorldHide pageId -> do
            logDebug logger CatSystem $ "Hiding world: " <> unWorldPageId pageId
            
            atomicModifyIORef' (worldManagerRef env) $ \manager ->
                (manager { wmVisible = filter (/= pageId) (wmVisible manager) }, ())
        
        WorldTick dt -> do
            -- Future: world simulation tick
            return ()
        
        WorldSetTexture pageId texType texHandle -> do
            logDebug logger CatSystem $ 
                "Setting texture for world: " <> unWorldPageId pageId 
                <> ", type: " <> T.pack (show texType)
                <> ", handle: " <> T.pack (show texHandle)
            
            -- Find the world and update its textures
            manager <- readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds manager) of
                Just worldState -> do
                    let updateTextures wt = case texType of
                            GrassTexture -> wt { wtGrassTexture = texHandle }
                    atomicModifyIORef' (wsTexturesRef worldState) 
                        (\wt -> (updateTextures wt, ()))
                    logDebug logger CatSystem $ 
                        "Texture updated for world: " <> unWorldPageId pageId
                Nothing -> 
                    logDebug logger CatSystem $ 
                        "World not found for texture update: " <> unWorldPageId pageId

unWorldPageId :: WorldPageId -> Text
unWorldPageId (WorldPageId t) = t

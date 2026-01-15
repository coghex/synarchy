{-# LANGUAGE Strict #-}
module Engine.Lua.Thread
  ( startLuaThread
  , runLuaLoop
  , executeLuaScripts
  , shutdownLuaThread
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List (sortBy)
import qualified Scripting.Lua as Lua
import qualified Engine.Core.Queue as Q
import Engine.Lua.Types
import Engine.Lua.Base
import Engine.Core.Thread
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar')
import Control.Exception (catch, SomeException)
import Data.IORef (newIORef, writeIORef)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Start the centralized Lua execution thread
startLuaThread ∷ Q.Queue T.Text → LuaEnv → IO ThreadState
startLuaThread logQueue luaEnv = catch
  (do
    logLuaInfo logQueue "Starting Lua execution thread..."
    stateRef ← newIORef ThreadRunning
    threadId ← forkIO $ runLuaLoop logQueue luaEnv ThreadRunning
    logLuaInfo logQueue "Lua thread started successfully"
    return $ ThreadState stateRef threadId
  )
  (\(e ∷ SomeException) → do
    logLuaError logQueue $ LuaStateError $ T.pack $
      "Failed to start Lua thread: " ⧺ show e
    error "Lua thread failed to start"
  )

-- | Main Lua execution loop with deterministic tick-based scheduling
runLuaLoop ∷ Q.Queue T.Text → LuaEnv → ThreadControl → IO ()
runLuaLoop _        _      ThreadStopped = return ()
runLuaLoop logQueue luaEnv ThreadPaused  = do
  threadDelay 100000  -- 100ms pause check
  runLuaLoop logQueue luaEnv ThreadRunning
runLuaLoop logQueue luaEnv ThreadRunning = do
  -- Start frame timing
  frameStart ← getCurrentTime
  
  -- Get current tick
  currentTick ← atomically $ readTVar (leCurrentTick luaEnv)
  
  -- Execute all scripts that should run on this tick
  executeLuaScripts logQueue luaEnv currentTick
  
  -- Increment tick counter
  atomically $ modifyTVar' (leCurrentTick luaEnv) (+1)
  
  -- Calculate frame time and delay to maintain consistent rate
  frameEnd ← getCurrentTime
  let diff = diffUTCTime frameEnd frameStart
      usecs = floor (toRational diff * 1000000) ∷ Int
      targetFrameTime = 16666  -- ~60 FPS (16.666ms per frame)
      delay = targetFrameTime - usecs
  
  -- Only delay if we're running faster than target
  when (delay > 0) $ threadDelay delay
  
  -- Continue loop
  runLuaLoop logQueue luaEnv ThreadRunning

-- | Execute all Lua scripts that should run on the current tick
executeLuaScripts ∷ Q.Queue T.Text → LuaEnv → Word64 → IO ()
executeLuaScripts logQueue luaEnv currentTick = do
  scripts ← atomically $ readTVar (leScripts luaEnv)
  let luaState = leLuaState luaEnv
  
  -- Get scripts that should execute on this tick, sorted by name for determinism
  let shouldRun script = lsEnabled script ∧
                        lsInitialized script ∧
                        (lsTickInterval script ≡ 0 ∨
                         currentTick `mod` lsTickInterval script ≡ 0)
      scriptsToRun = Map.elems $ Map.filter shouldRun scripts
      -- Sort by name for deterministic execution order
      sortedScripts = sortBy (\a b → compare (lsName a) (lsName b)) scriptsToRun
  
  -- Execute each script in deterministic order (sorted by name)
  forM_ sortedScripts $ \script → catch
    (do
      -- Load the script file if needed
      Lua.getglobal luaState "runLua"
      isFunc ← Lua.isfunction luaState (-1)
      
      if isFunc
        then do
          -- Push the current tick as argument
          Lua.pushinteger luaState (fromIntegral currentTick)
          
          -- Call runLua with the tick argument
          result ← Lua.pcall luaState 1 0 0
          
          when (result ≢ 0) $ do
            errMsg ← Lua.tostring luaState (-1)
            logLuaError logQueue $ LuaExecutionError
              (T.pack $ fromMaybe "Unknown error" errMsg)
              (lsFilePath script)
            Lua.pop luaState 1
        else do
          -- runLua not found, reload the script
          Lua.pop luaState 1
          reloadResult ← Lua.loadfile luaState (lsFilePath script)
          
          when (reloadResult ≡ 0) $ do
            pcallResult ← Lua.pcall luaState 0 0 0
            
            when (pcallResult ≢ 0) $ do
              errMsg ← Lua.tostring luaState (-1)
              logLuaError logQueue $ LuaExecutionError
                (T.pack $ fromMaybe "Unknown error" errMsg)
                (lsFilePath script)
              Lua.pop luaState 1
      
      -- Update next tick for this script (only meaningful for non-zero intervals)
      let nextTick = if lsTickInterval script ≡ 0
                     then currentTick + 1
                     else currentTick + lsTickInterval script
          updatedScript = script { lsNextTick = nextTick }
      updateScript (leScripts luaEnv) updatedScript
    )
    (\(e ∷ SomeException) → do
      logLuaError logQueue $ LuaExecutionError
        (T.pack $ "Exception during script execution: " ⧺ show e)
        (lsFilePath script)
    )

-- | Safely shutdown the Lua thread
shutdownLuaThread ∷ Q.Queue T.Text → LuaEnv → ThreadState → IO ()
shutdownLuaThread logQueue luaEnv ts = do
  -- Signal thread to stop
  writeIORef (tsRunning ts) ThreadStopped
  logLuaInfo logQueue "Lua thread signaled to stop..."
  
  -- Wait for thread to complete
  waitThreadComplete ts
  
  -- Close the Lua state
  Lua.close (leLuaState luaEnv)
  
  logLuaInfo logQueue "Lua thread shutdown complete."

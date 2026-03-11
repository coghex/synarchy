module Test.Headless.Harness
  ( withHeadlessEngine
  , sendWorldCommand
  , waitForWorldInit
  , getWorldState
  , getWorldTileData
  , getWorldGenParams
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Thread (shutdownThread, ThreadState)
import qualified Engine.Core.Queue as Q
import World.Thread (startWorldThread)
import World.Types

-- | Boot engine in headless mode, run action, shut down.
withHeadlessEngine ∷ (EngineEnv → IO α) → IO α
withHeadlessEngine action = bracket setup teardown (\(env, _) → action env)
  where
    setup = do
        EngineInitResult env _ _ ← initializeEngineHeadless
        writeIORef (lifecycleRef env) EngineRunning
        worldTS ← startWorldThread env
        pure (env, [worldTS])
    teardown (env, threads) = do
        writeIORef (lifecycleRef env) CleaningUp
        mapM_ shutdownThread threads
        threadDelay 100000

-- | Send a command to the world thread
sendWorldCommand ∷ EngineEnv → WorldCommand → IO ()
sendWorldCommand env cmd = Q.writeQueue (worldQueue env) cmd

-- | Wait for world generation to complete (LoadDone).
--   Returns the WorldState. Fails if timeout (in seconds) is exceeded.
waitForWorldInit ∷ EngineEnv → WorldPageId → Int → IO WorldState
waitForWorldInit env pageId timeoutSecs = go 0
  where
    pollIntervalMs = 100000  -- 100ms
    maxPolls = timeoutSecs * 10
    go n
      | n ≥ maxPolls = error $ "waitForWorldInit: timed out after "
                             ⧺ show timeoutSecs ⧺ "s waiting for "
                             ⧺ show pageId
      | otherwise = do
          mWs ← getWorldState env pageId
          case mWs of
              Nothing → do
                  threadDelay pollIntervalMs
                  go (n + 1)
              Just ws → do
                  phase ← readIORef (wsLoadPhaseRef ws)
                  case phase of
                      LoadDone → pure ws
                      _        → do
                          threadDelay pollIntervalMs
                          go (n + 1)

-- | Look up a WorldState by page ID
getWorldState ∷ EngineEnv → WorldPageId → IO (Maybe WorldState)
getWorldState env pageId = do
    wm ← readIORef (worldManagerRef env)
    pure $ lookup pageId (wmWorlds wm)

-- | Read tile data from a world state
getWorldTileData ∷ WorldState → IO WorldTileData
getWorldTileData ws = readIORef (wsTilesRef ws)

-- | Read generation params from a world state
getWorldGenParams ∷ WorldState → IO (Maybe WorldGenParams)
getWorldGenParams ws = readIORef (wsGenParamsRef ws)

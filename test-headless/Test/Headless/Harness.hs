module Test.Headless.Harness
  ( withHeadlessEngine
  , sharedWorld
  , sendWorldCommand
  , waitForWorldInit
  , getWorldState
  , getWorldTileData
  , getWorldGenParams
  , moveCamera
  , waitForChunksAt
  , queueChunks
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (readIORef, writeIORef, modifyIORef', atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Engine.Core.Queue as Q
import World.Thread (startWorldThread)
import World.Types

-- | Boot engine in headless mode, run action, shut down.
--   Sets camera zoom low so updateChunkLoading will trigger.
withHeadlessEngine ∷ (EngineEnv → IO α) → IO α
withHeadlessEngine action = bracket setup teardown (\(env, _) → action env)
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadless
        -- Set zoom low enough for chunk loading (needs < zoomFadeEnd + 0.5 = 2.1)
        modifyIORef' (cameraRef env) $ \cam → cam { camZoom = 0.5 }
        writeIORef (lifecycleRef env) EngineRunning
        worldTS ← startWorldThread env
        pure (env, [worldTS])
    teardown (env, threads) = do
        writeIORef (lifecycleRef env) CleaningUp
        mapM_ shutdownThread threads
        threadDelay 100000

-- | Get (or lazily create) a world keyed by its generation params.
--
--   World generation is the entire cost of this suite (~8–12 s per
--   w64 init; everything else is milliseconds), and most specs only
--   READ the world — so specs that can use the same (seed, size,
--   plateCount) share one generation instead of paying for their
--   own. The engine's world manager is the cache; the page id is
--   derived from the params so every caller converges on the same
--   page. Works because Spec.hs boots ONE engine for all worldgen
--   specs (a single top-level 'aroundAll withHeadlessEngine') and
--   hspec runs items sequentially.
--
--   Rules for using a shared world:
--     * read-only specs: share freely.
--     * specs that mutate the page (destroy, edits) or need a world
--       nobody else touches: do a private 'WorldInit' with a unique
--       page id instead (see the destroy test).
--     * queueing EXTRA chunks (Exposure) is fine — later readers
--       just see more chunks.
sharedWorld ∷ EngineEnv → Word64 → Int → Int → IO WorldState
sharedWorld env seed size plateCount = do
    let pid = WorldPageId $ T.pack $
            "shared_" ⧺ show seed ⧺ "_" ⧺ show size ⧺ "_" ⧺ show plateCount
    mWs ← getWorldState env pid
    case mWs of
        Just _ → waitForWorldInit env pid 300
        Nothing → do
            sendWorldCommand env (WorldInit pid seed size plateCount Nothing)
            waitForWorldInit env pid 300

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

-- | Move camera to a global tile coordinate. Converts to world position
--   assuming FaceSouth facing and sets zoom low for chunk loading.
moveCamera ∷ EngineEnv → Float → Float → IO ()
moveCamera env gx gy =
    modifyIORef' (cameraRef env) $ \cam →
        cam { camPosition = (gx, gy), camZoom = 0.5 }

-- | Wait until a chunk at the given chunk coordinate is loaded.
--   Polls for up to timeoutSecs.
waitForChunksAt ∷ WorldState → ChunkCoord → Int → IO Bool
waitForChunksAt ws coord timeoutSecs = go 0
  where
    maxPolls = timeoutSecs * 20  -- 50ms intervals
    go n
      | n ≥ maxPolls = pure False
      | otherwise = do
          td ← readIORef (wsTilesRef ws)
          if HM.member coord (wtdChunks td)
              then pure True
              else do
                  threadDelay 50000
                  go (n + 1)

-- | Queue chunk coords for generation by the world thread (the same
--   path Lua's @world.loadChunksInRegion@ uses). Pair with
--   'waitForChunksAt' on the last coord to block until generated.
queueChunks ∷ WorldState → [ChunkCoord] → IO ()
queueChunks ws coords = do
    td ← readIORef (wsTilesRef ws)
    let needed = filter (\c → not (HM.member c (wtdChunks td))) coords
    atomicModifyIORef' (wsInitQueueRef ws) $ \q → (q ⧺ needed, ())

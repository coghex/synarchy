module Test.Headless.Harness
  ( withHeadlessEngine
  , withHeadlessEngineExpectingStopped
  , withHeadlessEngineNoWorld
  , HeadlessWorker(..)
  , worldWorker
  , headlessWorkerLabel
  , checkHeadlessWorkers
  , withHeadlessWorkerCheck
  , installHudWorldPage
  , sharedWorld
  , sharedWorldPageId
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
import Control.Concurrent.MVar (isEmptyMVar)
import Control.Exception (bracket)
import Control.Monad (filterM)
import Data.List (intercalate)
import Data.IORef (readIORef, writeIORef, modifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Thread (ThreadState(..), shutdownThread)
import Engine.Graphics.Camera (Camera2D(..))
import Test.Hspec (expectationFailure)
import qualified Engine.Core.Queue as Q
import World.Chunk.Queue (enqueueChunkRequest)
import World.Thread (startWorldThread)
import World.Types

-- | A worker the headless harness started, identified by name.
--
--   Named rather than positional because both halves of the health
--   check have to talk about ONE worker: a failure says which worker
--   exited, and the opt-out exempts one worker without silencing the
--   rest. Call sites use the constants below rather than spelling a
--   name inline; a second worker gets its own constant here and an
--   entry in @setup@, and is then covered automatically.
newtype HeadlessWorker = HeadlessWorker String
    deriving (Show, Eq)

-- | The world worker 'withHeadlessEngine' starts. The only one today —
--   covering the unit, combat, sim and Lua workers belongs with
--   whatever harness starts them.
worldWorker ∷ HeadlessWorker
worldWorker = HeadlessWorker "world"

-- | The name a worker is reported and opted out by.
headlessWorkerLabel ∷ HeadlessWorker → String
headlessWorkerLabel (HeadlessWorker name) = name

-- | Fail the example for every started worker that has already exited
--   (#1388).
--
--   The signal is the worker's own @tsDone@. 'Engine.Core.Thread'
--   documents it as filled exactly once when the loop actually exits,
--   from a @finally@ at the fork site
--   ('World.Thread.startWorldThread'), so a FILLED @tsDone@ observed
--   before teardown means the worker stopped on its own — a fail-stop.
--   A worker that fail-stops catches its own exception, logs it, and
--   returns without rethrowing, so nothing reaches the hspec example
--   and every assertion that does not touch the worker keeps passing.
--
--   Deliberately NOT keyed on 'EngineLifecycle'. @CleaningUp@ has many
--   writers — every worker's crash handler, the input worker, the debug
--   console, @engine.quit()@'s Lua handler, the normal loop shutdown
--   and dump shutdown — so a lifecycle-keyed check would fail any test
--   that legitimately quits the engine and would fire for workers this
--   harness never started. @tsDone@ is per-thread and means one thing.
--   This function takes the workers and NOTHING else, so it cannot
--   consult the lifecycle even by accident; 'withHeadlessEngine' still
--   uses @lifecycleRef@ for ordinary setup and teardown, which is a
--   separate concern.
--
--   The probe is 'isEmptyMVar': non-blocking (the healthy case is an
--   empty @MVar@ and never waits) and non-consuming. 'shutdownThread'
--   joins on @tsDone@ with a non-consuming @readMVar@ (#2165), so a
--   consuming probe here would make failure-path teardown sit out its
--   full graceful timeout and then kill a thread that had already
--   exited.
--
--   @expectedStopped@ names workers this caller stopped on purpose;
--   every other worker is still checked strictly.
checkHeadlessWorkers ∷ [HeadlessWorker] → [(HeadlessWorker, ThreadState)]
                     → IO ()
checkHeadlessWorkers expectedStopped workers = do
    dead ← filterM hasExited
             [ w | w@(name, _) ← workers
                 , name `notElem` expectedStopped ]
    unless (null dead) $
        expectationFailure $ headlessWorkerReport (map fst dead)
  where
    hasExited (_, ts) = not ⊚ isEmptyMVar (tsDone ts)

-- | The diagnostic for one or more exited workers. @tsDone@ records
--   THAT a thread ended, not why, so the report points at the crash
--   line the worker itself logged (requirement 2 of #1388).
headlessWorkerReport ∷ [HeadlessWorker] → String
headlessWorkerReport dead = unlines
    [ "headless harness: worker exited before teardown: " ⧺ names
    , ""
    , "A worker that ends on its own has fail-stopped: it catches its"
    , "own exception, logs it and stops its loop without rethrowing, so"
    , "hspec never saw a failure and every assertion that does not touch"
    , "that worker kept passing."
    , ""
    , "tsDone records THAT the thread ended, not why. For the cause,"
    , "look in this run's captured output for the worker's own crash"
    , "line — the world worker logs \"World thread crashed: ...\"."
    , ""
    , "If this spec stops the worker deliberately, say so narrowly:"
    , "  withHeadlessEngineExpectingStopped [" ⧺ optOut ⧺ "] $ \\env → ..."
    ]
  where
    names  = intercalate ", " (map headlessWorkerLabel dead)
    optOut = intercalate ", " (map show dead)

-- | Run @body@, then assert every started worker is still alive.
--
--   The action's own exception wins: when @body@ throws, the health
--   check never runs, so a dead worker can never mask a real assertion
--   failure (requirement 4 of #1388). Teardown is the caller's
--   'bracket' and still runs on every path.
--
--   Under @aroundAll@ that precedence holds at the GROUP level too, and
--   hspec is what enforces it: a wrapper failure raised after the last
--   item is merged into that item's result, and @mergeResults@ keeps an
--   already-failing item's own failure. So a group that was already red
--   stays red for its own reason, and a group that was GREEN — the
--   false-green this check exists for — turns red here.
withHeadlessWorkerCheck ∷ [HeadlessWorker] → [(HeadlessWorker, ThreadState)]
                        → IO α → IO α
withHeadlessWorkerCheck expectedStopped workers body = do
    result ← body
    checkHeadlessWorkers expectedStopped workers
    pure result

-- | The page id @scripts\/hud.lua@ defaults @hud.worldId@ to
--   (@scripts\/hud.lua:25@). Stated once, here, because the fixture
--   helper below only works while it matches.
hudDefaultWorldPageId ∷ WorldPageId
hudDefaultWorldPageId = WorldPageId "main_world"

-- | Make 'hudDefaultWorldPageId' resolve, by installing one bare
--   in-memory page under it as the manager's only entry (#1366).
--
--   @hud.createUI()@ submits six cursor-texture commands
--   (@scripts\/hud.lua:389-398@) against @hud.worldId@ whenever its
--   texture handles are present — which they always are in a fixture,
--   because @hud.init@ is handed synthetic ones. A fixture that boots
--   the HUD without a world therefore drives
--   "World.Thread.Command.Cursor.Select" down its missing-page branch
--   six times per boot, and each one logs a warning. That is correct
--   production behaviour on a page that genuinely is not there
--   (@Test.Headless.World.CursorTextureDispatch@ pins it); the fixture
--   is what is wrong, since a real session has @main_world@ live before
--   @hud.createUI()@ ever runs. 158 HUD boots across the three
--   HUD-booting suites were emitting 948 of the headless step's 988
--   @WARN@ lines — 96 % of them, which is what a real diagnostic had to
--   be found among, and one of which landed mid-line inside an example's
--   own result row.
--
--   Two deliberate properties, both load-bearing:
--
--   * __No generation parameters.__ 'emptyWorldState' leaves
--     @wsGenParamsRef@ 'Nothing', and both world-thread page walks
--     ('World.Thread.ChunkLoading.updateChunkLoading',
--     'World.Thread.ChunkLoading.drainInitQueues') and
--     'World.Thread.Discovery.tickLocationDiscovery' skip such a page
--     outright. Handing a fixture page 'defaultWorldGenParams' instead
--     is exactly what killed the world worker with
--     @twoNearestPlates: no plates@ (#1362) — trading a warning flood
--     for a dead thread is not a repair. Since #1388 that is no longer
--     only a convention: 'checkHeadlessWorkers' fails every example in
--     these three suites if this page ever does kill the worker.
--
--   * __Not visible.__ The cursor-texture handlers resolve their page
--     through @wmWorlds@ alone, so visibility buys nothing here, while
--     an entry in @wmVisible@ would newly give these UI suites an
--     ACTIVE page (@world.getActiveWorldId@, chunk loading, the sun
--     angle). These are layout and widget-behaviour tests: the only
--     thing this may change about them is that the six commands now
--     find their page.
installHudWorldPage ∷ EngineEnv → IO ()
installHudWorldPage env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(hudDefaultWorldPageId, ws)]
        , wmVisible = [] }

-- | Boot engine in headless mode, run action, shut down.
--   Sets camera zoom low so updateChunkLoading will trigger.
--
--   After the action returns, every worker this harness started must
--   still be running; one that already exited fails the example
--   ('checkHeadlessWorkers'). A spec that stops a worker on purpose
--   uses 'withHeadlessEngineExpectingStopped' to exempt exactly that
--   worker.
withHeadlessEngine ∷ (EngineEnv → IO α) → IO α
withHeadlessEngine = withHeadlessEngineExpectingStopped []

-- | 'withHeadlessEngine', exempting the named workers from the
--   post-action health check.
--
--   The opt-out is narrow by construction: it names workers, so the
--   remaining ones stay strictly checked, and it cannot turn the check
--   off globally. It also does not touch exception precedence — an
--   action that throws still surfaces its own failure. Nothing in the
--   suite needs this today (no spec inside a 'withHeadlessEngine'
--   wrapper stops a worker), so it exists for a future spec that
--   deliberately shuts one down mid-example.
withHeadlessEngineExpectingStopped ∷ [HeadlessWorker] → (EngineEnv → IO α)
                                   → IO α
withHeadlessEngineExpectingStopped expectedStopped action =
    bracket setup teardown $ \(env, workers) →
        withHeadlessWorkerCheck expectedStopped workers (action env)
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        -- Set zoom low enough for chunk loading (needs < zoomFadeEnd + 0.5 = 2.1)
        modifyIORef' (cameraRef env) $ \cam → cam { camZoom = 0.5 }
        writeIORef (lifecycleRef env) EngineRunning
        worldTS ← startWorldThread env
        pure (env, [(worldWorker, worldTS)])
    -- No settling delay after 'shutdownThread' (#1363).
    --
    -- 'shutdownThread' is a join, not a signal: on the normal path it
    -- blocks on @readMVar (tsDone ts)@, and 'World.Thread.startWorldThread'
    -- fills that @MVar@ from a @finally@ at the fork site. So the worker's
    -- loop has provably exited by the time this returns, and the harness
    -- starts no other thread — @initializeEngineHeadlessQuiet@ binds no socket
    -- and starts no debug server. A fixed sleep here waited for nothing
    -- and cost 100 ms per engine, ~27 s across the suite's 270 boots.
    --
    -- @shutdownThread@'s exceptional path joins too (#2165): after the
    -- 10 s graceful timeout it force-kills, waits again, bounded, and
    -- reports a worker that still has not exited as fatal rather than
    -- returning. There is no path on which a sleep here would have
    -- stood in for that join.
    --
    -- If a spec ever needs settling time, it waits in that spec for the
    -- condition it actually needs. A blanket delay charged to every
    -- engine is what #1363 removed and must not come back here.
    teardown (env, workers) = do
        writeIORef (lifecycleRef env) CleaningUp
        mapM_ (shutdownThread ∘ snd) workers

-- | Boot engine in headless mode with NO world thread, run action, shut
--   down (#1362).
--
--   'withHeadlessEngine' starts a real world worker, which is what a
--   spec that sends a world command or generates a page needs. A spec
--   that only installs in-memory 'World.State.Types.emptyWorldState'
--   pages and reads them straight back needs no worker — and paying for
--   one is not free. The worker's chunk loading picks up every VISIBLE
--   page carrying generation parameters and generates against them, so
--   a fixture whose params come from 'defaultWorldGenParams' (seed 42,
--   worldSize 128, plateCount 10, and an EMPTY @wgpPlates@) drives
--   'World.Plate.Query.twoNearestPlates' into its own @error@.
--   'World.Thread' catches that, logs @World thread crashed@, writes
--   'CleaningUp' and stops the loop without rethrowing — so every later
--   example runs against a dead worker and a cleaning-up engine while
--   hspec still reports green.
--
--   Booting 'initializeEngineHeadlessQuiet' with no thread at all is the
--   in-tree idiom for that shape: 'Test.Headless.Unit.LineOfSight' and
--   'Test.Headless.Core.LoopStartup' both do it directly. This is the
--   same thing as an 'aroundAll'-shaped wrapper, so a spec written
--   against 'withHeadlessEngine' switches by changing only which
--   wrapper 'Spec.hs' names.
--
--   Camera zoom is deliberately left alone: 'withHeadlessEngine' lowers
--   it only so 'updateChunkLoading' triggers, and there is no worker
--   here to trigger.
withHeadlessEngineNoWorld ∷ (EngineEnv → IO α) → IO α
withHeadlessEngineNoWorld = bracket setup teardown
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        writeIORef (lifecycleRef env) EngineRunning
        pure env
    teardown env = writeIORef (lifecycleRef env) CleaningUp

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
    let pid = sharedWorldPageId seed size plateCount
    mWs ← getWorldState env pid
    case mWs of
        Just _ → waitForWorldInit env pid 300
        Nothing → do
            sendWorldCommand env (WorldInit pid seed size plateCount Nothing)
            waitForWorldInit env pid 300

-- | The page id 'sharedWorld' registers a world under. Exposed because
--   a chunk request is page-qualified (#2001), so a caller that queues
--   extra chunks into a shared world has to name it.
sharedWorldPageId ∷ Word64 → Int → Int → WorldPageId
sharedWorldPageId seed size plateCount = WorldPageId $ T.pack $
    "shared_" ⧺ show seed ⧺ "_" ⧺ show size ⧺ "_" ⧺ show plateCount

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

-- | Queue chunk coords for generation by the world thread — literally
--   the call Lua's @world.loadChunksInRegion@ makes, so the harness
--   registers demand on the page's residency owner exactly as production
--   does (#2001) instead of appending behind its back. Pair with
--   'waitForChunksAt' on the last coord to block until generated.
queueChunks ∷ WorldPageId → WorldState → [ChunkCoord] → IO ()
queueChunks pid ws coords = void (enqueueChunkRequest pid ws coords)

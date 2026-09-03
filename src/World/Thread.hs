{-# LANGUAGE Strict #-}
module World.Thread
    ( startWorldThread
    , worldTickWith
    , partitionAuthorized
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, writeIORef, newIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay)
import Data.List (partition)
import Engine.Core.Clock (monotonicSeconds, sampleElapsed)
import Engine.Core.Thread
    (ThreadState, WorkerFailLevel(..), WorkerSpec(..), noRefusal
    , startWorkerThread, workerCrashStderrSink)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.SaveLoad
    (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, logError, logWarn, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import World.Render (updateWorldTiles)
import World.Thread.Cursor (pollCursorInfo)
import World.Thread.Time (tickWorldTime)
import World.Thread.ChunkLoading (updateChunkLoading, drainInitQueues)
import World.Thread.Command (handleWorldCommand)
import World.Command.Types (WorldCommand(..))
import World.State.Types (settleSelectionProjection)
import World.Types (WorldManager(..))
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, ownerGated)

-- * Start World Thread

startWorldThread ∷ EngineEnv → IO ThreadState
startWorldThread env = startWorkerThread WorkerSpec
    { wsName        = "World"
    , wsLoggerRef   = ccLoggerRef (toCoreCapability env)
    , wsCategory    = CatWorld
    , wsLifecycleRef = ccLifecycleRef (toCoreCapability env)
    , wsCrashSink   = workerCrashStderrSink
    , wsStartingMsg = "Starting world thread..."
    , wsStartedMsg  = Just "World thread started"
    , wsFailMsg     = "Failed starting world thread: "
    , wsFailLevel   = WorkerFailError
    , wsFailFatal   = "World thread start failure."
    , wsStartup     = \_ → noRefusal (monotonicSeconds ⌦ newIORef)
    , wsTick        = worldTickWith monotonicSeconds env
    , wsOnStop      = \_ → do
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        logDebug logger CatWorld "World thread stopping..."
    , wsOnCrash     = \_ e → do
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        logError logger CatWorld $ "World thread crashed: " <> tshow e
      -- The lifecycle write this line used to precede belongs to the
      -- shared loop now, ahead of the log (#2283).
    , wsOnCrashCleanup = \_ _ → pure ()
    }

-- * World Tick

-- | One world tick against an injectable clock (#2204). Production
--   passes 'monotonicSeconds' (see 'startWorldThread'); the headless
--   gate scripts a jump. The elapsed value handed to 'tickWorldTime' is
--   the SANITISED one from "Engine.Core.Clock" — never negative, never
--   above the shared cap — and @lastTimeRef@ is replaced with the raw
--   sample every tick, so an over-cap difference is dropped rather than
--   carried into the next tick.
worldTickWith ∷ IO Double → EngineEnv → IORef Double → IO (Maybe (IORef Double))
worldTickWith clock env lastTimeRef = do
    logger ← readIORef (ccLoggerRef (toCoreCapability env))
    dt ← sampleElapsed clock lastTimeRef

    -- #2221: 'ownerGated', not 'captureLocked'. From this owner's own
    -- final-pass acknowledgement the tick takes the authorized-command
    -- branch below, so 'drainInitQueues'/'updateChunkLoading' can no
    -- longer enqueue fresh unit/building/sim work after every owner has
    -- already drained its three passes. The world owner's standing
    -- exception is unchanged and is exactly what this branch is: it
    -- keeps consuming the authorized WorldSave / WorldLoadPublish
    -- commands, and defers (save) or discards (load publish) the rest.
    locked ← ownerGated (slSaveBarrierRef (toSaveLoadCapability env)) SaveWorld
    if locked
        then processAuthorizedSave env logger
        else do
            processAllCommands env logger
            -- Acknowledging BEFORE the rest
            -- of this tick's work (drainInitQueues/
            -- tickWorldTime/updateChunkLoading/pollCursorInfo,
            -- all of which can queue fresh Lua/HUD/sim
            -- messages) let this ack be the FINAL one a
            -- quiescence pass needed while this tick was
            -- still mid-flight producing more side effects —
            -- if the barrier then reached SaveSnapshotBoundary
            -- before this tick finished, that later work
            -- could straddle the publish boundary and land
            -- against the replacement session. Folded into
            -- this branch (all already unconditional on
            -- "not locked", since this whole branch only
            -- runs when locked is False) so the ack fires
            -- only once every side-effect-producing step
            -- below has actually completed.
            drainInitQueues env logger
            tickWorldTime env (realToFrac dt)
            updateChunkLoading env logger
            pollCursorInfo env
            acknowledgeCurrent (slSaveBarrierRef (toSaveLoadCapability env))
                               SaveWorld

    _camera ← readIORef (rvCameraRef (toRenderViewCapability env))
    allQuads ← updateWorldTiles env
    -- Plain writeIORef is fine here: the value is an immutable
    -- LayeredQuads built entirely before the write, so the
    -- reader (Frame.hs) always sees either the old or the new
    -- value, never a torn pointer — at worst it draws one
    -- frame against the previous quads.
    writeIORef (rhWorldQuadsRef (toRenderHandoffCapability env)) allQuads
    threadDelay 16666
    pure (Just lastTimeRef)

-- | Drain all pending commands from the queue, except while an
--   Exit-to-Menu teardown is still outstanding, in which case the drain
--   stops at the first command that would register a page (#2291).
--
--   The teardown spans two threads: the destroy-all handler runs here,
--   but the clears it queues and the epoch reset that follows them run
--   on the unit thread ('Unit.Thread.endSessionEpoch'). The queue
--   markers order that reset against the unit and building queues and
--   say nothing about THIS thread — which is where pages are registered
--   and, since #1602, where a bound building placement is committed
--   directly rather than through the building queue. A @world.init@
--   drained inside that window would give the next session a page while
--   the outgoing one's clock is still live, so a placement onto it would
--   record @biSpawnedAt@ from a clock that is about to move backwards.
--
--   So a page-registering command is left QUEUED rather than run, with
--   everything behind it, and the drain resumes on a later tick.
--   Nothing else is fenced and nothing is discarded: an outstanding
--   teardown lasts one unit tick, and the flag is cleared by that reset
--   itself ('World.State.Types.wmSessionTeardown').
processAllCommands ∷ EngineEnv → LoggerState → IO ()
processAllCommands env logger = do
    mCmd ← Q.tryReadQueue queue
    case mCmd of
        Nothing  → return ()
        Just cmd → do
            fenced ← if beginsSession cmd
                        then wmSessionTeardown
                                 <$> readIORef (wsWorldManagerRef worldSim)
                        else pure False
            if fenced
            then requeueAhead queue cmd
            else do
                handleWorldCommand env logger cmd
                settleSelection env
                processAllCommands env logger
  where
    worldSim = toWorldSimCapability env
    queue    = wsWorldQueue worldSim

-- | The commands that put a page into 'World.State.Types.wmWorlds', and
--   so bring the next session into existence on the world thread. Both
--   register through the same @atomicModifyIORef'@ shape in
--   "World.Thread.Command.Init"; the load path registers its own
--   replacement manager instead and is deliberately not one of these
--   (see 'World.State.Types.wmSessionTeardown').
beginsSession ∷ WorldCommand → Bool
beginsSession (WorldInit _ _ _ _ _) = True
beginsSession (WorldInitArena _)    = True
beginsSession _                     = False

-- | Put @cmd@ back at the FRONT of @q@, ahead of everything still
--   queued behind it. The queue has no push-front, so the tail is
--   flushed and rewritten after it; both operations run on this thread,
--   the only consumer, so a producer appending concurrently lands after
--   the rewritten tail exactly as it would have anyway.
requeueAhead ∷ Q.Queue α → α → IO ()
requeueAhead q cmd = do
    rest ← Q.flushQueue q
    Q.writeQueue q cmd
    mapM_ (Q.writeQueue q) rest

-- | The capture lock admits only its queued WorldSave / WorldLoadPublish
-- command. A load's WorldLoadPublish reaches this window only after every
-- other state owner has already quiesced against the SAME save-barrier
-- protocol a save uses (issue #763, save-overhaul C2 — see
-- "Engine.Scripting.Lua.Thread.Dispatch"), so the two authorized command
-- kinds never contend with each other.
--
-- What happens to every OTHER command still sitting in the queue differs
-- by kind (requirement 12: isolate old asynchronous work). A save doesn't
-- replace anything -- the live session stays the SAME session before and
-- after -- so its non-authorized commands are simply deferred, ordered
-- exactly as before, for the world owner's next unlocked tick
-- (unchanged from pre-#763 behaviour). A load publish REPLACES THE
-- COMPLETE SESSION: anything else queued at this exact moment was queued
-- against the OLD session (a debug-console/Lua call that landed just
-- before this window closed, a sim writeback that raced the barrier,
-- ...) and, if merely deferred, would run again on the world owner's
-- very next tick -- AFTER the swap -- against the NEW session instead,
-- silently corrupting it (e.g. a stale WorldSetTime for a page id the
-- new session also happens to use). Since no queued WorldCommand
-- represents durable intent (everything durable already lives in the
-- staged snapshot this transaction is about to publish), it is safe,
-- and required, to discard it outright rather than let it survive into
-- the replacement.
processAuthorizedSave ∷ EngineEnv → LoggerState → IO ()
processAuthorizedSave env logger = do
    commands ← Q.flushQueue (wsWorldQueue (toWorldSimCapability env))
    let (authorized, deferred) = partitionAuthorized commands
        discarded = length commands - length authorized - length deferred
    when (discarded > 0) $
        logWarn logger CatWorld $
            "Load publish discarded " <> tshow discarded
            <> " stale WorldCommand(s) queued before the whole-session replacement"
    forM_ authorized $ \cmd → handleWorldCommand env logger cmd
                               >> settleSelection env
    forM_ deferred $ Q.writeQueue (wsWorldQueue (toWorldSimCapability env))

-- | Once the queue holds no outstanding selection request, the
-- projection IS the applied state (#1602). Running it after every
-- command keeps a request whose predicted effect never materialised —
-- a @world.show@ the handler refused because the page does not exist —
-- from leaving every later placement binding permanently stale.
settleSelection ∷ EngineEnv → IO ()
settleSelection env = do
    -- Read first, write only when there is something to settle. This
    -- runs after EVERY world command, and the world-manager ref is read
    -- by several threads, so an unconditional atomicModifyIORef' here
    -- would add a contended write to the hot command path for no reason
    -- — the overwhelming majority of commands are not selection changes.
    --
    -- The read is not the decision: 'settleSelectionProjection'
    -- re-checks inside the atomic update, so a request that lands
    -- between the two is not swallowed.
    mgr ← readIORef ref
    when (needsSettle mgr) $
        atomicModifyIORef' ref $ \m → (settleSelectionProjection m, ())
  where
    ref = wsWorldManagerRef (toWorldSimCapability env)
    needsSettle m = wmSelectionPending m ≡ 0
        ∧ (wmProjectedGen m ≢ wmSelectionGen m
           ∨ wmProjectedVisible m ≢ wmVisible m)

-- | Pure split of one captureLocked-window batch into (authorized to run
-- now, preserved for after release) -- see 'processAuthorizedSave' for
-- why a load publish discards the rest instead of preserving it. Exposed
-- for direct hspec coverage of the discard decision without needing a
-- real queue/engine.
partitionAuthorized ∷ [WorldCommand] → ([WorldCommand], [WorldCommand])
partitionAuthorized commands
    | any isLoadPublish authorized = (authorized, [])
    | otherwise                    = (authorized, deferred)
  where
    (authorized, deferred) = partition isAuthorized commands
    isAuthorized (WorldSave _ _ _ _ _ _) = True
    isAuthorized (WorldLoadPublish _)  = True
    isAuthorized _                     = False
    isLoadPublish (WorldLoadPublish _) = True
    isLoadPublish _                    = False

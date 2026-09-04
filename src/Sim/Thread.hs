{-# LANGUAGE Strict #-}
module Sim.Thread
    ( startSimThread
    , completeFastSettleWith
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Maybe (mapMaybe)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import System.Timeout (timeout)
import Engine.Core.Clock (monotonicSeconds)
import Engine.Core.Thread
    (ThreadState, WorkerFailLevel(..), WorkerSpec(..), noRefusal
    , startWorkerThread, workerCrashStderrSink)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.Core
    (CoreCapability(..), toCoreCapability)
import Engine.Core.State (EngineEnv, saveBarrierRef)
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, ownerGated)
import Engine.Core.Log (logDebug, logError, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import World.Page.Types (WorldPageId(..))
import World.Fluid.Types (FluidCell(..), renderedSurfaceZ)
import World.Command.Types (WorldCommand(..), FluidWriteback(..)
                           , FluidWritebackBatch(..)
                           , FluidAckOutcome(..))
import Sim.Command.Types (SimCommand(..), FastSettleRequest(..)
                         , FastSettleOutcome(..))
import Sim.State.Types (SimState(..), SimWorldState(..), SimChunkState(..)
                       , emptySimState, emptySimWorldState)
import Sim.Fluid.Types (activeToFluidCell)
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Chunk (applyChunkEdit, loadedChunkState, reactivateSettleTicks)

-- | Hard cap on synchronous settle iterations for 'SimFastSettleAll'
--   (the dump path) — a safety net against runaway settling, not
--   expected to be hit in practice.
maxFastSettleIterations ∷ Int
maxFastSettleIterations = 500

startSimThread ∷ EngineEnv → IO ThreadState
startSimThread env = startWorkerThread WorkerSpec
    { wsName        = "Sim"
    , wsLoggerRef   = ccLoggerRef (toCoreCapability env)
    , wsCategory    = CatWorld
    , wsLifecycleRef = ccLifecycleRef (toCoreCapability env)
    , wsCrashSink   = workerCrashStderrSink
    , wsStartingMsg = "Starting simulation thread..."
    , wsStartedMsg  = Just "Simulation thread started"
    , wsFailMsg     = "Failed starting sim thread: "
    , wsFailLevel   = WorkerFailError
    , wsFailFatal   = "Sim thread start failure."
    , wsStartup     = \_ → noRefusal (newIORef emptySimState)
    , wsTick        = simTick env
    , wsOnStop      = \_ → do
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        logDebug logger CatWorld "Sim thread stopping..."
    , wsOnCrash     = \_ e → do
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        logError logger CatWorld $ "Sim thread crashed: " <> tshow e
      -- The lifecycle write this line used to precede belongs to the
      -- shared loop now, ahead of the log (#2283).
    , wsOnCrashCleanup = \_ _ → pure ()
    }

-- | True when at least one world is active and holds chunks — i.e. there
--   is simulation work to do this tick.
anyLiveWorld ∷ SimState → Bool
anyLiveWorld ss = any (\sws → swsActive sws ∧ not (HM.null (swsChunks sws)))
                      (HM.elems (ssWorlds ss))

simTick ∷ EngineEnv → IORef SimState → IO (Maybe (IORef SimState))
simTick env simStateRef = do
    logger ← readIORef (ccLoggerRef (toCoreCapability env))
    -- Process all pending commands
    -- #2221: 'ownerGated', not 'captureLocked' — parked from this
    -- owner's own final-pass acknowledgement, so no fluid tick and no
    -- WorldApplyFluids writeback can straddle the boundary.
    locked ← ownerGated (saveBarrierRef env) SaveSimulation
    unless locked $ processSimCommands env logger simStateRef

    ss ← readIORef simStateRef
    -- 'ssPaused' is set ONLY by 'SimFastSettleAll' (dump
    -- mode's own synchronous settle path) --
    -- engine.setPaused (Engine.Scripting
    -- .Lua.API.Core.setPausedFn) writes ONLY
    -- 'enginePausedRef' and has never dispatched a SimPause
    -- command, so 'ssPaused' alone never reflected ordinary
    -- gameplay pause at all. Concretely for #763: a load
    -- publish sets 'enginePausedRef' True but never touches
    -- 'ssPaused', so fluid simulation kept ticking against
    -- the freshly-published, "supposedly paused" session.
    -- Reading 'enginePausedRef' directly (the single
    -- authoritative flag every other paused-gate in the
    -- engine already reads) fixes both: the general
    -- gameplay-pause gap and this issue's post-publish one.
    enginePaused ← readIORef (wsEnginePausedRef (toWorldSimCapability env))
    -- Acknowledging BEFORE this tick's own work (the
    -- tick/emitWorldDirtyFluids branch below, which queues
    -- WorldApplyFluids writebacks to the world thread) let
    -- this ack be the FINAL one a quiescence
    -- pass needed while a writeback was still about to be
    -- produced -- the world thread could already have
    -- processed WorldLoadPublish and released the barrier
    -- by the time that late writeback arrived, letting it
    -- mutate a freshly-published page that reused the same
    -- id. Moved to fire only once BOTH branches below have
    -- fully finished producing (or skipping) that work.
    if locked ∨ ssPaused ss ∨ enginePaused ∨ not (anyLiveWorld ss)
        then do
            acknowledgeCurrent (saveBarrierRef env) SaveSimulation
            threadDelay (ssTickRate ss)
            pure (Just simStateRef)
        else do
            -- Tick every active world independently, emit each
            -- world's dirty fluids tagged with its page id, then
            -- clear the per-world dirty sets.
            let ticked = HM.map tickWorld (ssWorlds ss)
            forM_ (HM.toList ticked) $ \(pid, sws) →
                when (swsActive sws) $
                    emitWorldDirtyFluids env pid sws Nothing
            let cleared = HM.map clearDirty ticked
            writeIORef simStateRef ss { ssWorlds = cleared }

            acknowledgeCurrent (saveBarrierRef env) SaveSimulation
            threadDelay (ssTickRate ss)
            pure (Just simStateRef)

-- | Settle + simulate one world's chunks (a no-op for an inactive world).
tickWorld ∷ SimWorldState → SimWorldState
tickWorld sws
    | not (swsActive sws) = sws
    | otherwise           = simulateActiveTick (settleNewChunks sws)

clearDirty ∷ SimWorldState → SimWorldState
clearDirty sws = sws { swsDirtyChunks = HS.empty }

processSimCommands ∷ EngineEnv → LoggerState → IORef SimState → IO ()
processSimCommands env logger simStateRef = do
    mCmd ← Q.tryReadQueue (wsSimQueue (toWorldSimCapability env))
    case mCmd of
        Just cmd → do
            handleSimCommand env logger simStateRef cmd
            processSimCommands env logger simStateRef
        Nothing → return ()

-- | Apply @f@ to one world's state, creating an empty entry if absent.
modifyWorld ∷ WorldPageId → (SimWorldState → SimWorldState)
            → SimState → SimState
modifyWorld pid f ss =
    let cur = HM.lookupDefault emptySimWorldState pid (ssWorlds ss)
    in ss { ssWorlds = HM.insert pid (f cur) (ssWorlds ss) }

handleSimCommand ∷ EngineEnv → LoggerState → IORef SimState → SimCommand → IO ()
handleSimCommand env logger simStateRef cmd = do
    ss ← readIORef simStateRef
    case cmd of
        SimActivateWorld pid topo → do
            -- Re-trigger settle so this world's existing chunks get
            -- simulated now that writeback is possible. Activation is
            -- what lets this world tick at all, so it is also where the
            -- page's seam topology lands (#2044).
            writeIORef simStateRef $
                modifyWorld pid (\sws → sws
                    { swsActive = True
                    , swsTopology = topo
                    , swsChunks = HM.map (\scs → scs { scsSettleTicks = reactivateSettleTicks })
                                         (swsChunks sws)
                    }) ss
            logDebug logger CatWorld $ "Sim: world activated " <> tshow pid

        SimDeactivateWorld pid → do
            -- Hidden: stop ticking but KEEP the chunks so a later show can
            -- resume them (ChunkLoading won't re-emit SimChunkLoaded for
            -- already-loaded coords, so dropping them here would leave the
            -- re-shown world's sim inert). Other worlds untouched (#55).
            writeIORef simStateRef $
                ss { ssWorlds = HM.adjust (\sws → sws { swsActive = False })
                                          pid (ssWorlds ss) }
            logDebug logger CatWorld $ "Sim: world deactivated " <> tshow pid

        SimDropWorld pid → do
            -- Destroyed: discard this world's sim state entirely (#61).
            writeIORef simStateRef $
                ss { ssWorlds = HM.delete pid (ssWorlds ss) }
            logDebug logger CatWorld $ "Sim: world dropped " <> tshow pid

        SimChunkLoaded pid topo coord fluidMap terrainMap → do
            writeIORef simStateRef $
                modifyWorld pid (\sws → sws
                    { swsTopology = topo
                    , swsChunks   = HM.insert coord
                                        (loadedChunkState fluidMap terrainMap)
                                        (swsChunks sws)
                    }) ss

        SimChunkUnloaded pid coord → do
            writeIORef simStateRef $
                modifyWorld pid (\sws →
                    sws { swsChunks = HM.delete coord (swsChunks sws) }) ss

        SimChunkEdited pid topo coord editGen fluidMap terrainMap →
            -- Re-seed the edited chunk from the authoritative post-edit
            -- tiles and wake it plus its four physically adjacent
            -- neighbours. The topology travels with the message, so
            -- 'applyChunkEdit' resolves those neighbours through the
            -- page's own seam frame (#2044).
            writeIORef simStateRef $
                modifyWorld pid
                    (applyChunkEdit coord editGen fluidMap terrainMap
                        . (\sws → sws { swsTopology = topo })) ss

        SimSetTickRate rate →
            writeIORef simStateRef $ ss { ssTickRate = rate }

        SimPause →
            writeIORef simStateRef $ ss { ssPaused = True }

        SimResume →
            writeIORef simStateRef $ ss { ssPaused = False }

        SimFastSettleAll req → do
            -- No wsTilesRef re-sync needed: the world sends the FINAL
            -- fluid in SimChunkLoaded (the old post-load seal that this
            -- guarded against was removed), so scsFluid is already fresh.
            -- Run sim ticks synchronously (no sleeping) for every world
            -- until all its chunks are settled and inactive. Capped at
            -- maxFastSettleIterations as a safety net. Explicitly unpause —
            -- the dump path pauses the sim before chunks load, but
            -- simulateActiveTick is a no-op while paused, so the synchronous
            -- settle below would do nothing.
            let settled = HM.map (fastSettleWorld maxFastSettleIterations) (ssWorlds ss)
                -- Mark every chunk dirty so the whole settled state is
                -- emitted to the world thread.
                dirtied = HM.map (\sws →
                    sws { swsDirtyChunks = HS.fromList (HM.keys (swsChunks sws)) })
                    settled
            -- Persist the cleared (post-emit) state.
            writeIORef simStateRef $
                ss { ssWorlds = HM.map clearDirty dirtied, ssPaused = True }
            -- Emit each world's batch and WAIT for the world thread to apply
            -- it before signalling done — the dump reads wsTilesRef right
            -- after. One ack per world (dump worlds are typically just one).
            completeFastSettleWith
                (\pid sws ack → emitWorldDirtyFluids env pid sws (Just ack))
                monotonicSeconds (logDebug logger CatWorld) req
                (HM.toList dirtied)

-- | The whole tail of the 'SimFastSettleAll' handler: wait out every
--   world's acknowledgement, report, and publish the outcome to the
--   request's completion (#2334).
--
--   The publication is the last statement, AFTER the report, and that
--   ordering is the point of keeping the two together in one tested
--   function. Filling 'fsrDone' first would let a failure in the report
--   kill this worker in the window where the caller has already read
--   success and is about to emit output derived from a half-dead
--   engine; the caller has no way to take that reading back. So the
--   completion is only ever published once nothing fallible is left.
--
--   Both the emit step and the report are parameters so the headless
--   gate can drive this without a world thread to acknowledge anything
--   and without a logger that can be made to fail. Production supplies
--   'emitWorldDirtyFluids' and @'logDebug' logger 'CatWorld'@, which is
--   exactly what ran here before the seam existed.
completeFastSettleWith
    ∷ (WorldPageId → SimWorldState → MVar FluidAckOutcome → IO ())
      -- ^ Emit one world's batch, to be acknowledged through this MVar.
    → IO Double
      -- ^ Monotonic clock; production passes
      --   'Engine.Core.Clock.monotonicSeconds'.
    → (Text → IO ())
      -- ^ Report the settle's outcome.
    → FastSettleRequest
    → [(WorldPageId, SimWorldState)]
    → IO ()
completeFastSettleWith emit clock report req worlds = do
    outcome ← awaitFastSettleAcks emit clock (fsrDeadline req) worlds
    report ("Sim: fast-settled and paused (" <> tshow outcome <> ")")
    putMVar (fsrDone req) outcome

-- | Emit one 'WorldApplyFluids' batch per world and wait for each to be
--   acknowledged, against ONE end-to-end deadline (#2334).
--
--   The deadline is an absolute monotonic instant, so the worlds SHARE
--   the caller's budget instead of each restarting it: three worlds
--   cannot turn a ten-minute budget into thirty minutes. An expired
--   deadline yields a zero-microsecond 'timeout', which gives up at once
--   rather than waiting forever — the negative value @deadline - now@
--   would otherwise produce means "no timeout at all" to 'timeout',
--   which is exactly the unbounded wait this replaces.
--
--   Stops at the FIRST world that fails or times out and reports it: the
--   remaining worlds' batches are never emitted, because the settle has
--   already failed and nothing downstream may read the tiles.
--
--   Reached only through 'completeFastSettleWith', which is what the
--   headless gate drives: an outcome computed correctly and then never
--   published strands the caller just as completely as one never
--   computed, so the two are gated together.
awaitFastSettleAcks
    ∷ (WorldPageId → SimWorldState → MVar FluidAckOutcome → IO ())
      -- ^ Emit one world's batch, to be acknowledged through this MVar.
    → IO Double
      -- ^ Monotonic clock; production passes
      --   'Engine.Core.Clock.monotonicSeconds'.
    → Double
      -- ^ The shared absolute deadline ('Sim.Command.Types.fsrDeadline').
    → [(WorldPageId, SimWorldState)]
    → IO FastSettleOutcome
awaitFastSettleAcks emit clock deadline = go
  where
    go [] = pure FastSettleApplied
    go ((pid, sws):rest) = do
        ack ← newEmptyMVar
        emit pid sws ack
        budget ← remainingMicros clock deadline
        acked ← timeout budget (takeMVar ack)
        case acked of
            Nothing                   → pure (FastSettleAckDeadline pid)
            Just (FluidAckFailed why) → pure (FastSettleWorldFailed pid why)
            Just FluidAckApplied      → go rest

-- | Microseconds left before an absolute monotonic deadline, floored at
--   zero. See 'awaitFastSettleAcks' for why the floor matters.
remainingMicros ∷ IO Double → Double → IO Int
remainingMicros clock deadline = do
    now ← clock
    pure $ max 0 (ceiling ((deadline - now) * 1000000))

-- | Run all sim ticks synchronously without sleeping for one world. Stops
--   when no chunk has settle ticks remaining and no chunk is active, or
--   when the iteration cap is reached.
fastSettleWorld ∷ Int → SimWorldState → SimWorldState
fastSettleWorld = go
  where
    go 0 sws = sws
    go n sws
      | allDone sws = sws
      | otherwise   = go (n - 1) (simulateActiveTick (settleNewChunks sws))

    allDone sws =
        not (any (\scs → scsSettleTicks scs > 0) (swsChunks sws))
        ∧ not (any scsActive (swsChunks sws))

-- | Tick down the per-chunk fast-settle countdown for one world. A freshly
--   loaded or just-edited chunk starts with a non-zero 'scsSettleTicks';
--   'fastSettleWorld' iterates until every chunk's countdown has reached 0
--   (and no chunk is active), which is how the synchronous settle knows the
--   world has quiesced. A no-op once all countdowns are 0.
settleNewChunks ∷ SimWorldState → SimWorldState
settleNewChunks sws
    | not (any (\scs → scsSettleTicks scs > 0) (swsChunks sws)) = sws
    | otherwise =
        let decremented = HM.map (\scs →
                if scsSettleTicks scs > 0
                    then scs { scsSettleTicks = scsSettleTicks scs - 1 }
                    else scs
                ) (swsChunks sws)
        in sws { swsChunks = decremented }

-- | Emit one world's dirty chunks' fluid results to the WORLD thread (the
--   sole writer of 'wsTilesRef') as a 'WorldApplyFluids' batch tagged with
--   the world's page id, so the world thread applies it ONLY to that world
--   (#59). The sim never touches 'wsTilesRef' itself. With 'Just' ack, the
--   world reports the batch's outcome through it once it is done with it
--   (the synchronous fast-settle waits on that).
emitWorldDirtyFluids ∷ EngineEnv → WorldPageId → SimWorldState
                     → Maybe (MVar FluidAckOutcome) → IO ()
emitWorldDirtyFluids env pid sws mAck = do
    let dirty = swsDirtyChunks sws
        writebacks = mapMaybe (\cc →
            case HM.lookup cc (swsChunks sws) of
                Nothing  → Nothing
                Just scs →
                    let newFluid = if scsActive scs
                            then deriveFluidMap scs
                            else scsFluid scs
                        newTerrain = scsTerrain scs
                        -- The rendered-surface rule (river renders flat)
                        -- has ONE definition, shared with generation and
                        -- the edit-replay paths: World.Fluid.Types.
                        newSurf = VU.imap (\idx terrZ →
                            renderedSurfaceZ terrZ (newFluid V.! idx)
                            ) newTerrain
                    in Just (FluidWriteback cc (scsEditGen scs) newFluid
                                            newTerrain newSurf
                                            (scsSideDeco scs))
            ) (HS.toList dirty)
    when (not (null writebacks) ∨ isJust mAck) $
        Q.writeQueue (wsWorldQueue (toWorldSimCapability env))
            (WorldApplyFluids (FluidWritebackBatch pid writebacks mAck))

deriveFluidMap ∷ SimChunkState → V.Vector (Maybe FluidCell)
deriveFluidMap scs =
    let terrV = scsTerrain scs
    in V.imap (\idx mafc →
        case mafc of
            Nothing  → Nothing
            Just afc → activeToFluidCell (terrV VU.! idx) afc
        ) (scsActiveFluid scs)

{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Thread
    ( startSimThread
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Maybe (mapMaybe)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, finally)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Log (logInfo, logDebug, logError, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Page.Types (WorldPageId(..))
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Command.Types (WorldCommand(..), FluidWriteback(..)
                           , FluidWritebackBatch(..))
import Sim.Command.Types (SimCommand(..))
import Sim.State.Types (SimState(..), SimWorldState(..), SimChunkState(..)
                       , emptySimState, emptySimWorldState)
import Sim.Fluid.Types (fluidCellToActive, activeToFluidCell)
import Sim.Fluid.Active (simulateActiveTick)

startSimThread ∷ EngineEnv → IO ThreadState
startSimThread env = do
    logger ← readIORef (loggerRef env)
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do
            logInfo logger CatWorld "Starting simulation thread..."
            simStateRef ← newIORef emptySimState
            tid ← forkIO $ simLoop env stateRef simStateRef `finally` putMVar doneVar ()
            logInfo logger CatWorld "Simulation thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatWorld $ "Failed starting sim thread: " <> T.pack (show e)
            error "Sim thread start failure."
        )
    return $ ThreadState stateRef threadId doneVar

-- | True when at least one world is active and holds chunks — i.e. there
--   is simulation work to do this tick.
anyLiveWorld ∷ SimState → Bool
anyLiveWorld ss = any (\sws → swsActive sws ∧ not (HM.null (swsChunks sws)))
                      (HM.elems (ssWorlds ss))

simLoop ∷ EngineEnv → IORef ThreadControl → IORef SimState → IO ()
simLoop env stateRef simStateRef = do
    control ← readIORef stateRef
    logger ← readIORef (loggerRef env)
    case control of
        ThreadStopped → do
            logDebug logger CatWorld "Sim thread stopping..."
            pure ()
        ThreadPaused → do
            threadDelay 100000
            simLoop env stateRef simStateRef
        ThreadRunning → do
            -- One guarded tick per iteration; the recursive call lives
            -- OUTSIDE the catch — inside it, each tick pushes a catch
            -- frame that never pops (unbounded stack growth).
            ok ← catch
              (do
                -- Process all pending commands
                processSimCommands env logger simStateRef

                ss ← readIORef simStateRef

                if ssPaused ss ∨ not (anyLiveWorld ss)
                    then do
                        threadDelay (ssTickRate ss)
                        pure True
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

                        threadDelay (ssTickRate ss)
                        pure True
              )
              (\(e ∷ SomeException) → do
                logError logger CatWorld $ "Sim thread crashed: " <> T.pack (show e)
                writeIORef (lifecycleRef env) CleaningUp
                pure False
              )
            when ok $ simLoop env stateRef simStateRef

-- | Settle + simulate one world's chunks (a no-op for an inactive world).
tickWorld ∷ SimWorldState → SimWorldState
tickWorld sws
    | not (swsActive sws) = sws
    | otherwise           = simulateActiveTick (settleNewChunks sws)

clearDirty ∷ SimWorldState → SimWorldState
clearDirty sws = sws { swsDirtyChunks = HS.empty }

processSimCommands ∷ EngineEnv → LoggerState → IORef SimState → IO ()
processSimCommands env logger simStateRef = do
    mCmd ← Q.tryReadQueue (simQueue env)
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
        SimActivateWorld pid → do
            -- Re-trigger settle so this world's existing chunks get
            -- simulated now that writeback is possible.
            writeIORef simStateRef $
                modifyWorld pid (\sws → sws
                    { swsActive = True
                    , swsChunks = HM.map (\scs → scs { scsSettleTicks = 24 })
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

        SimChunkLoaded pid coord fluidMap terrainMap → do
            let sz = chunkSize * chunkSize
                scs = SimChunkState
                    { scsFluid       = fluidMap
                    , scsTerrain     = terrainMap
                    , scsGenFluid    = fluidMap
                    , scsSettleTicks = 64
                    , scsActive      = False
                    , scsActiveFluid = V.replicate sz Nothing
                    , scsEquilTicks  = 0
                    , scsSideDeco    = VU.replicate sz 0
                    }
            writeIORef simStateRef $
                modifyWorld pid (\sws →
                    sws { swsChunks = HM.insert coord scs (swsChunks sws) }) ss

        SimChunkUnloaded pid coord → do
            writeIORef simStateRef $
                modifyWorld pid (\sws →
                    sws { swsChunks = HM.delete coord (swsChunks sws) }) ss

        SimChunkEdited pid coord fluidMap terrainMap → do
            let sws = HM.lookupDefault emptySimWorldState pid (ssWorlds ss)
                sz  = chunkSize * chunkSize
                -- Re-seed from the authoritative post-edit tiles. Build on
                -- the existing sim chunk if present, else create one (an
                -- edit can land before the sim has loaded that chunk).
                base = case HM.lookup coord (swsChunks sws) of
                    Just scs → scs { scsFluid       = fluidMap
                                   , scsTerrain     = terrainMap
                                   , scsGenFluid    = fluidMap
                                   , scsSettleTicks = 24
                                   }
                    Nothing  → SimChunkState
                        { scsFluid       = fluidMap
                        , scsTerrain     = terrainMap
                        , scsGenFluid    = fluidMap
                        , scsSettleTicks = 24
                        , scsActive      = False
                        , scsActiveFluid = V.replicate sz Nothing
                        , scsEquilTicks  = 0
                        , scsSideDeco    = VU.replicate sz 0
                        }
                -- Force a fresh activation so the volume grid is rebuilt
                -- from the NEW fluid: activateChunk no-ops on an already-
                -- active chunk, so clear the flag first. Without this the
                -- edited chunk kept the new snapshot but never flowed (#60).
                activated = activateChunk (base { scsActive = False })
                -- Activate the 4 cardinal neighbours too so dammed water can
                -- spill across the chunk seam (reconcileSeams needs both
                -- sides active). HM.adjust is a no-op for unloaded
                -- neighbours; activateChunk is idempotent for active ones.
                ChunkCoord cx cy = coord
                nbrCoords = [ ChunkCoord (cx + 1) cy, ChunkCoord (cx - 1) cy
                            , ChunkCoord cx (cy + 1), ChunkCoord cx (cy - 1) ]
                withSelf = HM.insert coord activated (swsChunks sws)
                withNbrs = foldl' (\m nc → HM.adjust activateChunk nc m)
                                  withSelf nbrCoords
            writeIORef simStateRef $
                ss { ssWorlds = HM.insert pid
                                  (sws { swsChunks = withNbrs })
                                  (ssWorlds ss) }

        SimSetTickRate rate →
            writeIORef simStateRef $ ss { ssTickRate = rate }

        SimPause →
            writeIORef simStateRef $ ss { ssPaused = True }

        SimResume →
            writeIORef simStateRef $ ss { ssPaused = False }

        SimFastSettleAll done → do
            -- No wsTilesRef re-sync needed: the world sends the FINAL
            -- fluid in SimChunkLoaded (the old post-load seal that this
            -- guarded against was removed), so scsFluid is already fresh.
            -- Run sim ticks synchronously (no sleeping) for every world
            -- until all its chunks are settled and inactive. Capped at
            -- maxIterations as a safety net. Explicitly unpause — the dump
            -- path pauses the sim before chunks load, but simulateActiveTick
            -- is a no-op while paused, so the synchronous settle below would
            -- do nothing.
            let maxIterations = 500 ∷ Int
                settled = HM.map (fastSettleWorld maxIterations) (ssWorlds ss)
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
            forM_ (HM.toList dirtied) $ \(pid, sws) → do
                ack ← newEmptyMVar
                emitWorldDirtyFluids env pid sws (Just ack)
                takeMVar ack
            putMVar done ()
            logDebug logger CatWorld "Sim: fast-settled and paused"

tshow ∷ Show a ⇒ a → T.Text
tshow = T.pack ∘ show

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

-- | Activate a passive chunk for volume-based simulation.
activateChunk ∷ SimChunkState → SimChunkState
activateChunk scs
    | scsActive scs = scs  -- already active
    | otherwise =
        let terrV = scsTerrain scs
            fluidV = scsFluid scs
            _sz = V.length fluidV
            activeFluid = V.imap (\idx mfc →
                case mfc of
                    Nothing → Nothing
                    Just fc → fluidCellToActive (terrV VU.! idx) fc
                ) fluidV
        in scs { scsActive      = True
               , scsActiveFluid = activeFluid
               , scsEquilTicks  = 0
               }

-- | Emit one world's dirty chunks' fluid results to the WORLD thread (the
--   sole writer of 'wsTilesRef') as a 'WorldApplyFluids' batch tagged with
--   the world's page id, so the world thread applies it ONLY to that world
--   (#59). The sim never touches 'wsTilesRef' itself. With 'Just' ack, the
--   world signals it after applying (the synchronous fast-settle waits).
emitWorldDirtyFluids ∷ EngineEnv → WorldPageId → SimWorldState
                     → Maybe (MVar ()) → IO ()
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
                        -- River fluid renders as a flat plane: surface
                        -- comes directly from the fluid; other fluid sits
                        -- at max(terrain, surface). Must match the rule in
                        -- World/Generate/Chunk.hs and ChunkLoading.hs.
                        newSurf = VU.imap (\idx terrZ →
                            case newFluid V.! idx of
                                Just fc | fcType fc ≡ River → fcSurface fc
                                Just fc → max terrZ (fcSurface fc)
                                Nothing → terrZ
                            ) newTerrain
                    in Just (FluidWriteback cc newFluid newTerrain newSurf
                                            (scsSideDeco scs))
            ) (HS.toList dirty)
    when (not (null writebacks) ∨ isJust mAck) $
        Q.writeQueue (worldQueue env)
            (WorldApplyFluids (FluidWritebackBatch pid writebacks mAck))

deriveFluidMap ∷ SimChunkState → V.Vector (Maybe FluidCell)
deriveFluidMap scs =
    let terrV = scsTerrain scs
    in V.imap (\idx mafc →
        case mafc of
            Nothing  → Nothing
            Just afc → activeToFluidCell (terrV VU.! idx) afc
        ) (scsActiveFluid scs)

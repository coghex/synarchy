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
import Data.Maybe (mapMaybe, isJust)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, finally)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn, LogCategory(..)
                        , LoggerState)
import qualified Engine.Core.Queue as Q
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Command.Types (WorldCommand(..), FluidWriteback(..)
                           , FluidWritebackBatch(..))
import Sim.Command.Types (SimCommand(..))
import Sim.State.Types (SimState(..), SimChunkState(..), emptySimState)
import Sim.Fluid.Types (ActiveFluidCell(..), fluidCellToActive, activeToFluidCell)
import Sim.Fluid.Tick (simulateFluidTick)
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

                if ssPaused ss ∨ HM.null (ssChunks ss)
                                ∨ not (ssWorldActive ss)
                    then do
                        threadDelay (ssTickRate ss)
                        pure True
                    else do
                        ss' ← settleNewChunks ss
                        let ss''   = simulateFluidTick ss'
                            ss'''  = simulateActiveTick ss''
                        emitDirtyFluids env ss''' Nothing
                        let ss'''' = ss''' { ssDirtyChunks = HS.empty }
                        writeIORef simStateRef ss''''

                        threadDelay (ssTickRate ss'''')
                        pure True
              )
              (\(e ∷ SomeException) → do
                logError logger CatWorld $ "Sim thread crashed: " <> T.pack (show e)
                writeIORef (lifecycleRef env) CleaningUp
                pure False
              )
            when ok $ simLoop env stateRef simStateRef

processSimCommands ∷ EngineEnv → LoggerState → IORef SimState → IO ()
processSimCommands env logger simStateRef = do
    mCmd ← Q.tryReadQueue (simQueue env)
    case mCmd of
        Just cmd → do
            handleSimCommand env logger simStateRef cmd
            processSimCommands env logger simStateRef
        Nothing → return ()

handleSimCommand ∷ EngineEnv → LoggerState → IORef SimState → SimCommand → IO ()
handleSimCommand env logger simStateRef cmd = do
    ss ← readIORef simStateRef
    case cmd of
        SimActivateWorld → do
            -- Re-trigger settle so existing chunks get simulated now that writeback is possible
            let reSettled = HM.map (\scs →
                    scs { scsSettleTicks = 24 }) (ssChunks ss)
            writeIORef simStateRef $
                ss { ssWorldActive = True
                   , ssChunks = reSettled }
            logDebug logger CatWorld "Sim: world activated"

        SimDeactivateWorld → do
            writeIORef simStateRef $ emptySimState
                { ssTickRate = ssTickRate ss }
            logDebug logger CatWorld "Sim: world deactivated"

        SimChunkLoaded coord fluidMap terrainMap → do
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
                ss { ssChunks = HM.insert coord scs (ssChunks ss) }

        SimChunkUnloaded coord → do
            writeIORef simStateRef $
                ss { ssChunks = HM.delete coord (ssChunks ss) }

        SimTerrainModified coord mods → do
            case HM.lookup coord (ssChunks ss) of
                Nothing → pure ()
                Just scs → do
                    let terrV' = scsTerrain scs VU.// mods
                        -- Activate the chunk for volume simulation
                        activated = activateChunk (scs { scsTerrain = terrV'
                                                       , scsSettleTicks = 24
                                                       })
                        -- Activate the 4 cardinal neighbours too, so dammed
                        -- water can spill across the chunk seam: the seam
                        -- exchange pass (Sim.Fluid.Active.reconcileSeams)
                        -- needs both sides active to have a grid to transfer
                        -- into. HM.adjust is a no-op for unloaded neighbours;
                        -- activateChunk is idempotent for already-active ones.
                        ChunkCoord cx cy = coord
                        nbrCoords = [ ChunkCoord (cx + 1) cy, ChunkCoord (cx - 1) cy
                                    , ChunkCoord cx (cy + 1), ChunkCoord cx (cy - 1) ]
                        withSelf = HM.insert coord activated (ssChunks ss)
                        withNbrs = foldl' (\m nc → HM.adjust activateChunk nc m)
                                          withSelf nbrCoords
                    writeIORef simStateRef $ ss { ssChunks = withNbrs }

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
            -- Run sim ticks synchronously (no sleeping) until all chunks
            -- are settled and inactive. Capped at maxIterations as a
            -- safety net. Explicitly unpause — the dump path pauses the
            -- sim before chunks load but simulateFluidTick is a no-op
            -- when paused.
            let maxIterations = 500 ∷ Int
                ssUnpaused = ss { ssPaused = False }
                ssSettled = fastSettleAll maxIterations ssUnpaused
                -- Mark every chunk dirty so the whole settled state is
                -- emitted to the world thread.
                allCoords = HS.fromList (HM.keys (ssChunks ssSettled))
                ssDirty = ssSettled { ssDirtyChunks = allCoords
                                    , ssPaused = True }
            writeIORef simStateRef (ssDirty { ssDirtyChunks = HS.empty })
            -- Emit to the world thread (sole writer) and WAIT for it to
            -- apply before signalling done — the dump reads wsTilesRef
            -- immediately after.
            ack ← newEmptyMVar
            emitDirtyFluids env ssDirty (Just ack)
            takeMVar ack
            putMVar done ()
            logDebug logger CatWorld "Sim: fast-settled and paused"

-- | Run all sim ticks synchronously without sleeping. Stops when no
--   chunks have settle ticks remaining and no chunks are active, or
--   when the iteration cap is reached.
fastSettleAll ∷ Int → SimState → SimState
fastSettleAll = go
  where
    go 0 ss = ss
    go n ss
      | allDone ss = ss
      | otherwise =
          let ss'   = settleNewChunksPure ss
              ss''  = simulateFluidTick ss'
              ss''' = simulateActiveTick ss''
          in go (n - 1) ss'''

    allDone ss =
        not (any (\scs → scsSettleTicks scs > 0) (ssChunks ss))
        ∧ not (any scsActive (ssChunks ss))

-- | Pure version of settleNewChunks for use in synchronous fast-settle.
settleNewChunksPure ∷ SimState → SimState
settleNewChunksPure ss =
    if not (any (\scs → scsSettleTicks scs > 0) (ssChunks ss))
    then ss
    else
        let ss' = simulateFluidTick ss
            decremented = HM.map (\scs →
                if scsSettleTicks scs > 0
                    then scs { scsSettleTicks = scsSettleTicks scs - 1 }
                    else scs
                ) (ssChunks ss')
        in ss' { ssChunks = decremented }

-- | Activate a passive chunk for volume-based simulation.
activateChunk ∷ SimChunkState → SimChunkState
activateChunk scs
    | scsActive scs = scs  -- already active
    | otherwise =
        let terrV = scsTerrain scs
            fluidV = scsFluid scs
            sz = V.length fluidV
            activeFluid = V.imap (\idx mfc →
                case mfc of
                    Nothing → Nothing
                    Just fc → fluidCellToActive (terrV VU.! idx) fc
                ) fluidV
        in scs { scsActive      = True
               , scsActiveFluid = activeFluid
               , scsEquilTicks  = 0
               }

-- | Deactivate a chunk: bake active fluid back to passive FluidCells.
deactivateChunk ∷ SimChunkState → SimChunkState
deactivateChunk scs =
    let terrV = scsTerrain scs
        bakedFluid = V.imap (\idx mafc →
            case mafc of
                Nothing  → Nothing
                Just afc → activeToFluidCell (terrV VU.! idx) afc
            ) (scsActiveFluid scs)
    in scs { scsActive      = False
           , scsActiveFluid = V.replicate (V.length bakedFluid) Nothing
           , scsFluid       = bakedFluid
           , scsGenFluid    = bakedFluid
           , scsEquilTicks  = 0
           }

-- | Run fast settle ticks for newly loaded chunks.
settleNewChunks ∷ SimState → IO SimState
settleNewChunks ss = do
    let needsSettle = HM.filter (\scs → scsSettleTicks scs > 0) (ssChunks ss)
    if HM.null needsSettle
        then return ss
        else do
            let ss' = simulateFluidTick ss
                decremented = HM.map (\scs →
                    if scsSettleTicks scs > 0
                        then scs { scsSettleTicks = scsSettleTicks scs - 1 }
                        else scs
                    ) (ssChunks ss')
            return $ ss' { ssChunks = decremented }

-- | Emit the dirty chunks' fluid results to the WORLD thread (the sole
--   writer of 'wsTilesRef') as a 'WorldApplyFluids' batch — the sim
--   never touches 'wsTilesRef' itself. With 'Just' ack, the world
--   signals it after applying (the synchronous fast-settle waits on it).
emitDirtyFluids ∷ EngineEnv → SimState → Maybe (MVar ()) → IO ()
emitDirtyFluids env ss mAck = do
    let dirty = ssDirtyChunks ss
        writebacks = mapMaybe (\cc →
            case HM.lookup cc (ssChunks ss) of
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
            (WorldApplyFluids (FluidWritebackBatch writebacks mAck))

deriveFluidMap ∷ SimChunkState → V.Vector (Maybe FluidCell)
deriveFluidMap scs =
    let terrV = scsTerrain scs
    in V.imap (\idx mafc →
        case mafc of
            Nothing  → Nothing
            Just afc → activeToFluidCell (terrV VU.! idx) afc
        ) (scsActiveFluid scs)

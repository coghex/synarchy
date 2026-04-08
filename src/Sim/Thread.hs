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
import Data.IORef (IORef, readIORef, writeIORef, newIORef, atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Exception (SomeException, catch)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn, LogCategory(..)
                        , LoggerState)
import qualified Engine.Core.Queue as Q
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Fluid.Types (FluidCell(..))
import World.Tile.Types (WorldTileData(..), lookupChunk)
import World.State.Types (WorldManager(..), WorldState(..))
import Sim.Command.Types (SimCommand(..))
import Sim.State.Types (SimState(..), SimChunkState(..), emptySimState)
import Sim.Fluid.Types (ActiveFluidCell(..), fluidCellToActive, activeToFluidCell)
import Sim.Fluid.Tick (simulateFluidTick)
import Sim.Fluid.Active (simulateActiveTick)

startSimThread ∷ EngineEnv → IO ThreadState
startSimThread env = do
    logger ← readIORef (loggerRef env)
    stateRef ← newIORef ThreadRunning
    threadId ← catch
        (do
            logInfo logger CatWorld "Starting simulation thread..."
            simStateRef ← newIORef emptySimState
            tid ← forkIO $ simLoop env stateRef simStateRef
            logInfo logger CatWorld "Simulation thread started"
            return tid
        )
        (\(e ∷ SomeException) → do
            logError logger CatWorld $ "Failed starting sim thread: " <> T.pack (show e)
            error "Sim thread start failure."
        )
    return $ ThreadState stateRef threadId

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
            catch
              (do
                -- Process all pending commands
                processSimCommands env logger simStateRef

                ss ← readIORef simStateRef

                if ssPaused ss ∨ HM.null (ssChunks ss)
                                ∨ ssTilesRef ss ≡ Nothing
                    then do
                        threadDelay (ssTickRate ss)
                        simLoop env stateRef simStateRef
                    else do
                        ss' ← settleNewChunks ss
                        let ss''   = simulateFluidTick ss'
                            ss'''  = simulateActiveTick ss''
                        writeDirtyFluids env ss'''
                        let ss'''' = ss''' { ssDirtyChunks = HS.empty }
                        writeIORef simStateRef ss''''

                        threadDelay (ssTickRate ss'''')
                        simLoop env stateRef simStateRef
              )
              (\(e ∷ SomeException) → do
                logError logger CatWorld $ "Sim thread crashed: " <> T.pack (show e)
                writeIORef (lifecycleRef env) CleaningUp
              )

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
        SimActivateWorld tilesRef → do
            -- Re-trigger settle so existing chunks get simulated now that writeback is possible
            let reSettled = HM.map (\scs →
                    scs { scsSettleTicks = 24 }) (ssChunks ss)
            writeIORef simStateRef $
                ss { ssTilesRef = Just tilesRef
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
                    writeIORef simStateRef $
                        ss { ssChunks = HM.insert coord activated (ssChunks ss) }

        SimSetTickRate rate →
            writeIORef simStateRef $ ss { ssTickRate = rate }

        SimPause →
            writeIORef simStateRef $ ss { ssPaused = True }

        SimResume →
            writeIORef simStateRef $ ss { ssPaused = False }

        SimFastSettleAll done → do
            -- Re-sync scsFluid from wsTilesRef BEFORE settling. The
            -- world thread's seal/strip passes modify lcFluidMap after
            -- SimChunkLoaded was sent (where scsFluid was first cached),
            -- so the cached scsFluid is stale by the time we settle.
            -- Without this resync, our writeDirtyFluids below would
            -- overwrite the seal's work with stale pre-seal data.
            ssSynced ← syncFluidsFromWorld ss
            -- Run sim ticks synchronously (no sleeping) until all
            -- chunks are settled and inactive, then write back.
            -- Capped at maxIterations as a safety net.
            let maxIterations = 500 ∷ Int
                ssSettled = fastSettleAll maxIterations ssSynced
                -- Mark every chunk as dirty so writeDirtyFluids
                -- pushes the settled state back to wsTilesRef.
                allCoords = HS.fromList (HM.keys (ssChunks ssSettled))
                ssDirty = ssSettled { ssDirtyChunks = allCoords
                                    , ssPaused = True }
            writeIORef simStateRef ssDirty
            writeDirtyFluids env ssDirty
            -- Reset dirty after writing so we don't re-write next tick
            writeIORef simStateRef $ ssDirty { ssDirtyChunks = HS.empty }
            putMVar done ()
            logDebug logger CatWorld "Sim: fast-settled and paused"

-- | Re-read fluid maps from wsTilesRef into scsFluid for every chunk.
--   The world thread's per-batch and final seal/strip passes modify
--   lcFluidMap after SimChunkLoaded was processed, so the cached
--   scsFluid in passive (non-active) chunks is stale. This sync
--   reconciles them before settling so writeDirtyFluids doesn't
--   clobber the seal's work.
--
--   Active chunks are left alone — their authoritative state lives
--   in scsActiveFluid, not lcFluidMap.
syncFluidsFromWorld ∷ SimState → IO SimState
syncFluidsFromWorld ss =
    case ssTilesRef ss of
        Nothing → return ss
        Just tilesRef → do
            wtd ← readIORef tilesRef
            let updatedChunks = HM.mapWithKey (\cc scs →
                    if scsActive scs
                    then scs
                    else case lookupChunk cc wtd of
                        Just lc → scs { scsFluid    = lcFluidMap lc
                                      , scsGenFluid = lcFluidMap lc }
                        Nothing → scs
                    ) (ssChunks ss)
            return ss { ssChunks = updatedChunks }

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

-- | Write dirty fluid maps back to the shared WorldTileData,
--   update surface maps, and invalidate render caches.
writeDirtyFluids ∷ EngineEnv → SimState → IO ()
writeDirtyFluids env ss = do
    let dirty = ssDirtyChunks ss
    case ssTilesRef ss of
        Nothing → return ()
        Just tilesRef →
            when (not (HS.null dirty)) $ do
                -- Update fluid maps and recompute surface maps
                atomicModifyIORef' tilesRef $ \wtd →
                    let wtd' = HS.foldl' (\w cc →
                            case (HM.lookup cc (ssChunks ss), lookupChunk cc w) of
                                (Just scs, Just lc) →
                                    let newFluid = if scsActive scs
                                            then deriveFluidMap scs
                                            else scsFluid scs
                                        newSurfMap = VU.imap (\idx terrZ →
                                            case newFluid V.! idx of
                                                Just fc → max terrZ (fcSurface fc)
                                                Nothing → terrZ
                                            ) (lcTerrainSurfaceMap lc)
                                        newSideDeco = scsSideDeco scs
                                        lc' = lc { lcFluidMap   = newFluid
                                                 , lcSurfaceMap = newSurfMap
                                                 , lcSideDeco   = newSideDeco
                                                 , lcModified   = True }
                                    in w { wtdChunks = HM.insert cc lc' (wtdChunks w) }
                                _ → w
                            ) wtd dirty
                    in (wtd', ())

                invalidateRenderCaches env

deriveFluidMap ∷ SimChunkState → V.Vector (Maybe FluidCell)
deriveFluidMap scs =
    let terrV = scsTerrain scs
    in V.imap (\idx mafc →
        case mafc of
            Nothing  → Nothing
            Just afc → activeToFluidCell (terrV VU.! idx) afc
        ) (scsActiveFluid scs)

invalidateRenderCaches ∷ EngineEnv → IO ()
invalidateRenderCaches env = do
    mgr ← readIORef (worldManagerRef env)
    forM_ (wmVisible mgr) $ \pageId →
        case lookup pageId (wmWorlds mgr) of
            Nothing → pure ()
            Just ws → do
                writeIORef (wsQuadCacheRef ws) Nothing
                writeIORef (wsZoomQuadCacheRef ws) Nothing
                writeIORef (wsBgQuadCacheRef ws) Nothing

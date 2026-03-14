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
import Control.Exception (SomeException, catch)
import Engine.Core.Thread (ThreadState(..), ThreadControl(..))
import Engine.Core.State (EngineEnv(..))
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

-----------------------------------------------------------
-- Start Simulation Thread
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Simulation Loop
-----------------------------------------------------------

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

                -- Don't simulate if paused, no chunks, or no active world
                if ssPaused ss ∨ HM.null (ssChunks ss)
                                ∨ ssTilesRef ss ≡ Nothing
                    then do
                        threadDelay (ssTickRate ss)
                        simLoop env stateRef simStateRef
                    else do
                        -- Run settle ticks for newly loaded chunks
                        ss' ← settleNewChunks ss

                        -- Run one fluid simulation tick (passive chunks)
                        let ss'' = simulateFluidTick ss'

                        -- Run active volume simulation for active chunks
                        let ss''' = simulateActiveTick ss''

                        -- Write dirty fluid maps back to WorldTileData
                        writeDirtyFluids env ss'''

                        -- Clear dirty set
                        let ss'''' = ss''' { ssDirtyChunks = HS.empty }
                        writeIORef simStateRef ss''''

                        threadDelay (ssTickRate ss'''')
                        simLoop env stateRef simStateRef
              )
              (\(e ∷ SomeException) → do
                logError logger CatWorld $ "Sim thread crashed: " <> T.pack (show e)
              )

-----------------------------------------------------------
-- Command Processing
-----------------------------------------------------------

processSimCommands ∷ EngineEnv → LoggerState → IORef SimState → IO ()
processSimCommands env logger simStateRef = do
    mCmd ← Q.tryReadQueue (simQueue env)
    case mCmd of
        Just cmd → do
            handleSimCommand logger simStateRef cmd
            processSimCommands env logger simStateRef
        Nothing → return ()

handleSimCommand ∷ LoggerState → IORef SimState → SimCommand → IO ()
handleSimCommand logger simStateRef cmd = do
    ss ← readIORef simStateRef
    case cmd of
        SimActivateWorld tilesRef → do
            -- Re-trigger settle on all existing chunks so they get
            -- simulated now that we can write back
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

-----------------------------------------------------------
-- Chunk Activation / Deactivation
-----------------------------------------------------------

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

-- | Deactivate a chunk that has reached equilibrium.
--   Bakes active fluid back to passive FluidCells.
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

-----------------------------------------------------------
-- Settle: fast ticks for newly loaded chunks
-----------------------------------------------------------

settleNewChunks ∷ SimState → IO SimState
settleNewChunks ss = do
    let needsSettle = HM.filter (\scs → scsSettleTicks scs > 0) (ssChunks ss)
    if HM.null needsSettle
        then return ss
        else do
            -- Run one settle tick (decrements counter each loop iteration)
            let ss' = simulateFluidTick ss
                decremented = HM.map (\scs →
                    if scsSettleTicks scs > 0
                        then scs { scsSettleTicks = scsSettleTicks scs - 1 }
                        else scs
                    ) (ssChunks ss')
            return $ ss' { ssChunks = decremented }

-----------------------------------------------------------
-- Write Results Back
-----------------------------------------------------------

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
                                    let -- For active chunks, derive FluidMap from volumes
                                        newFluid = if scsActive scs
                                            then deriveFluidMap scs
                                            else scsFluid scs
                                        -- Recompute surface map: max(terrain, fluid surface)
                                        newSurfMap = VU.imap (\idx terrZ →
                                            case newFluid V.! idx of
                                                Just fc → max terrZ (fcSurface fc)
                                                Nothing → terrZ
                                            ) (lcTerrainSurfaceMap lc)
                                        -- Write back side decos if present
                                        newSideDeco = scsSideDeco scs
                                        lc' = lc { lcFluidMap   = newFluid
                                                 , lcSurfaceMap = newSurfMap
                                                 , lcSideDeco   = newSideDeco
                                                 , lcModified   = True }
                                    in w { wtdChunks = HM.insert cc lc' (wtdChunks w) }
                                _ → w
                            ) wtd dirty
                    in (wtd', ())

                -- Invalidate render caches so changes become visible
                invalidateRenderCaches env

-- | Derive a FluidMap from active volume data.
deriveFluidMap ∷ SimChunkState → V.Vector (Maybe FluidCell)
deriveFluidMap scs =
    let terrV = scsTerrain scs
    in V.imap (\idx mafc →
        case mafc of
            Nothing  → Nothing
            Just afc → activeToFluidCell (terrV VU.! idx) afc
        ) (scsActiveFluid scs)

-- | Invalidate all render caches for visible worlds
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

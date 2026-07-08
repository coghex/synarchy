-- | Dump boot path: generate a world, load a chunk region, and dump
--   per-tile data as JSON to stdout, then exit. No TCP server, no loop.
module App.Dump
  ( runDump
  ) where

import UPrelude
import Control.Exception (displayException)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Geology.Ore (oreMaterialIds)
import World.Material (getMaterialProps, MaterialProps(..)
                      , MaterialId(..), MaterialRegistry)
import World.Geology.Ore.Types (wodByChunk)
import World.Fluid.Lake.Types (WorldLakes(..), lkArea)
import World.Fluid.River.Types (WorldRivers(..), rivFlowRate)
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (initializeEngineHeadlessWith, EngineInitResult(..))
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Types (EngineConfig(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Error.Exception (EngineException(..), ExceptionType(..)
                                   , SystemError(..), mkErrorContext
                                   , throwEngineException)
import Engine.Core.Log (LogBackend(..), shutdownLogger)
import Engine.Scripting.Lua.Message (processLuaMessages)
import Engine.Loop.Shutdown (checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import World.Types
import World.Plate (isGlacierZone, isBeyondGlacier)
import World.Weather.Types (ClimateState, initClimateState)
import World.Weather.Lookup (lookupWaterTable)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import Sim.Command.Types (SimCommand(..))
import App.Cli (DumpLayers(..))
import App.Exception (guardNativeExceptions)

-- | Run engine in dump mode: generate world, load chunks, dump tile
--   data as JSON to stdout, and exit. No TCP server, no loop.
runDump ∷ DumpLayers → Int → Int → Int → (Int, Int, Int, Int) → IO ()
runDump layers seed worldSize plateCount (cx1, cy1, cx2, cy2) = do
  hPutStrLn stderr $ "dump: seed=" ⧺ show seed
                   ⧺ " worldSize=" ⧺ show worldSize
                   ⧺ " plates=" ⧺ show plateCount
                   ⧺ " region=(" ⧺ show cx1 ⧺ ","
                   ⧺ show cy1 ⧺ "," ⧺ show cx2 ⧺ ","
                   ⧺ show cy2 ⧺ ")"

  -- Logger is born writing to stderr (not redirected after the fact),
  -- so init-time logging (e.g. loadNotificationCfg) can't pollute the
  -- JSON on stdout.
  EngineInitResult env ← initializeEngineHeadlessWith (LogToHandle stderr)

  let env' = env { engineConfig = (engineConfig env) { ecDebugPort = 0 } }

  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        liftIO $ writeIORef (lifecycleRef env') EngineRunning
        liftIO $ threadDelay 500000
        processLuaMessages

        -- Pause the sim thread BEFORE any chunks load. This prevents
        -- the sim from racing with the world thread's per-batch seal
        -- during chunk generation. The sim will be fast-settled
        -- synchronously after all chunks are loaded.
        liftIO $ Q.writeQueue (simQueue env') SimPause

        liftIO $ hPutStrLn stderr "dump: generating world..."
        liftIO $ Q.writeQueue (worldQueue env')
            (WorldInit (WorldPageId "dump")
                       (fromIntegral seed ∷ Word64)
                       worldSize plateCount)
        -- Timeout scales with worldSize (gen time grows ~ quadratic
        -- with worldSize, not with plate count). Min 300s for tiny
        -- worlds, plenty of headroom for the largest practical sizes.
        initOk ← liftIO $ waitForInit env' (max 300 (worldSize * 4))
        unless initOk $ throwEngineException $ EngineException
            (ExSystem (TimeoutError
                "dump: world generation did not complete in time"))
            "dump aborted before emitting output"
            mkErrorContext

        liftIO $ Q.writeQueue (worldQueue env')
            (WorldShow (WorldPageId "dump"))
        liftIO $ threadDelay 500000

        liftIO $ do
            manager ← readIORef (worldManagerRef env')
            case wmWorlds manager of
                ((_, ws):_) → do
                    td ← readIORef (wsTilesRef ws)
                    let coords = [ ChunkCoord x y
                                 | x ← [cx1..cx2], y ← [cy1..cy2] ]
                        needed = filter
                            (\c → isNothing (lookupChunk c td)) coords
                    atomicModifyIORef' (wsInitQueueRef ws) $ \q →
                        (q ⧺ needed, ())
                    hPutStrLn stderr $ "dump: queued "
                        ⧺ show (length needed) ⧺ " chunks"
                [] → hPutStrLn stderr "dump: no world found"

        chunksOk ← liftIO $ waitForChunks env' 300
        unless chunksOk $ throwEngineException $ EngineException
            (ExSystem (TimeoutError
                "dump: chunk load did not complete in time"))
            "dump aborted before emitting output"
            mkErrorContext

        -- Run the sim thread's settle iterations synchronously so the
        -- dump sees a stable state. The sim was paused at the start
        -- of dump mode, so this is the first time it actually
        -- simulates anything for these chunks.
        liftIO $ do
            hPutStrLn stderr "dump: fast-settling sim..."
            settleDone ← newEmptyMVar
            Q.writeQueue (simQueue env') (SimFastSettleAll settleDone)
            takeMVar settleDone
            hPutStrLn stderr "dump: sim settled"

        liftIO $ do
            manager ← readIORef (worldManagerRef env')
            case wmWorlds manager of
                ((_, ws):_) → do
                    -- Read tile data as-is after sim settle. Do NOT
                    -- apply extra post-passes — the GUI doesn't get
                    -- post-sim cleanup, so the dump must match what
                    -- the user sees. The initial post-passes already
                    -- ran during chunk loading (drainInitQueues).
                    mParams ← readIORef (wsGenParamsRef ws)
                    let climate = maybe (initClimateState worldSize)
                                     wgpClimateState mParams
                    td ← readIORef (wsTilesRef ws)
                    registry ← readIORef (materialRegistryRef env')
                    let json = dumpTilesJSON layers registry worldSize climate td cx1 cy1 cx2 cy2
                    -- Phase 1 sanity print: how many lakes did the
                    -- global flood produce, how many chunks they
                    -- touch.
                    case mParams of
                        Just p → do
                            let wl = gtWorldLakes (wgpGeoTimeline p)
                                nL = V.length (wlLakes wl)
                                nC = HM.size (wlByChunk wl)
                                totWet = V.sum (V.map lkArea (wlLakes wl))
                            hPutStrLn stderr $
                                "dump: WorldLakes lakes=" ⧺ show nL
                                ⧺ " chunks_touched=" ⧺ show nC
                                ⧺ " total_wet_tiles=" ⧺ show totWet
                            let wr = gtWorldRivers (wgpGeoTimeline p)
                                nR = V.length (wrRivers wr)
                                nRC = HM.size (wrByChunk wr)
                                peakFlow = if V.null (wrRivers wr)
                                           then 0
                                           else V.maximum
                                                 (V.map rivFlowRate
                                                        (wrRivers wr))
                            hPutStrLn stderr $
                                "dump: WorldRivers rivers=" ⧺ show nR
                                ⧺ " chunks_touched=" ⧺ show nRC
                                ⧺ " peak_flow=" ⧺ show peakFlow
                            let wod = wodByChunk
                                    (gtOreDeposits (wgpGeoTimeline p))
                                chunkVols = sortBy (comparing negate)
                                    [ sum (map snd es) | es ← HM.elems wod ]
                                oreVol = sum chunkVols
                                pick i = case drop i chunkVols of
                                    (v:_) → v
                                    []    → 0
                            hPutStrLn stderr $
                                "dump: OreDeposits chunks_touched="
                                ⧺ show (HM.size wod)
                                ⧺ " total_volume=" ⧺ show oreVol
                                ⧺ " max_chunk=" ⧺ show (pick 0)
                                ⧺ " p90_chunk="
                                ⧺ show (pick (length chunkVols `div` 10))
                                ⧺ " median_chunk="
                                ⧺ show (pick (length chunkVols `div` 2))
                        Nothing → pure ()
                    BS.putStr json
                    hFlush stdout
                    hPutStrLn stderr $ "dump: done"
                [] → hPutStrLn stderr "dump: no world data"

        liftIO $ writeIORef (lifecycleRef env') CleaningUp
        liftIO $ shutdownThread combatThreadState
        liftIO $ shutdownThread simThreadState
        liftIO $ shutdownThread unitThreadState
        liftIO $ shutdownThread worldThreadState
        liftIO $ shutdownThread luaThreadState
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  case result of
    Left err → do
        -- Errors go to stderr so a failed dump never pollutes the JSON
        -- stdout channel with success-shaped output.
        hPutStrLn stderr $ displayException err
        shutdownThread combatThreadState
        shutdownThread simThreadState
        shutdownThread unitThreadState
        shutdownThread worldThreadState
        shutdownThread luaThreadState
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
        logger ← readIORef (loggerRef env')
        shutdownLogger logger
        exitFailure
    Right _ → pure ()

-- | Poll until world generation is done. The argument is a timeout in
--   /seconds/; internally we poll every 250ms (4 iterations per second).
--   Returns 'True' on completion, 'False' on timeout so the caller can
--   fail the dump rather than emit partial output.
waitForInit ∷ EngineEnv → Int → IO Bool
waitForInit env seconds = go (seconds * pollsPerSecond)
  where
    -- Check readiness BEFORE deciding to time out, so a completion that
    -- lands in the final poll window (after the last sleep) is still
    -- counted as success rather than a spurious timeout.
    go n = do
        manager ← readIORef (worldManagerRef env)
        done ← case wmWorlds manager of
            ((_, ws):_) → do
                phase ← readIORef (wsLoadPhaseRef ws)
                pure $ case phase of
                    LoadDone → True
                    _        → False
            [] → pure False
        if done
            then hPutStrLn stderr "dump: init complete" >> pure True
            else if n ≤ 0
                then hPutStrLn stderr "dump: init timeout" >> pure False
                else threadDelay pollInterval >> go (n - 1)

-- | Poll until chunk init queue is empty. The argument is a timeout in
--   /seconds/; internally we poll every 250ms (4 iterations per second).
--   Returns 'True' once the queue drains, 'False' on timeout so the
--   caller can fail the dump rather than emit partial output.
waitForChunks ∷ EngineEnv → Int → IO Bool
waitForChunks env seconds = go (seconds * pollsPerSecond)
  where
    -- Check readiness BEFORE deciding to time out (see 'waitForInit').
    go n = do
        manager ← readIORef (worldManagerRef env)
        done ← case wmWorlds manager of
            ((_, ws):_) → do
                remaining ← length <$> readIORef (wsInitQueueRef ws)
                pure (remaining ≡ 0)
            [] → pure False
        if done
            then hPutStrLn stderr "dump: all chunks loaded" >> pure True
            else if n ≤ 0
                then hPutStrLn stderr "dump: chunk load timeout"
                        >> pure False
                else threadDelay pollInterval >> go (n - 1)

-- | Poll cadence for the dump-mode wait helpers: a 250ms sleep between
--   checks, so four polls make up one second of timeout budget.
pollInterval ∷ Int
pollInterval = 250000

pollsPerSecond ∷ Int
pollsPerSecond = 1000000 `div` pollInterval

-- | Dump per-tile data in a chunk region as JSON.
--   Every tile in the region gets one object. Fields are included
--   based on the DumpLayers whitelist.
dumpTilesJSON ∷ DumpLayers → MaterialRegistry → Int → ClimateState → WorldTileData
              → Int → Int → Int → Int → BS.ByteString
dumpTilesJSON layers registry worldSize climate td cx1 cy1 cx2 cy2 =
    let entries = concatMap dumpChunkTiles
            [ ChunkCoord x y | x ← [cx1..cx2], y ← [cy1..cy2] ]
    in BS.pack $ "[" ⧺ intercalate "," entries ⧺ "]\n"
  where
    dumpChunkTiles coord = case lookupChunk coord td of
        Nothing → []
        Just lc →
            let ChunkCoord cx cy = coord
                gx0 = cx * chunkSize
                gy0 = cy * chunkSize
            in [ tileToJSON lc (gx0 + lx) (gy0 + ly) idx
               | ly ← [0..chunkSize-1]
               , lx ← [0..chunkSize-1]
               , let idx = ly * chunkSize + lx
               ]

    tileToJSON lc gx gy idx =
        let v = gx + gy
            base = "{\"x\":" ⧺ show gx ⧺ ",\"y\":" ⧺ show gy
                 ⧺ ",\"v\":" ⧺ show v
            terrainZ = lcTerrainSurfaceMap lc VU.! idx
            surfZ    = lcSurfaceMap lc VU.! idx
            waterTableZ = lcWaterTableMap lc VU.! idx
            (wtSummer, wtWinter) = lookupWaterTable climate worldSize gx gy
            terrainFields
              | dlTerrain layers =
                  ",\"terrainZ\":" ⧺ show terrainZ
                  ⧺ ",\"surfaceZ\":" ⧺ show surfZ
                  ⧺ ",\"waterTableZ\":" ⧺ show waterTableZ
                  ⧺ ",\"waterTableSummer\":" ⧺ show wtSummer
                  ⧺ ",\"waterTableWinter\":" ⧺ show wtWinter
              | otherwise = ""
            matFields
              | dlMaterial layers =
                  let col = lcTiles lc V.! idx
                      matId = if VU.null (ctMats col) then 0
                              else ctMats col VU.! (VU.length (ctMats col) - 1)
                  in ",\"matId\":" ⧺ show matId
              | otherwise = ""
            fluidFields
              | dlFluid layers =
                  case lcFluidMap lc V.! idx of
                      Just fc → ",\"fluidType\":\"" ⧺ fluidTypeStr (fcType fc) ⧺ "\""
                              ⧺ ",\"fluidSurf\":" ⧺ show (fcSurface fc)
                      Nothing → ",\"fluidType\":null,\"fluidSurf\":null"
              | otherwise = ""
            iceFields
              | dlIce layers = case lcIceMap lc V.! idx of
                  Just ic → ",\"iceSurf\":" ⧺ show (icSurface ic)
                          ⧺ ",\"iceMode\":\"" ⧺ iceModeStr (icMode ic) ⧺ "\""
                  Nothing → ",\"iceSurf\":null,\"iceMode\":null"
              | otherwise = ""
            -- Ore layer: scan the column's strata for ore materials.
            -- Reports the topmost ore band: its material, its top z,
            -- and how many cells of that material the column holds.
            -- Only covers the stored strata range (ctStartZ up) —
            -- which is the mineable band the report tool cares about.
            oreFields
              | dlOre layers =
                  let col = lcTiles lc V.! idx
                      mats = ctMats col
                      isOre m = m `elem` oreMaterialIds
                      topOreIdx = let go i | i < 0 = (-1)
                                           | isOre (mats VU.! i) = i
                                           | otherwise = go (i - 1)
                                  in go (VU.length mats - 1)
                  in if topOreIdx < 0
                     then ",\"oreId\":null,\"oreTopZ\":null,\"oreCount\":0"
                     else let oid = mats VU.! topOreIdx
                              cnt = VU.length (VU.filter (≡ oid) mats)
                          in ",\"oreId\":" ⧺ show oid
                           ⧺ ",\"oreTopZ\":" ⧺ show (ctStartZ col + topOreIdx)
                           ⧺ ",\"oreCount\":" ⧺ show cnt
              | otherwise = ""
            -- Slope layer: the rendered slope bitmask of the surface
            -- tile (bit0=N,1=E,2=S,3=W; 0 = flat top). Lets headless
            -- tools measure how often terrain slopes vs. steps (#224).
            -- NB: index by terrainZ, not surfZ. The strata vectors
            -- (ctSlopes/ctMats) are keyed to the TERRAIN surface; surfZ is
            -- max(terrain, fluid), so on submerged tiles surfZ overshoots
            -- the stored range and would report a spurious flat/empty bed.
            -- terrainZ gives the real bed slope + bed material everywhere.
            slopeFields
              | dlSlope layers =
                  let col = lcTiles lc V.! idx
                      i   = terrainZ - ctStartZ col
                      sl  = if i ≥ 0 ∧ i < VU.length (ctSlopes col)
                            then ctSlopes col VU.! i else 0
                      smat = if i ≥ 0 ∧ i < VU.length (ctMats col)
                             then ctMats col VU.! i else 0
                      hard = mpHardness (getMaterialProps registry (MaterialId smat))
                  in ",\"slope\":" ⧺ show sl
                   ⧺ ",\"hardness\":" ⧺ show hard
              | otherwise = ""
            zoneFields =
                  ",\"glacierZone\":" ⧺ boolStr (isGlacierZone worldSize gx gy)
                ⧺ ",\"beyondGlacier\":" ⧺ boolStr (isBeyondGlacier worldSize gx gy)
        in base ⧺ terrainFields ⧺ matFields ⧺ fluidFields
                ⧺ iceFields ⧺ oreFields ⧺ slopeFields ⧺ zoneFields ⧺ "}"

    fluidTypeStr Ocean = "ocean"
    fluidTypeStr Lake  = "lake"
    fluidTypeStr River = "river"
    fluidTypeStr Lava  = "lava"
    iceModeStr BasinIce = "basin"
    iceModeStr DrapeIce = "drape"
    boolStr True  = "true"
    boolStr False = "false"

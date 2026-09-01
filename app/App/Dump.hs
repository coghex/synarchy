-- | Dump boot path: generate a world, load a chunk region, and dump
--   per-tile data as JSON to stdout, then exit. No TCP server, no loop.
module App.Dump
  ( DumpGenParams(..)
  , runDump
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.IORef (readIORef, writeIORef)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Aeson (Value(..), (.=), object, encode)
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy.Char8 as BSL
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
import Control.Monad.Error.Class (MonadError(..))
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Types (BootProfile(..), BootMode(..))
import Engine.Core.Error.Exception (EngineException(..), ExceptionType(..)
                                   , SystemError(..), mkErrorContext)
import Engine.Core.Log (LogBackend(..), shutdownLogger)
import Engine.Scripting.Lua.Message (processLuaMessages)
import Engine.Loop.Shutdown (checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import World.Types
import World.Chunk.Queue (enqueueChunkRequest)
import World.Plate (isGlacierZone, isBeyondGlacier)
import World.Weather.Types (ClimateState, initClimateState)
import World.Weather.Lookup (lookupWaterTable)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import Sim.Command.Types (SimCommand(..))
import App.Cli (DumpLayers(..), ChunkRegion(..), chunkRegionCoords)
import Engine.Core.Workers (EngineWorkers(..), shutdownEngineWorkers)
import App.Boot (FatalStream(..), bootConfig, handleBootResult
                , luaThreadOrAbort)
import App.Exception (guardNativeExceptions)

-- | The three world-generation values a dump is given, named so that
--   exchanging two of them at the call site is a compile error rather
--   than a different world reported under the swapped labels (#1081).
--
--   Deliberately NOT 'World.Generate.Config.Types.WorldGenConfig',
--   whose @wgcSeed@ is a @Maybe Word64@: the dump's seed is a concrete
--   'Int' (so @--seed -1@ keeps parsing and meaning what it always
--   did), and the widening to 'Word64' stays where it has always been,
--   at the 'WorldInit' message below. Only the @wgc*@ naming idiom is
--   borrowed — per-scalar newtypes exist nowhere in this tree.
data DumpGenParams = DumpGenParams
    { dgpSeed       ∷ !Int
    , dgpWorldSize  ∷ !Int
      -- ^ Already normalized by 'Main' ('normalizeWorldSize').
    , dgpPlateCount ∷ !Int
      -- ^ Already normalized by 'Main' ('normalizePlateCount').
    } deriving (Eq, Show)

-- | Run engine in dump mode: generate world, load chunks, dump tile
--   data as JSON to stdout, and exit. No TCP server, no loop.
runDump ∷ DumpLayers → DumpGenParams → ChunkRegion → IO ()
runDump layers gen region = do
  let seed       = dgpSeed gen
      worldSize  = dgpWorldSize gen
      plateCount = dgpPlateCount gen
  hPutStrLn stderr $ "dump: seed=" ⧺ show seed
                   ⧺ " worldSize=" ⧺ show worldSize
                   ⧺ " plates=" ⧺ show plateCount
                   ⧺ " region=(" ⧺ show (crX1 region) ⧺ ","
                   ⧺ show (crY1 region) ⧺ "," ⧺ show (crX2 region) ⧺ ","
                   ⧺ show (crY2 region) ⧺ ")"

  -- Logger is born writing to stderr (not redirected after the fact),
  -- so init-time logging (e.g. loadNotificationCfg) can't pollute the
  -- JSON on stdout.
  EngineInitResult env ← initializeEngineHeadlessWith (LogToHandle stderr)

  -- Port 0 is dump's own contract, not a CLI default: it tells
  -- startDebugServer to open no TCP listener at all.
  let env' = bootConfig ModeDump BootNormal (Just 0) env

  -- 'ModeDump' is what keeps issue #46's port-0 sentinel meaning "no
  -- TCP listener" here while the same 0 is refused in the two modes
  -- that depend on one (#1190); dump therefore never takes the Left
  -- branch, and starts no worker before Lua to tear down if it did.
  luaThreadState   ← startLuaThread env' ⌦ luaThreadOrAbort env' []
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  -- Dump, like headless, starts no input thread.
  let workers = EngineWorkers
        { ewCombat = Just combatThreadState
        , ewSim    = Just simThreadState
        , ewUnit   = Just unitThreadState
        , ewWorld  = Just worldThreadState
        , ewInput  = Nothing
        , ewLua    = Just luaThreadState
        }

  let engineAction ∷ EngineM' ()
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
                       worldSize plateCount Nothing)
        -- Timeout scales with worldSize (gen time grows ~ quadratic
        -- with worldSize, not with plate count). Min 300s for tiny
        -- worlds, plenty of headroom for the largest practical sizes.
        initOk ← liftIO $ waitForInit env' (max 300 (worldSize * 4))
        unless initOk $ throwError $ EngineException
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
                ((pid, ws):_) → do
                    -- Physical chunks, not coordinate spellings: a
                    -- seam-crossing --region names one chunk twice, and
                    -- the reported count is what waitForChunks below is
                    -- actually waiting for (#1723). The page id qualifies
                    -- the canonical key the demand registers under
                    -- (#2001).
                    queued ← enqueueChunkRequest pid ws $
                        map (uncurry ChunkCoord) (chunkRegionCoords region)
                    hPutStrLn stderr $ "dump: queued "
                        ⧺ show queued ⧺ " chunks"
                [] → hPutStrLn stderr "dump: no world found"

        chunksOk ← liftIO $ waitForChunks env' 300
        unless chunksOk $ throwError $ EngineException
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
                    let json = dumpTilesJSON layers registry worldSize
                                             climate td region
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
                    BSL.putStr json
                    hFlush stdout
                    hPutStrLn stderr $ "dump: done"
                [] → hPutStrLn stderr "dump: no world data"

        liftIO $ writeIORef (lifecycleRef env') CleaningUp
        liftIO $ shutdownEngineWorkers workers
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped

  -- FatalToStderr so a failed dump never pollutes the JSON stdout
  -- channel with success-shaped output.
  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  handleBootResult FatalToStderr env' workers result

-- | The one polling loop behind every dump-mode wait. The timeout is
--   given in /seconds/; internally we poll every 250ms (4 iterations
--   per second). Returns 'True' on completion, 'False' on timeout so
--   the caller can fail the dump rather than emit partial output.
--
--   Readiness is checked BEFORE deciding to time out, so a completion
--   that lands in the final poll window (after the last sleep) is still
--   counted as success rather than a spurious timeout. Every wait below
--   inherits that ordering from here.
pollUntil ∷ String → String → Int → IO Bool → IO Bool
pollUntil doneMsg timeoutMsg seconds isReady = go (seconds * pollsPerSecond)
  where
    go n = do
        done ← isReady
        if done
            then hPutStrLn stderr doneMsg >> pure True
            else if n ≤ 0
                then hPutStrLn stderr timeoutMsg >> pure False
                else threadDelay pollInterval >> go (n - 1)

-- | Run a readiness check against the dump's world page. Not-yet-ready
--   while the world manager still holds no page.
worldReady ∷ EngineEnv → (WorldState → IO Bool) → IO Bool
worldReady env check = do
    manager ← readIORef (worldManagerRef env)
    case wmWorlds manager of
        ((_, ws):_) → check ws
        []          → pure False

-- | Poll until world generation is done.
waitForInit ∷ EngineEnv → Int → IO Bool
waitForInit env seconds =
    pollUntil "dump: init complete" "dump: init timeout" seconds $
        worldReady env $ \ws → do
            phase ← readIORef (wsLoadPhaseRef ws)
            pure $ case phase of
                LoadDone → True
                _        → False

-- | Poll until the chunk init queue is empty.
waitForChunks ∷ EngineEnv → Int → IO Bool
waitForChunks env seconds =
    pollUntil "dump: all chunks loaded" "dump: chunk load timeout" seconds $
        worldReady env $ \ws → do
            remaining ← length <$> readIORef (wsInitQueueRef ws)
            pure (remaining ≡ 0)

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
              → ChunkRegion → BSL.ByteString
dumpTilesJSON layers registry worldSize climate td region =
    let entries = concatMap dumpChunkTiles
            (map (uncurry ChunkCoord) (chunkRegionCoords region))
    in encode entries <> "\n"
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
            base ∷ [Pair]
            base = [ "x" .= gx, "y" .= gy, "v" .= v ]
            terrainZ = lcTerrainSurfaceMap lc VU.! idx
            surfZ    = lcSurfaceMap lc VU.! idx
            waterTableZ = lcWaterTableMap lc VU.! idx
            (wtSummer, wtWinter) = lookupWaterTable climate worldSize gx gy
            terrainFields ∷ [Pair]
            terrainFields
              | dlTerrain layers =
                  [ "terrainZ" .= terrainZ
                  , "surfaceZ" .= surfZ
                  , "waterTableZ" .= waterTableZ
                  , "waterTableSummer" .= wtSummer
                  , "waterTableWinter" .= wtWinter
                  ]
              | otherwise = []
            matFields ∷ [Pair]
            matFields
              | dlMaterial layers =
                  let col = lcTiles lc V.! idx
                      matId = if VU.null (ctMats col) then 0
                              else ctMats col VU.! (VU.length (ctMats col) - 1)
                  in [ "matId" .= matId ]
              | otherwise = []
            -- Enum labels are plain values handed to the encoder, which
            -- quotes and escapes them — nothing here writes JSON text.
            fluidFields ∷ [Pair]
            fluidFields
              | dlFluid layers =
                  case lcFluidMap lc V.! idx of
                      Just fc →
                          let ftype = case fcType fc of
                                  Ocean → "ocean"
                                  Lake  → "lake"
                                  River → "river"
                                  Lava  → "lava"
                          in [ "fluidType" .= (ftype ∷ Text)
                             , "fluidSurf" .= fcSurface fc ]
                      Nothing → [ "fluidType" .= Null, "fluidSurf" .= Null ]
              | otherwise = []
            iceFields ∷ [Pair]
            iceFields
              | dlIce layers = case lcIceMap lc V.! idx of
                  Just ic →
                      let imode = case icMode ic of
                              BasinIce → "basin"
                              DrapeIce → "drape"
                      in [ "iceSurf" .= icSurface ic
                         , "iceMode" .= (imode ∷ Text) ]
                  Nothing → [ "iceSurf" .= Null, "iceMode" .= Null ]
              | otherwise = []
            -- Ore layer: scan the column's strata for ore materials.
            -- Reports the topmost ore band: its material, its top z,
            -- and how many cells of that material the column holds.
            -- Only covers the stored strata range (ctStartZ up) —
            -- which is the mineable band the report tool cares about.
            oreFields ∷ [Pair]
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
                     then [ "oreId" .= Null, "oreTopZ" .= Null
                          , "oreCount" .= (0 ∷ Int) ]
                     else let oid = mats VU.! topOreIdx
                              cnt = VU.length (VU.filter (≡ oid) mats)
                          in [ "oreId" .= oid
                             , "oreTopZ" .= (ctStartZ col + topOreIdx)
                             , "oreCount" .= cnt ]
              | otherwise = []
            -- Slope layer: the rendered slope bitmask of the surface
            -- tile (bit0=N,1=E,2=S,3=W; 0 = flat top). Lets headless
            -- tools measure how often terrain slopes vs. steps (#224).
            -- NB: index by terrainZ, not surfZ. The strata vectors
            -- (ctSlopes/ctMats) are keyed to the TERRAIN surface; surfZ is
            -- max(terrain, fluid), so on submerged tiles surfZ overshoots
            -- the stored range and would report a spurious flat/empty bed.
            -- terrainZ gives the real bed slope + bed material everywhere.
            slopeFields ∷ [Pair]
            slopeFields
              | dlSlope layers =
                  let col = lcTiles lc V.! idx
                      i   = terrainZ - ctStartZ col
                      sl  = if i ≥ 0 ∧ i < VU.length (ctSlopes col)
                            then ctSlopes col VU.! i else 0
                      smat = if i ≥ 0 ∧ i < VU.length (ctMats col)
                             then ctMats col VU.! i else 0
                      hard = mpHardness (getMaterialProps registry (MaterialId smat))
                  in [ "slope" .= sl, "hardness" .= hard ]
              | otherwise = []
            zoneFields ∷ [Pair]
            zoneFields =
                [ "glacierZone" .= isGlacierZone worldSize gx gy
                , "beyondGlacier" .= isBeyondGlacier worldSize gx gy ]
        in object $ base ⧺ terrainFields ⧺ matFields ⧺ fluidFields
                 ⧺ iceFields ⧺ oreFields ⧺ slopeFields ⧺ zoneFields

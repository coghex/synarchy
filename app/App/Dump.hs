-- | Dump boot path: generate a world, load a chunk region, and dump
--   per-tile data as JSON to stdout, then exit. No TCP server, no loop.
module App.Dump
  ( DumpGenParams(..)
  , runDump
    -- * The fast-settle wait (#2334)
  , SettleWatch(..)
  , SettleWaitResult(..)
  , classifySettleWait
  , awaitFastSettle
  , settleWaitFailure
  , fastSettleBudgetSeconds
  , settleReportGraceSeconds
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryReadMVar)
import Data.IORef (IORef, readIORef, writeIORef)
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
import Engine.Core.Clock (monotonicSeconds)
import Engine.Core.Error.Exception (EngineException(..), ExceptionType(..)
                                   , SystemError(..), mkErrorContext)
import Engine.Core.Thread (ThreadState(..))
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
import Sim.Command.Types (SimCommand(..), FastSettleRequest(..)
                         , FastSettleOutcome(..))
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
        settleResult ← liftIO $ do
            hPutStrLn stderr "dump: fast-settling sim..."
            settleDone ← newEmptyMVar
            simDeadline ← (+ fastSettleBudgetSeconds) ⊚ monotonicSeconds
            Q.writeQueue (simQueue env')
                (SimFastSettleAll (FastSettleRequest settleDone simDeadline))
            -- The sim owns the inner deadline; this one is deliberately
            -- the same instant plus a grace, so a sim that is still
            -- alive gets to name WHICH world stalled before this wait
            -- falls back to a bare timeout.
            awaitFastSettle monotonicSeconds (lifecycleRef env')
                SettleWatch { swSim   = tsDone ⊚ ewSim workers
                            , swWorld = tsDone ⊚ ewWorld workers }
                settleDone (simDeadline + settleReportGraceSeconds)
        case settleWaitFailure settleResult of
            Just why → throwError $ EngineException (ExSystem why)
                "dump aborted before emitting output"
                mkErrorContext
            Nothing → liftIO $ hPutStrLn stderr "dump: sim settled"

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

-- | The polling loop behind the dump's two boolean waits, 'waitForInit'
--   and 'waitForChunks'. The timeout is given in /seconds/; internally
--   we poll every 250ms (4 iterations per second). Returns 'True' on
--   completion, 'False' on timeout so the caller can fail the dump
--   rather than emit partial output.
--
--   Readiness is checked BEFORE deciding to time out, so a completion
--   that lands in the final poll window (after the last sleep) is still
--   counted as success rather than a spurious timeout. Both waits below
--   inherit that ordering from here, and 'awaitFastSettle' — which needs
--   an outcome rather than a boolean, and so runs its own loop at the
--   same 'pollInterval' — restates it deliberately (#2334).
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

-- | The two worker exit signals the fast-settle wait watches (#2334).
--
--   A record rather than a pair, for the reason 'DumpGenParams' is one
--   (#1081): exchanging the two at the call site would type-check and
--   then report a world crash as a sim crash for the rest of the
--   project's life.
--
--   Each is that worker's 'Engine.Core.Thread.tsDone', which the fork
--   finalizer fills on ANY exit including a crash, and which is READ
--   never taken — so a settle wait observing it does not consume the
--   evidence a later shutdown depends on. 'Nothing' is a mode that
--   never started that worker; dump starts both, so both are 'Just'
--   there.
data SettleWatch = SettleWatch
    { swSim   ∷ !(Maybe (MVar ()))
    , swWorld ∷ !(Maybe (MVar ()))
    }

-- | How the dump's fast-settle wait ended (#2334). Exactly one
--   constructor means the settle landed; see 'settleWaitFailure'.
data SettleWaitResult
    = SettleSettled
      -- ^ The sim published 'FastSettleApplied' and nothing else was
      --   showing: every requested writeback reached world-thread
      --   application.
    | SettleReported !FastSettleOutcome
      -- ^ The sim published a FAILING outcome, which names the world
      --   whose acknowledgement failed or never arrived.
    | SettleWorldExited
      -- ^ The world worker's 'tsDone' filled: it left before the settle
      --   completed, so no further writeback can ever be applied.
    | SettleSimExited
      -- ^ The sim worker's 'tsDone' filled without publishing.
    | SettleCleaningUp
      -- ^ The engine lifecycle left its running states with no worker
      --   'tsDone' to attribute it to.
    | SettleTimedOut
      -- ^ The deadline passed with nothing else observable.
    deriving (Eq, Show)

-- | The fast-settle wait's whole decision, kept apart from the polling
--   so the precedence is testable without timing (#2334).
--
--   Precedence: any observable FAILURE beats a published success. The
--   sim fills its completion and the world worker can fail-stop in the
--   same instant, and a dump that read the success would emit
--   partial-success JSON derived from tiles a dead world thread had
--   stopped writing. So success is returned only when nothing else is
--   showing.
--
--   Provenance where it exists, and only there. A filled sim or world
--   'tsDone' names the worker that exited; 'CleaningUp' names nothing,
--   because 'Engine.Core.State.EngineLifecycle' is a four-constructor
--   enum that carries no cause — so a lifecycle-only observation is
--   reported generically rather than guessed at and mislabelled.
--
--   The world is checked before the sim because a world worker that
--   left is the cause the sim's own strand is a symptom of. The
--   deadline is checked LAST, so a completion landing inside the final
--   poll window is still counted — the same readiness-before-timeout
--   ordering 'pollUntil' documents.
--
--   'Nothing' means nothing is decidable yet: keep waiting.
classifySettleWait
    ∷ Maybe FastSettleOutcome  -- ^ the completion, once published
    → Bool                     -- ^ the world worker has exited
    → Bool                     -- ^ the sim worker has exited
    → Bool                     -- ^ the lifecycle has left its running states
    → Bool                     -- ^ the deadline has passed
    → Maybe SettleWaitResult
classifySettleWait mOutcome worldExited simExited cleaning expired
    | Just outcome ← mOutcome
    , outcome ≢ FastSettleApplied       = Just (SettleReported outcome)
    | worldExited                       = Just SettleWorldExited
    | simExited                         = Just SettleSimExited
    | cleaning                          = Just SettleCleaningUp
    | mOutcome ≡ Just FastSettleApplied = Just SettleSettled
    | expired                           = Just SettleTimedOut
    | otherwise                         = Nothing

-- | Wait for the sim's fast settle: bounded, and watching the two
--   workers whose death is the only other way it can end (#2334).
--
--   Polls at 'pollInterval', the cadence every other dump wait uses.
--   The deadline is an absolute monotonic instant so the loop cannot
--   drift, and the clock is a parameter so a spec can drive the whole
--   decision without sleeping.
awaitFastSettle
    ∷ IO Double                 -- ^ monotonic clock
    → IORef EngineLifecycle
    → SettleWatch
    → MVar FastSettleOutcome    -- ^ the settle's completion
    → Double                    -- ^ absolute monotonic deadline
    → IO SettleWaitResult
awaitFastSettle clock lifecycle watch done deadline = go
  where
    go = do
        mOutcome ← tryReadMVar done
        worldExited ← hasExited (swWorld watch)
        simExited ← hasExited (swSim watch)
        cleaning ← leftRunning ⊚ readIORef lifecycle
        now ← clock
        case classifySettleWait mOutcome worldExited simExited cleaning
                                (now ≥ deadline) of
            Just result → pure result
            Nothing     → threadDelay pollInterval >> go
    -- A worker the mode never started cannot have exited.
    hasExited = maybe (pure False) (fmap isJust ∘ tryReadMVar)
    leftRunning lc = lc ≡ CleaningUp ∨ lc ≡ EngineStopped

-- | The 'SystemError' a settle result fails the dump with, or 'Nothing'
--   for the one result that means the settle actually landed (#2334).
--
--   Every failure travels the same route the init and chunk timeouts
--   already take: an 'EngineException' out of the engine action, caught
--   by @handleBootResult FatalToStderr@, which is what turns it into a
--   nonzero exit and a stderr diagnostic with no JSON on stdout.
settleWaitFailure ∷ SettleWaitResult → Maybe SystemError
settleWaitFailure result = case result of
    SettleSettled → Nothing
    SettleReported outcome → Just ∘ IOError $
        "dump: sim fast settle failed: " <> tshow outcome
    SettleWorldExited → Just $ IOError
        "dump: world thread exited before the sim fast settle completed"
    SettleSimExited → Just $ IOError
        "dump: sim thread exited before the sim fast settle completed"
    SettleCleaningUp → Just $ IOError
        "dump: engine began shutting down before the sim fast settle \
        \completed"
    SettleTimedOut → Just $ TimeoutError
        "dump: sim fast settle did not complete in time"

-- | The fast settle's budget, in seconds.
--
--   Generous on purpose: unlike the init and chunk waits, this one
--   covers COMPUTE as well as a handoff — the sim runs up to
--   'Sim.Thread.maxFastSettleIterations' synchronous fluid ticks over
--   every loaded chunk before it emits a single batch. The number that
--   matters is the other end: a strand used to hold a CI job to its
--   90-minute cap (@.github\/workflows\/ci.yml@) and an operator's shell
--   indefinitely, so any bound far below that is the whole win, and one
--   an order of magnitude above the settles actually observed cannot
--   cost a healthy dump its output.
fastSettleBudgetSeconds ∷ Double
fastSettleBudgetSeconds = 600

-- | How long the dump waits past the sim's own deadline before giving
--   up on it.
--
--   The sim and the dump share one instant, so without this they expire
--   together and the race decides which diagnostic the operator gets.
--   The grace resolves it in favour of the specific one: a live sim
--   names the world whose acknowledgement stalled, and only a sim that
--   cannot report at all leaves this wait to say so generically.
settleReportGraceSeconds ∷ Double
settleReportGraceSeconds = 5

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

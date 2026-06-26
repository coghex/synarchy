{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException, fromException, throwIO, catch
                         , SomeException)
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Data.Char (toLower)
import System.Environment (setEnv, getArgs)
import System.Exit (exitFailure, ExitCode)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Data.List (intercalate, isPrefixOf, sortBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Generate.Types (WorldGenParams(..))
import World.Generate.Config (minimumWorldSize, normalizeWorldSize
                             , normalizePlateCount)
import World.Geology.Ore (oreMaterialIds)
import World.Geology.Ore.Types (wodByChunk)
import World.Geology.Timeline.Types (GeoTimeline(..))
import World.Fluid.Lake.Types (WorldLakes(..), lkArea)
import World.Fluid.River.Types (WorldRivers(..), rivFlowRate)
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (initializeEngine, initializeEngineHeadless
                        , initializeEngineHeadlessWith, EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..)
                         , graphicsState, glfwWindow)
import Engine.Core.Types (EngineConfig(..), BootProfile(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Error.Exception (EngineException(..), ExceptionType(..)
                                   , SystemError(..), mkErrorContext
                                   , throwEngineException)
import qualified Data.Text as T
import Engine.Core.Log (LogCategory(..), LoggerState(..), LogBackend(..)
                       , shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Headless (headlessLoop)
import Engine.Scripting.Lua.Message (processLuaMessages)
import Engine.Loop.Shutdown (shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Thread (startLuaThread)
import World.Thread (startWorldThread)
import World.Types
import World.Plate (isGlacierZone, isBeyondGlacier, defaultPlatesFor)
import World.Weather.Types (ClimateState, initClimateState)
import World.Weather.Lookup (lookupWaterTable)
import Unit.Thread (startUnitThread)
import Combat.Thread (startCombatThread)
import Sim.Thread (startSimThread)
import Sim.Command.Types (SimCommand(..))
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)

-- | Which layers to include in dump output.
data DumpLayers = DumpLayers
    { dlTerrain  ∷ !Bool
    , dlMaterial ∷ !Bool
    , dlFluid    ∷ !Bool
    , dlIce      ∷ !Bool
    , dlOre      ∷ !Bool
    } deriving (Show)

-- | All layers enabled (default when --dump has no =value).
allLayers ∷ DumpLayers
allLayers = DumpLayers True True True True True

-- | Parse --dump or --dump=layer1,layer2,... from args.
--   Returns Nothing if --dump not present, Just layers otherwise.
parseDump ∷ [String] → Maybe DumpLayers
parseDump [] = Nothing
parseDump (a:rest)
    | a ≡ "--dump" = Just allLayers
    | "--dump=" `isPrefixOf` a =
        let flags = map (map toLower) $ splitOn ',' (drop 7 a)
        in Just DumpLayers
            { dlTerrain  = "terrain"  `elem` flags ∨ "elevation" `elem` flags
            , dlMaterial = "material" `elem` flags
            , dlFluid    = "fluid"    `elem` flags
            , dlIce      = "ice"      `elem` flags
            , dlOre      = "ore"      `elem` flags
            }
    | otherwise = parseDump rest

main ∷ IO ()
main = do
  setEnv "NSLog_Disabled" "YES"
  setEnv "MVK_CONFIG_USE_METAL_ARGUMENT_BUFFERS" "2"
#ifdef DEVELOPMENT
  setEnv "VK_LOADER_DEBUG" "none"
  setEnv "VK_LOADER_MESSAGE_LEVEL" "error"
  setEnv "VK_LOADER_LOG_LEVEL" "0"
#endif

  args ← getArgs
  let headless = "--headless" `elem` args
      bootProfile = if "--arena" `elem` args then BootArena else BootNormal
      mDump    = parseDump args
      port = parseArg "--port" args
      seed = parseArg "--seed" args
      worldSz = parseArg "--worldSize" args
      -- `--plates` is the canonical flag; `--ages` is a legacy alias
      -- (its original name was misleading — the value is the plate
      -- count, not number of geological ages, which is rolled
      -- randomly inside buildTimeline).
      plates  = parseArg "--plates" args
      agesLeg = parseArg "--ages" args
      region = parseRegion args
      rawWorldSize = fromMaybe 256 worldSz
      worldSize = normalizeWorldSize rawWorldSize
      rawPlateCount = case plates of
          Just p  → p
          Nothing → fromMaybe (defaultPlatesFor worldSize) agesLeg
      plateCount = normalizePlateCount rawPlateCount

  when (isJust mDump ∧ worldSize /= rawWorldSize) $
    hPutStrLn stderr $ "worldSize " ⧺ show rawWorldSize
        ⧺ " normalized to " ⧺ show worldSize
        ⧺ " (minimum/multiple " ⧺ show minimumWorldSize ⧺ ")."
  when (isJust mDump ∧ plateCount /= rawPlateCount) $
    hPutStrLn stderr $ "plateCount " ⧺ show rawPlateCount
        ⧺ " normalized to " ⧺ show plateCount
        ⧺ " (minimum 1)."

  case mDump of
    Just layers → runDump layers (fromMaybe 42 seed) worldSize
                                 plateCount region
    Nothing
      | headless  → runHeadless bootProfile (Just (fromMaybe 8008 port))
      | otherwise → runGraphical bootProfile (Just (fromMaybe 8008 port))

-- | Parse --flag N from args
parseArg ∷ Read a ⇒ String → [String] → Maybe a
parseArg _ [] = Nothing
parseArg flag (f:n:rest)
    | f ≡ flag  = case reads n of
        [(v, "")] → Just v
        _         → parseArg flag rest
    | otherwise = parseArg flag (n:rest)
parseArg _ [_] = Nothing

-- | Parse --region cx1,cy1,cx2,cy2 from args
parseRegion ∷ [String] → (Int, Int, Int, Int)
parseRegion [] = (-8, -8, 8, 8)
parseRegion ("--region":s:_) =
    case map reads (splitOn ',' s) of
        [[(cx1,"")],[(cy1,"")],[(cx2,"")],[(cy2,"")]] →
            (cx1, cy1, cx2, cy2)
        _ → (-8, -8, 8, 8)
parseRegion (_:rest) = parseRegion rest

splitOn ∷ Char → String → [String]
splitOn _ [] = [""]
splitOn d (c:cs)
    | c ≡ d    = "" : splitOn d cs
    | otherwise = case splitOn d cs of
        (w:ws) → (c:w) : ws
        []     → [[c]]

-- | Native (IO) exceptions — e.g. a VulkanException thrown straight
--   from the bindings — bypass the CPS error channel ('throwError')
--   and would escape 'runEngineM' uncaught, skipping the Left branch
--   that shuts worker threads down and flushes buffered logs. Route
--   them into that branch; explicit ExitCode throws still propagate.
guardNativeExceptions ∷ IO (Either EngineException ())
                      → IO (Either EngineException ())
guardNativeExceptions act = act `catch` \(e ∷ SomeException) →
    case fromException e of
        Just (ec ∷ ExitCode) → throwIO ec
        Nothing → pure $ Left $ EngineException
            (ExSystem (IOError (T.pack (displayException e))))
            "uncaught native exception"
            mkErrorContext

-- | Run engine with full graphics (GLFW window + Vulkan)
runGraphical ∷ BootProfile → Maybe Int → IO ()
runGraphical bootProfile mPort = do
  -- Initialize engine
  EngineInitResult env ← initializeEngine

  let env' = case mPort of
        Just p  → env
            { engineConfig = (engineConfig env)
                { ecDebugPort = p
                , ecBootProfile = bootProfile
                } }
        Nothing → env
            { engineConfig = (engineConfig env)
                { ecBootProfile = bootProfile
                } }

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  videoConfig ← readIORef (videoConfigRef env')

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine..."
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
        modify $ \s → s { graphicsState = (graphicsState s) {
                            glfwWindow = Just window } }

        let Window glfwWin = window
        liftIO $ setupCallbacks glfwWin (lifecycleRef env') (inputQueue env')

        _ ← initializeVulkan window
        mainLoop

        -- Combat first: wound ticks enqueue UnitKill/UnitCollapse onto
        -- the unit queue, so the producer has to stop before the
        -- consumer (unit thread) is torn down inside shutdownEngine.
        liftIO $ shutdownThread combatThreadState
        liftIO $ shutdownThread simThreadState
        shutdownEngine window unitThreadState worldThreadState
                              inputThreadState luaThreadState
        logDebugM CatSystem "Engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread combatThreadState
        shutdownThread simThreadState
        shutdownThread unitThreadState
        shutdownThread inputThreadState
        shutdownThread worldThreadState
        shutdownThread luaThreadState
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
        logger ← readIORef (loggerRef env')
        shutdownLogger logger
        exitFailure
    Right _ → pure ()

-- | Run engine in headless mode (no window, no GPU)
--   Starts Lua, world, and unit threads. Debug console on configurable port.
--   Useful for automated testing, CI, and scripted world generation.
runHeadless ∷ BootProfile → Maybe Int → IO ()
runHeadless bootProfile mPort = do
  EngineInitResult env ← initializeEngineHeadless

  let env' = case mPort of
        Just p  → env
            { engineConfig = (engineConfig env)
                { ecDebugPort = p
                , ecBootProfile = bootProfile
                } }
        Nothing → env
            { engineConfig = (engineConfig env)
                { ecBootProfile = bootProfile
                } }

  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'
  combatThreadState ← startCombatThread env'

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine (headless)..."
        headlessLoop
        logInfoM CatSystem "Headless engine shutting down..."
        liftIO $ shutdownThread combatThreadState
        liftIO $ shutdownThread simThreadState
        liftIO $ shutdownThread unitThreadState
        liftIO $ shutdownThread worldThreadState
        liftIO $ shutdownThread luaThreadState
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped
        logDebugM CatSystem "Headless engine shutdown complete."

  result ← guardNativeExceptions $ runEngineM engineAction env' checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
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
                    let json = dumpTilesJSON layers worldSize climate td cx1 cy1 cx2 cy2
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
dumpTilesJSON ∷ DumpLayers → Int → ClimateState → WorldTileData
              → Int → Int → Int → Int → BS.ByteString
dumpTilesJSON layers worldSize climate td cx1 cy1 cx2 cy2 =
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
            zoneFields =
                  ",\"glacierZone\":" ⧺ boolStr (isGlacierZone worldSize gx gy)
                ⧺ ",\"beyondGlacier\":" ⧺ boolStr (isBeyondGlacier worldSize gx gy)
        in base ⧺ terrainFields ⧺ matFields ⧺ fluidFields
                ⧺ iceFields ⧺ oreFields ⧺ zoneFields ⧺ "}"

    fluidTypeStr Ocean = "ocean"
    fluidTypeStr Lake  = "lake"
    fluidTypeStr River = "river"
    fluidTypeStr Lava  = "lava"
    iceModeStr BasinIce = "basin"
    iceModeStr DrapeIce = "drape"
    boolStr True  = "true"
    boolStr False = "false"

{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException)
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Data.Word (Word8, Word64)
import Data.Char (toLower)
import System.Environment (setEnv, getArgs)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Data.List (intercalate, isPrefixOf)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (initializeEngine, initializeEngineHeadless
                        , EngineInitResult(..))
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (runEngineM, EngineM', liftIO)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..)
                         , graphicsState, glfwWindow)
import Engine.Core.Types (EngineConfig(..))
import Engine.Core.Thread (shutdownThread)
import Engine.Core.Log (LogCategory(..), LoggerState(..), LogBackend(..)
                       , shutdownLogger)
import Engine.Core.Log.Monad (logDebugM, logInfoM)
import Engine.Graphics.Vulkan.Init (initializeVulkan)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (setupCallbacks)
import Engine.Input.Thread (startInputThread)
import Engine.Loop (mainLoop)
import Engine.Loop.Headless (headlessLoop, processLuaMessagesHeadless)
import Engine.Loop.Shutdown (shutdownEngine, checkStatus)
import Engine.Scripting.Lua.Backend (startLuaThread)
import World.Thread (startWorldThread)
import World.Types
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell(..), IceMode(..))
import World.Plate (isGlacierZone, isBeyondGlacier)
import World.Thread.ChunkLoading (fillOrphanedSubseaTiles)
import Unit.Thread (startUnitThread)
import Sim.Thread (startSimThread)
import Sim.Command.Types (SimCommand(..))
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)

-- | Which layers to include in dump output.
data DumpLayers = DumpLayers
    { dlTerrain  ∷ !Bool
    , dlMaterial ∷ !Bool
    , dlFluid    ∷ !Bool
    , dlIce      ∷ !Bool
    } deriving (Show)

-- | All layers enabled (default when --dump has no =value).
allLayers ∷ DumpLayers
allLayers = DumpLayers True True True True

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
      mDump    = parseDump args
      port = parseArg "--port" args
      seed = parseArg "--seed" args
      worldSz = parseArg "--worldSize" args
      ages = parseArg "--ages" args
      region = parseRegion args

  case mDump of
    Just layers → runDump layers (fromMaybe 42 seed) (fromMaybe 256 worldSz)
                                 (fromMaybe 5 ages) region
    Nothing
      | headless  → runHeadless (Just (fromMaybe 8008 port))
      | otherwise → runGraphical (Just (fromMaybe 8008 port))

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

-- | Run engine with full graphics (GLFW window + Vulkan)
runGraphical ∷ Maybe Int → IO ()
runGraphical mPort = do
  -- Initialize engine
  EngineInitResult env envVar stateVar ← initializeEngine

  let env' = case mPort of
        Just p  → env { engineConfig = (engineConfig env) { ecDebugPort = p } }
        Nothing → env

  inputThreadState ← startInputThread env'
  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'

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

        liftIO $ shutdownThread simThreadState
        shutdownEngine window unitThreadState worldThreadState
                              inputThreadState luaThreadState
        logDebugM CatSystem "Engine shutdown complete."

  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread simThreadState
        shutdownThread inputThreadState
        shutdownThread luaThreadState
        shutdownThread worldThreadState
    Right _ → pure ()

-- | Run engine in headless mode (no window, no GPU)
--   Starts Lua, world, and unit threads. Debug console on configurable port.
--   Useful for automated testing, CI, and scripted world generation.
runHeadless ∷ Maybe Int → IO ()
runHeadless mPort = do
  EngineInitResult env envVar stateVar ← initializeEngineHeadless

  let env' = case mPort of
        Just p  → env { engineConfig = (engineConfig env) { ecDebugPort = p } }
        Nothing → env

  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        logInfoM CatSystem "Starting engine (headless)..."
        headlessLoop
        logInfoM CatSystem "Headless engine shutting down..."
        liftIO $ shutdownThread simThreadState
        liftIO $ shutdownThread unitThreadState
        liftIO $ shutdownThread worldThreadState
        liftIO $ shutdownThread luaThreadState
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped
        logDebugM CatSystem "Headless engine shutdown complete."

  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread simThreadState
        shutdownThread luaThreadState
        shutdownThread worldThreadState
    Right _ → pure ()

-- | Run engine in dump mode: generate world, load chunks, dump tile
--   data as JSON to stdout, and exit. No TCP server, no loop.
runDump ∷ DumpLayers → Int → Int → Int → (Int, Int, Int, Int) → IO ()
runDump layers seed worldSize ages (cx1, cy1, cx2, cy2) = do
  hPutStrLn stderr $ "dump: seed=" ⧺ show seed
                   ⧺ " worldSize=" ⧺ show worldSize
                   ⧺ " ages=" ⧺ show ages
                   ⧺ " region=(" ⧺ show cx1 ⧺ ","
                   ⧺ show cy1 ⧺ "," ⧺ show cx2 ⧺ ","
                   ⧺ show cy2 ⧺ ")"

  EngineInitResult env envVar stateVar ← initializeEngineHeadless

  let env' = env { engineConfig = (engineConfig env) { ecDebugPort = 0 } }

  -- Redirect logger to stderr so stdout stays clean JSON
  atomicModifyIORef' (loggerRef env') $ \ls →
      (ls { lsBackend = LogToHandle stderr }, ())

  luaThreadState   ← startLuaThread env'
  worldThreadState ← startWorldThread env'
  unitThreadState  ← startUnitThread env'
  simThreadState   ← startSimThread env'

  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        liftIO $ writeIORef (lifecycleRef env') EngineRunning
        liftIO $ threadDelay 500000
        processLuaMessagesHeadless

        -- Pause the sim thread BEFORE any chunks load. This prevents
        -- the sim from racing with the world thread's per-batch seal
        -- during chunk generation. The sim will be fast-settled
        -- synchronously after all chunks are loaded.
        liftIO $ Q.writeQueue (simQueue env') SimPause

        liftIO $ hPutStrLn stderr "dump: generating world..."
        liftIO $ Q.writeQueue (worldQueue env')
            (WorldInit (WorldPageId "dump")
                       (fromIntegral seed ∷ Word64)
                       worldSize ages)
        liftIO $ waitForInit env' (ages * 120)

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

        liftIO $ waitForChunks env' 300

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
                    -- Apply orphaned-subsea cleanup as a final pass.
                    td ← atomicModifyIORef' (wsTilesRef ws) $ \td →
                        let td' = fillOrphanedSubseaTiles td
                        in (td', td')
                    let json = dumpTilesJSON layers worldSize td cx1 cy1 cx2 cy2
                    BS.putStr json
                    hFlush stdout
                    hPutStrLn stderr $ "dump: done"
                [] → hPutStrLn stderr "dump: no world data"

        liftIO $ writeIORef (lifecycleRef env') CleaningUp
        liftIO $ shutdownThread simThreadState
        liftIO $ shutdownThread unitThreadState
        liftIO $ shutdownThread worldThreadState
        liftIO $ shutdownThread luaThreadState
        logger ← liftIO $ readIORef $ loggerRef env'
        liftIO $ shutdownLogger logger
        liftIO $ writeIORef (lifecycleRef env') EngineStopped

  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        shutdownThread simThreadState
        shutdownThread luaThreadState
        shutdownThread worldThreadState
    Right _ → pure ()

-- | Poll until world generation is done
waitForInit ∷ EngineEnv → Int → IO ()
waitForInit _ 0 = hPutStrLn stderr "dump: init timeout"
waitForInit env n = do
    manager ← readIORef (worldManagerRef env)
    case wmWorlds manager of
        ((_, ws):_) → do
            phase ← readIORef (wsLoadPhaseRef ws)
            case phase of
                LoadDone → hPutStrLn stderr "dump: init complete"
                _        → threadDelay 250000 >> waitForInit env (n - 1)
        [] → threadDelay 250000 >> waitForInit env (n - 1)

-- | Poll until chunk init queue is empty
waitForChunks ∷ EngineEnv → Int → IO ()
waitForChunks _ 0 = hPutStrLn stderr "dump: chunk load timeout"
waitForChunks env n = do
    manager ← readIORef (worldManagerRef env)
    case wmWorlds manager of
        ((_, ws):_) → do
            remaining ← length <$> readIORef (wsInitQueueRef ws)
            if remaining ≡ 0
                then hPutStrLn stderr "dump: all chunks loaded"
                else threadDelay 250000 >> waitForChunks env (n - 1)
        [] → threadDelay 250000 >> waitForChunks env (n - 1)

-- | Dump per-tile data in a chunk region as JSON.
--   Every tile in the region gets one object. Fields are included
--   based on the DumpLayers whitelist.
dumpTilesJSON ∷ DumpLayers → Int → WorldTileData
              → Int → Int → Int → Int → BS.ByteString
dumpTilesJSON layers worldSize td cx1 cy1 cx2 cy2 =
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
            terrainFields
              | dlTerrain layers =
                  ",\"terrainZ\":" ⧺ show terrainZ
                  ⧺ ",\"surfaceZ\":" ⧺ show surfZ
              | otherwise = ""
            matFields
              | dlMaterial layers =
                  let col = lcTiles lc V.! idx
                      matId = if VU.null (ctMats col) then 0
                              else ctMats col VU.! (VU.length (ctMats col) - 1)
                  in ",\"matId\":" ⧺ show matId
              | otherwise = ""
            fluidFields
              | dlFluid layers = case lcFluidMap lc V.! idx of
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
            zoneFields =
                  ",\"glacierZone\":" ⧺ boolStr (isGlacierZone worldSize gx gy)
                ⧺ ",\"beyondGlacier\":" ⧺ boolStr (isBeyondGlacier worldSize gx gy)
        in base ⧺ terrainFields ⧺ matFields ⧺ fluidFields
                ⧺ iceFields ⧺ zoneFields ⧺ "}"

    fluidTypeStr Ocean = "ocean"
    fluidTypeStr Lake  = "lake"
    fluidTypeStr River = "river"
    fluidTypeStr Lava  = "lava"
    iceModeStr BasinIce = "basin"
    iceModeStr DrapeIce = "drape"
    boolStr True  = "true"
    boolStr False = "false"

module Engine.Scripting.Lua.API.Core
  ( quitFn
  , getFPSFn
  , loadScriptFn
  , killScriptFn
  , setTickIntervalFn
  , pauseScriptFn
  , resumeScriptFn
  , listFilesFn
  , setPausedFn
  , isPausedFn
  , getBootProfileFn
  , getPreviewTargetFn
  , realTimeFn
  , gameTimeFn
  ) where

import UPrelude
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.Script (callModuleFunction, loadModuleRef)
import Engine.Scripting.Lua.Util (isValidRef, nowSeconds)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..))
import Engine.Core.Types (EngineConfig(..), bootProfileTag)
import Engine.Core.Log (logInfo, logWarn, logDebug, LogCategory(..))
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath (takeExtension)


quitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
quitFn env = do
  liftIO $ writeIORef (lifecycleRef env) CleaningUp
  return 0

getFPSFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getFPSFn env = do
  fps ← liftIO $ readIORef (fpsRef env)
  Lua.pushnumber (Lua.Number fps)
  return 1

-- | engine.setPaused(bool) — write the global pause flag.
--   Side effects (timeScale, etc.) are handled Lua-side in
--   scripts/pause.lua so the engine doesn't depend on world state.
setPausedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setPausedFn env = do
  b ← Lua.toboolean 1
  Lua.liftIO $ writeIORef (enginePausedRef env) b
  return 0

-- | engine.isPaused() → bool
isPausedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
isPausedFn env = do
  p ← Lua.liftIO $ readIORef (enginePausedRef env)
  Lua.pushboolean p
  return 1

-- | engine.getBootProfile() → "normal" | "arena"
getBootProfileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getBootProfileFn env = do
  Lua.pushstring . TE.encodeUtf8 . bootProfileTag . ecBootProfile $
      engineConfig env
  return 1

-- | engine.getPreviewTarget() → {category=..., item=...} | nil
--   The parsed @--preview category[/item]@ target; nil outside preview
--   boot ('BootPreview'). @item@ is omitted for a bare category.
getPreviewTargetFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getPreviewTargetFn env = do
  case ecPreviewTarget (engineConfig env) of
    Nothing → Lua.pushnil
    Just (cat, mItem) → do
      Lua.newtable
      Lua.pushstring (TE.encodeUtf8 cat)
      Lua.setfield (-2) "category"
      case mItem of
        Just item → do
          Lua.pushstring (TE.encodeUtf8 item)
          Lua.setfield (-2) "item"
        Nothing → pure ()
  return 1

-- | engine.realTime() → number
--   POSIX wall-clock seconds (sub-second precision). Doesn't freeze
--   when the engine is paused — use this when timing events that
--   should respect real time regardless of in-game pause state
--   (e.g. popup-coalescing windows).
realTimeFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
realTimeFn = do
  now ← Lua.liftIO getCurrentTime
  let secs = realToFrac (utcTimeToPOSIXSeconds now) ∷ Double
  Lua.pushnumber (Lua.Number secs)
  return 1

-- | engine.gameTime() → number
--   Monotonic game-clock in seconds. Advances by real-tick dt only
--   when the engine is NOT paused; survives save/load (persisted to
--   sdGameTime in v6+). Use this for in-game elapsed-time
--   heuristics — AI session timing, building stuck-timeouts,
--   anything that should freeze when the player pauses or that
--   should NOT include real-world save→load gap time.
gameTimeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
gameTimeFn env = do
  t ← Lua.liftIO $ readIORef (gameTimeRef env)
  Lua.pushnumber (Lua.Number t)
  return 1

setTickIntervalFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setTickIntervalFn env backendState = do
   scriptIdNum ← Lua.tointeger 1
   interval ← Lua.tonumber 2
   case (scriptIdNum, interval) of
       (Just sid, Just (Lua.Number seconds)) → Lua.liftIO $ do
           currentSecs ← nowSeconds
           atomically $ modifyTVar' (lbsScripts backendState) $
               Map.adjust (\s → s { scriptTickRate = seconds
                                  , scriptNextTick = currentSecs + seconds
                                  }) (fromIntegral sid)
           logger ← readIORef (loggerRef env)
           logInfo logger CatLua $ T.pack $
               "Tick interval for script " ⧺ show sid ⧺ " set to " ⧺ show seconds ⧺ " seconds."
       _ → Lua.liftIO $ do
           logger ← readIORef (loggerRef env)
           logInfo logger CatLua
               "setTickInterval requires 2 arguments: scriptId, seconds"
   return 0

loadScriptFn ∷ EngineEnv → LuaBackendState → Lua.State 
             → Lua.LuaE Lua.Exception Lua.NumResults
loadScriptFn env backendState lst = do
    path ← Lua.tostring 1
    tickRate ← Lua.tonumber 2
    case (path, tickRate) of
        (Just pathBS, Just rate) → do
            logger ← Lua.liftIO $ readIORef (loggerRef env)
            scriptId ← Lua.liftIO $ do
                let pathStr = TE.decodeUtf8Lenient pathBS

                -- Dedup by path: loading the same script twice would
                -- create a second tickable instance (update/broadcast
                -- handlers firing twice) and leak the first registry
                -- ref. Return the existing ID instead — deliberately
                -- NOT reload semantics; killScript first to reload.
                existing ← readTVarIO (lbsScripts backendState)
                case [ s | s ← Map.elems existing
                         , scriptPath s ≡ T.unpack pathStr ] of
                  (dup:_) → do
                    logDebug logger CatLua $
                        "loadScript: already loaded, reusing ID "
                        <> T.pack (show (scriptId dup)) <> ": " <> pathStr
                    return (Just (scriptId dup))
                  [] → do
                    logDebug logger CatLua $ "Loading Lua script: " <> pathStr

                    sid ← atomicModifyIORef' (lbsNextScriptId backendState)
                        (\n → (n + 1, n))

                    result ← Lua.runWith lst $ loadModuleRef (T.unpack pathStr)
                    case result of
                        Right modRef → do
                            let dropDir (('/'):xs) = T.pack xs
                                dropDir (_x:xs    ) = dropDir xs
                                dropDir _          = ""
                            logDebug logger CatLua $ "loaded: "
                                                  <> (dropDir (T.unpack (pathStr)))
                            logDebug logger CatLua $ " with ID " <> T.pack (show sid)

                            currentSecs ← nowSeconds
                            let script = LuaScript
                                    { scriptId        = sid
                                    , scriptPath      = T.unpack pathStr
                                    , scriptTickRate  = realToFrac rate
                                    , scriptNextTick  = currentSecs + realToFrac rate
                                    , scriptModuleRef = modRef
                                    , scriptPaused    = False
                                    }

                            atomically $ modifyTVar' (lbsScripts backendState) $
                                Map.insert sid script

                            when (isValidRef modRef) $
                                void $ callModuleFunction backendState modRef "init" []

                            logDebug logger CatLua $ "Lua script initialized with ID "
                                           <> T.pack (show sid)

                            return (Just sid)
                        Left errMsg → do
                            logWarn logger CatLua $ "Failed to load Lua script: " <> pathStr
                                           <> " - " <> errMsg
                            return Nothing
            
            case scriptId of
                Just sid → Lua.pushinteger (Lua.Integer $ fromIntegral sid)
                Nothing  → Lua.pushnil
        _ → Lua.pushnil
    return 1

killScriptFn ∷ EngineEnv → LuaBackendState → Lua.State → Lua.LuaE Lua.Exception Lua.NumResults
killScriptFn env backendState lst = do
    sidNum ← Lua.tointeger 1
    case sidNum of
        Just sid → Lua.liftIO $ do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua $ "Destroying Lua script with ID " 
                           <> T.pack (show sid)
            scriptsMap ← readTVarIO (lbsScripts backendState)
            case Map.lookup (fromIntegral sid) scriptsMap of
                Just script → do
                    when (isValidRef (scriptModuleRef script)) $ do
                        _ ← callModuleFunction backendState (scriptModuleRef script) "shutdown" []
                        Lua.runWith lst $ Lua.unref Lua.registryindex (scriptModuleRef script)
                    atomically $ modifyTVar' (lbsScripts backendState) $
                        Map.delete (fromIntegral sid)
                    logDebug logger CatLua $ "Lua script destroyed: ID " 
                                   <> T.pack (show sid)
                Nothing → return ()
        _ → return ()
    return 0

pauseScriptFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
pauseScriptFn backendState = do
    sidNum ← Lua.tointeger 1
    case sidNum of
        Just sid → Lua.liftIO $ atomically $ modifyTVar' (lbsScripts backendState) $
            Map.adjust (\s → s { scriptPaused = True }) (fromIntegral sid)
        _ → return ()
    return 0

resumeScriptFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
resumeScriptFn backendState = do
    sidNum ← Lua.tointeger 1
    case sidNum of
        Just sid → Lua.liftIO $ do
            currentSecs ← nowSeconds
            atomically $ modifyTVar' (lbsScripts backendState) $
                Map.adjust (\s → s { scriptPaused = False, scriptNextTick = currentSecs })
                           (fromIntegral sid)
        _ → return ()
    return 0

-- | List files in a directory matching an extension.
--   Returns a Lua array of filenames, or nil if the directory doesn't exist.
--   NB: order is OS-dependent (listDirectory) — callers that need a
--   deterministic order must sort themselves. (Do NOT sort here: flora
--   IDs are allocated in load order and salt worldgen placement, so a
--   global sort would change same-seed flora output.)
listFilesFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
listFilesFn = do
    dirArg ← Lua.tostring 1
    extArg ← Lua.tostring 2
    case (dirArg, extArg) of
        (Just dirBS, Just extBS) → do
            let dirPath = T.unpack (TE.decodeUtf8Lenient dirBS)
                ext     = T.unpack (TE.decodeUtf8Lenient extBS)
            exists ← Lua.liftIO $ doesDirectoryExist dirPath
            if not exists
                then do
                    Lua.pushnil
                    return 1
                else do
                    allFiles ← Lua.liftIO $ listDirectory dirPath
                    let matching = filter (\f → takeExtension f ≡ ext) allFiles
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] matching) $ \(i, filename) → do
                        Lua.pushstring (TE.encodeUtf8 (T.pack filename))
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1
        _ → do
            Lua.pushnil
            return 1

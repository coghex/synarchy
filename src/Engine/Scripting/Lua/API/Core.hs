module Engine.Scripting.Lua.API.Core
  ( quitFn
  , getFPSFn
  , loadScriptFn
  , killScriptFn
  , setTickIntervalFn
  , pauseScriptFn
  , resumeScriptFn
  , listFilesFn
  , listFilesRecursiveFn
  , setPausedFn
  , isPausedFn
  , getBootProfileFn
  , getPreviewTargetFn
  , getPreviewBrowseFn
  , realTimeFn
  , gameTimeFn
  ) where

import UPrelude
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.Script (callModuleFunction, loadModuleRef)
import Engine.Scripting.Lua.Util (isValidRef, nowSeconds)
import Engine.Scripting.Lua.TickPolicy
    (TickInterval(..), classifyTickInterval, tickIntervalSeconds
    , describeTickRefusal)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability, withPlayerIntent)
import Engine.Core.Capability.Core
    (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv, EngineLifecycle(..), loadStatusRef)
import Engine.Core.Types
    (EngineConfig(..), bootProfileTag, PreviewBrowse(..), PreviewEntry(..)
    , PreviewUnit(..), PreviewAnim(..), PreviewFrameDir(..), PreviewFrame(..)
    , PreviewBuilding(..), PreviewBuildingEntry(..))
import Engine.Core.Log (logInfo, logWarn, logDebug, LogCategory(..))
import Engine.Load.Status (loadInProgress)
import World.Pause (imposePauseHeld, releasePauseHeld)
import Engine.Asset.Discovery (walkFilesWithExtension)
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
  liftIO $ writeIORef (ccLifecycleRef (toCoreCapability env)) CleaningUp
  return 0

getFPSFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getFPSFn env = do
  fps ← liftIO $ readIORef (rvFpsRef (toRenderViewCapability env))
  Lua.pushnumber (Lua.Number fps)
  return 1

-- | engine.setPaused(bool) → bool (applied?) — flip the global pause
--   flag AND the paused page's clock, as one pair ("World.Pause").
--
--   #1599 moved that pairing in here from scripts/pause.lua. Lua could
--   only maintain it for a pause Lua itself imposed, and every
--   engine-side writer of the flag (a @pause: true@ notification
--   category, a save's acceptance, a load publish) bypassed it — so the
--   player's own resume handed back a stale speed. The engine now
--   captures the visible page's chosen speed when a pause epoch opens
--   and gives it back when the epoch closes, whoever opened it.
--
--   Issue #763: an UNPAUSE (setPaused(false)) while a
--   load transaction is in flight is rejected outright — staging runs
--   BEFORE the save barrier's capture lock is ever entered, and the Lua
--   thread's own tick loop keeps servicing debug/script work throughout
--   that entire window (this function included), so an unpause landing
--   there could resume the OLD, still-live session's simulation before
--   the transaction either publishes or fails. A subsequent staging
--   failure must leave the pre-load session's pause state exactly as it
--   was, per the #763 "nothing changed" contract — so this holds it
--   paused instead. A PAUSE (setPaused(true)) is never blocked: pausing
--   an already-paused-or-not session can't violate that contract.
--
--   The boolean return (previously no return value at all) reports
--   whether the flag was actually flipped. It was added because
--   scripts/pause.lua's pause.set applied its OWN side effects
--   unconditionally, with no way to notice a rejection, which left
--   "ticks frozen, but world time still advancing". The side effects
--   have since moved in here (#1599), so a rejection now leaves both
--   halves of the pair untouched by construction; the module still
--   reads the answer to keep its own mirror honest.
setPausedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setPausedFn env = do
  b ← Lua.toboolean 1
  applied ← Lua.liftIO $ do
      loading ← loadInProgress (loadStatusRef env)
      if loading ∧ not b
        then do
            logger ← readIORef (ccLoggerRef (toCoreCapability env))
            logWarn logger CatLua
                "setPaused(false) rejected: a load transaction is in \
                \flight -- unpausing now could resume simulation before \
                \it either publishes or fails"
            pure False
        else do
            -- #913: an APPLIED pause/resume is player intent. The flag
            -- write happens INSIDE the intent lock, so an autosave's
            -- compare-then-restore (which takes the same lock) can
            -- neither miss this transition nor overwrite it afterwards.
            -- Counted unconditionally rather than only on a value CHANGE:
            -- the generation records that the player asked for something
            -- during a save's request window, and an autosave that finds
            -- it bumped declines to restore its own pre-save state — so
            -- over-counting is the safe direction and under-counting is
            -- not. A REJECTED call above deliberately doesn't count:
            -- nothing changed for anyone.
            -- The @…Held@ variants: 'withPlayerIntent' already holds
            -- the MVar that is also the epoch mutex ("World.Pause"), and
            -- re-entering it would deadlock.
            withPlayerIntent (toWorldSimCapability env) $
                if b then imposePauseHeld (toWorldSimCapability env)
                     else releasePauseHeld (toWorldSimCapability env)
            pure True
  Lua.pushboolean applied
  return 1

-- | engine.isPaused() → bool
isPausedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
isPausedFn env = do
  p ← Lua.liftIO $ readIORef (wsEnginePausedRef (toWorldSimCapability env))
  Lua.pushboolean p
  return 1

-- | engine.getBootProfile() → "normal" | "arena"
getBootProfileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getBootProfileFn env = do
  Lua.pushstring . TE.encodeUtf8 . bootProfileTag . ecBootProfile $
      ccEngineConfig (toCoreCapability env)
  return 1

-- | engine.getPreviewTarget() → {category=..., item=...} | nil
--   The parsed @--preview category[/item]@ target; nil outside preview
--   boot ('BootPreview'). @item@ is omitted for a bare category.
getPreviewTargetFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getPreviewTargetFn env = do
  case ecPreviewTarget (ccEngineConfig (toCoreCapability env)) of
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

-- | engine.getPreviewBrowse() → nil | {mode="list", entries={{label=,path=},...}}
--   | {mode="item", entry={label=,path=}}
--   | {mode="unit", unit={name=,defaultAnim=,animations={...}}}
--   | {mode="building", building={name=,defaultEntry=,entries={...}}}
--   The browsing state @app/Main.hs@ resolved before boot:
--   'PreviewList' for a bare @--preview \<simple category\>@ (#886) and
--   for a @--preview flora\/\<name\>@ \/ @structures\/\<name\>@ target
--   (#888 routes both into that same browser, rooted at the item's own
--   folder), 'PreviewItem' for a validated
--   @--preview \<simple category\>/\<item\>@ (#886), 'PreviewUnitAnims'
--   for a validated @--preview units/\<name\>@ (#887), and
--   'PreviewBuildingAssets' for a validated
--   @--preview buildings/\<name\>@ (#888). 'nil' only outside
--   'BootPreview' — every canonical preview target now resolves to a
--   real browsing mode.
--
--   The unit payload's @animations@ array is already in the viewer's
--   display order, and each entry's @directions@ array is already in
--   the game's @S, SW, W, NW, N, NE, E, SE@ order with unavailable
--   directions omitted; the building payload's @entries@ array is
--   likewise already ordered, each carrying its own static\/animation
--   identity and effective @fps@\/@loop@ —
--   'Engine.Preview.Unit'\/'Engine.Preview.Building' own those rules,
--   the Lua side never re-derives them.
--
--   A unit animation's FRAME is a table, not a path (#1260):
--   @{ path, u0, v0, u1, v1 }@ plus @width@\/@height@ when the compiled
--   index knows the cell size. An ATLAS-backed animation carries its
--   compiled atlas path in @atlas@ and every one of its frames names
--   that same image, differing only in the sub-rect; a legacy animation
--   omits @atlas@ and its frames carry the whole-image rect. So a
--   consumer must publish a frame with @UI.setSpriteFrame@ — a bare
--   @setSpriteTexture@ would draw the entire sheet. Building frames are
--   still bare paths: buildings are never compiled.
getPreviewBrowseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getPreviewBrowseFn env = do
  case ecPreviewBrowse (ccEngineConfig (toCoreCapability env)) of
    Nothing → Lua.pushnil
    Just (PreviewList entries) → do
      Lua.newtable
      Lua.pushstring "list"
      Lua.setfield (-2) "mode"
      Lua.newtable
      forM_ (zip [1 ∷ Int ..] entries) $ \(i, entry) → do
        pushPreviewEntry entry
        Lua.rawseti (-2) (fromIntegral i)
      Lua.setfield (-2) "entries"
    Just (PreviewItem entry) → do
      Lua.newtable
      Lua.pushstring "item"
      Lua.setfield (-2) "mode"
      pushPreviewEntry entry
      Lua.setfield (-2) "entry"
    Just (PreviewUnitAnims unit) → do
      Lua.newtable
      Lua.pushstring "unit"
      Lua.setfield (-2) "mode"
      pushPreviewUnit unit
      Lua.setfield (-2) "unit"
    Just (PreviewBuildingAssets building) → do
      Lua.newtable
      Lua.pushstring "building"
      Lua.setfield (-2) "mode"
      pushPreviewBuilding building
      Lua.setfield (-2) "building"
  return 1
  where
    pushPreviewEntry entry = do
      Lua.newtable
      pushTextField "label" (peLabel entry)
      pushTextField "path"  (pePath entry)

    pushTextField key value = do
      Lua.pushstring (TE.encodeUtf8 value)
      Lua.setfield (-2) key

    -- 'void': 'push' generalizes to an unconstrained result type here,
    -- so a bare `push x` trips -Wunused-do-bind (an error under CI's
    -- -Werror). Every pusher passed in returns (), so discarding is
    -- correct as well as necessary.
    pushArray push xs = do
      Lua.newtable
      forM_ (zip [1 ∷ Int ..] xs) $ \(i, x) → do
        void (push x)
        Lua.rawseti (-2) (fromIntegral i)

    pushPreviewUnit unit = do
      Lua.newtable
      pushTextField "name" (puName unit)
      pushTextField "defaultAnim" (puDefault unit)
      pushArray pushPreviewAnim (puAnims unit)
      Lua.setfield (-2) "animations"

    pushPreviewAnim anim = do
      Lua.newtable
      pushTextField "name" (paName anim)
      Lua.pushnumber (realToFrac (paFps anim))
      Lua.setfield (-2) "fps"
      Lua.pushboolean (paLoop anim)
      Lua.setfield (-2) "loop"
      Lua.pushboolean (paFlip anim)
      Lua.setfield (-2) "flip"
      -- The animation's compiled atlas path (#1260). Unconditional
      -- since #1261: every unit animation is atlas-backed, so this is a
      -- fact about the animation rather than the mode signal its
      -- absence used to be.
      pushTextField "atlas" (paAtlas anim)
      forM_ (paThumb anim) $ \t → do
        pushPreviewFrame t
        Lua.setfield (-2) "thumb"
      pushArray pushPreviewDir (paDirs anim)
      Lua.setfield (-2) "directions"

    pushPreviewDir d = do
      Lua.newtable
      pushTextField "direction" (pfdDirection d)
      pushTextField "source"    (pfdSource d)
      Lua.pushboolean (pfdMirrored d)
      Lua.setfield (-2) "mirrored"
      pushArray pushPreviewFrame (pfdFrames d)
      Lua.setfield (-2) "frames"

    -- ONE frame: the atlas to load, the cell to sample within it, and
    -- that cell's own pixel size. The size is the CELL's, never the
    -- sheet's, so the viewer must not measure the resident texture to
    -- size a frame — it still measures it to learn whether the upload
    -- has landed.
    pushPreviewFrame f = do
      Lua.newtable
      pushTextField "path" (pfPath f)
      let (u0, v0, u1, v1) = pfUV f
      forM_ [("u0", u0), ("v0", v0), ("u1", u1), ("v1", v1)] $ \(k, v) → do
        Lua.pushnumber (realToFrac v)
        Lua.setfield (-2) k
      let (cw, ch) = pfCell f
      Lua.pushinteger (fromIntegral cw)
      Lua.setfield (-2) "width"
      Lua.pushinteger (fromIntegral ch)
      Lua.setfield (-2) "height"

    pushPreviewBuilding building = do
      Lua.newtable
      pushTextField "name"         (pbName building)
      pushTextField "defaultEntry" (pbDefault building)
      pushArray pushBuildingEntry (pbEntries building)
      Lua.setfield (-2) "entries"

    pushBuildingEntry e = do
      Lua.newtable
      pushTextField "label" (pbeLabel e)
      Lua.pushboolean (pbeAnimated e)
      Lua.setfield (-2) "animated"
      Lua.pushnumber (realToFrac (pbeFps e))
      Lua.setfield (-2) "fps"
      Lua.pushboolean (pbeLoop e)
      Lua.setfield (-2) "loop"
      pushArray (Lua.pushstring ∘ TE.encodeUtf8) (pbeFrames e)
      Lua.setfield (-2) "frames"

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
  t ← Lua.liftIO $ readIORef (wsGameTimeRef (toWorldSimCapability env))
  Lua.pushnumber (Lua.Number t)
  return 1

-- | @engine.setTickInterval(scriptId, seconds)@.
--
--   @seconds@ obeys the shared tick-interval policy in
--   "Engine.Scripting.Lua.TickPolicy" (#1695), identically to
--   'loadScriptFn': @0@ means EVENT-ONLY (the script keeps receiving
--   broadcasts, messages and direct calls, but its @update@ is never
--   called on a timer and it no longer takes part in scheduling), a
--   finite value of at least 'minTickInterval' schedules as it always
--   has, and a negative, @NaN@, infinite or sub-'minTickInterval' value
--   is REFUSED.
--
--   A refusal leaves BOTH 'scriptTickRate' and 'scriptNextTick' exactly
--   as they were — nothing is clamped and nothing bad is stored — and
--   reports itself through 'logWarn' naming the offending value rather
--   than raising. The Lua return convention is unchanged: this verb has
--   always returned no values, so the log entry is the signal.
setTickIntervalFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setTickIntervalFn env backendState = do
   scriptIdNum ← Lua.tointeger 1
   interval ← Lua.tonumber 2
   case (scriptIdNum, interval) of
       (Just sid, Just (Lua.Number seconds)) → Lua.liftIO $ do
           logger ← readIORef (ccLoggerRef (toCoreCapability env))
           case classifyTickInterval seconds of
             Left refusal →
               logWarn logger CatLua $
                   "setTickInterval refused for script " <> tshow sid <> ": "
                   <> describeTickRefusal refusal seconds
             Right accepted → do
               let rate = tickIntervalSeconds accepted
               currentSecs ← nowSeconds
               atomically $ modifyTVar' (lbsScripts backendState) $
                   Map.adjust (\s → s { scriptTickRate = rate
                                      , scriptNextTick = currentSecs + rate
                                      }) (fromIntegral sid)
               logInfo logger CatLua $ case accepted of
                   TickEventOnly → "Tick interval for script " <> tshow sid
                                   <> " set to 0 seconds (event-only: no timed update)."
                   TickEvery _   → "Tick interval for script " <> tshow sid
                                   <> " set to " <> tshow rate <> " seconds."
       _ → Lua.liftIO $ do
           logger ← readIORef (ccLoggerRef (toCoreCapability env))
           logInfo logger CatLua
               "setTickInterval requires 2 arguments: scriptId, seconds"
   return 0

-- | @engine.loadScript(path, tickRate)@.
--
--   @tickRate@ obeys the shared tick-interval policy in
--   "Engine.Scripting.Lua.TickPolicy" (#1695), identically to
--   'setTickIntervalFn': @0@ means EVENT-ONLY (the module is loaded,
--   initialised and reachable by broadcast, message and direct call, but
--   its @update@ is never called on a timer and it takes no part in
--   scheduling), a finite value of at least 'minTickInterval' ticks as
--   it always has, and a negative, @NaN@, infinite or
--   sub-'minTickInterval' value is REFUSED.
--
--   The interval is validated FIRST, before the dedup-by-path lookup and
--   before any chunk runs: a refused rate returns @nil@, logs the
--   refusal naming the value, executes neither the chunk nor @init@,
--   allocates no script id, and — when the path happens to be loaded
--   already — leaves that existing script's interval untouched. A valid
--   duplicate still returns its existing id without changing its
--   interval, exactly as before.
--
--   The chunk runs on the backend's own canonical 'Lua.State'
--   ('lbsLuaState'), NOT on whatever state this handler happens to be
--   invoked with (#1059). Handler-local @Lua.state@ is the INVOKING
--   state: a script calling @engine.loadScript@ from inside a
--   coroutine would otherwise load the module onto the coroutine's
--   stack instead of the main state the registrar was given. Every
--   'Engine.Scripting.Lua.API.registerLuaAPI' caller passes
--   @lbsLuaState backendState@ as that state already, so reading it
--   back off the backend is the same state the old threaded
--   'Lua.State' parameter carried.
loadScriptFn ∷ EngineEnv → LuaBackendState
             → Lua.LuaE Lua.Exception Lua.NumResults
loadScriptFn env backendState = do
    let lst = lbsLuaState backendState
    path ← Lua.tostring 1
    tickRate ← Lua.tonumber 2
    scriptId ← case (path, tickRate) of
        (Just pathBS, Just (Lua.Number rate)) → do
            logger ← Lua.liftIO $ readIORef (ccLoggerRef (toCoreCapability env))
            let pathStr = TE.decodeUtf8Lenient pathBS
            -- Validate the interval BEFORE the dedup lookup and before
            -- any chunk runs, so a refusal cannot half-load a module or
            -- disturb an already-loaded one (#1695).
            case classifyTickInterval rate of
              Left refusal → Lua.liftIO $ do
                logWarn logger CatLua $
                    "loadScript refused for " <> pathStr <> ": "
                    <> describeTickRefusal refusal rate
                    <> " The script was not loaded."
                return Nothing
              Right accepted → Lua.liftIO $ do
                let tickSeconds = tickIntervalSeconds accepted

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
                        <> tshow (scriptId dup) <> ": " <> pathStr
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
                            logDebug logger CatLua $ " with ID " <> tshow sid

                            currentSecs ← nowSeconds
                            let script = LuaScript
                                    { scriptId        = sid
                                    , scriptPath      = T.unpack pathStr
                                    , scriptTickRate  = tickSeconds
                                    , scriptNextTick  = currentSecs + tickSeconds
                                    , scriptModuleRef = modRef
                                    , scriptPaused    = False
                                    }

                            atomically $ modifyTVar' (lbsScripts backendState) $
                                Map.insert sid script

                            when (isValidRef modRef) $
                                void $ callModuleFunction backendState modRef "init" []

                            logDebug logger CatLua $ "Lua script initialized with ID "
                                           <> tshow sid

                            return (Just sid)
                        Left errMsg → do
                            logWarn logger CatLua $ "Failed to load Lua script: " <> pathStr
                                           <> " - " <> errMsg
                            return Nothing
        _ → pure Nothing
    case scriptId of
        Just sid → Lua.pushinteger (Lua.Integer $ fromIntegral sid)
        Nothing  → Lua.pushnil
    return 1

-- | @engine.killScript(id)@. Unrefs on the backend's own canonical
--   'Lua.State' for the same reason 'loadScriptFn' loads on it.
killScriptFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
killScriptFn env backendState = do
    let lst = lbsLuaState backendState
    sidNum ← Lua.tointeger 1
    case sidNum of
        Just sid → Lua.liftIO $ do
            logger ← readIORef (ccLoggerRef (toCoreCapability env))
            logDebug logger CatLua $ "Destroying Lua script with ID " 
                           <> tshow sid
            scriptsMap ← readTVarIO (lbsScripts backendState)
            case Map.lookup (fromIntegral sid) scriptsMap of
                Just script → do
                    when (isValidRef (scriptModuleRef script)) $ do
                        _ ← callModuleFunction backendState (scriptModuleRef script) "shutdown" []
                        Lua.runWith lst $ Lua.unref Lua.registryindex (scriptModuleRef script)
                    atomically $ modifyTVar' (lbsScripts backendState) $
                        Map.delete (fromIntegral sid)
                    logDebug logger CatLua $ "Lua script destroyed: ID " 
                                   <> tshow sid
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
--   deterministic order must sort themselves. Do NOT sort here: this
--   binding's whole point is that ordering is the CALLER's decision, so
--   @scripts\/startup_loader.lua@'s canonical order stays a pure,
--   testable transformation over an already-enumerated list rather than
--   something buried inside the enumeration (#1232). @data\/flora@ is
--   the caller that needs one — its sequential @FloraId@s are what a
--   save's numeric flora references name — and since #2241 it applies
--   @canonicalFileOrder@ itself, at its own call site.
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

-- | List every file with a matching extension under a directory TREE,
--   recursively (#1232). Returns a Lua array of paths RELATIVE to the
--   requested directory, @/@-separated at every depth, or nil if the
--   directory doesn't exist.
--
--   The recursive counterpart of 'listFilesFn', deliberately kept
--   beside it rather than folded into it: @engine.listFiles@ must stay
--   flat for its own callers (see its note above).
--
--   NB: like 'listFilesFn', order is OS-dependent — a caller that needs
--   determinism applies its own total order to the result. That is not
--   an oversight: @scripts/startup_loader.lua@'s canonical item-file
--   order is a pure transformation over this list, which is what lets a
--   test drive it with two different enumeration orders.
--
--   A symlink at any depth is skipped, so the walk always terminates
--   and never escapes the requested directory
--   ('Engine.Asset.Discovery.walkFilesWithExtension').
listFilesRecursiveFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
listFilesRecursiveFn = do
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
                    matching ← Lua.liftIO $
                        walkFilesWithExtension dirPath ext
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] matching) $ \(i, relPath) → do
                        Lua.pushstring (TE.encodeUtf8 (T.pack relPath))
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1
        _ → do
            Lua.pushnil
            return 1

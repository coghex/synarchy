-- | Debug console command handling for the Lua thread: TCP debug-server
--   builtins (handled off the Lua thread) plus loadstring+pcall execution
--   of ordinary debug-console commands (handled on it).
module Engine.Scripting.Lua.Thread.Console
  ( processDebugCommands
  , debugBuiltin
  ) where

import UPrelude
import Engine.Scripting.Lua.DebugServer (DebugCommand(..), pollDebugCommand)
import Engine.Scripting.Lua.API.Shell (luaValueToText)
import Engine.Core.State (EngineEnv(..), EngineLifecycle(..), activeWorldState)
import World.State.Types (wsLoadPhaseRef, wsInitQueueRef, LoadPhase(..))
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.STM.TQueue (TQueue)

-- | Process all pending debug commands from the TCP server.
--   Each command is a line of Lua code. We execute it via
--   loadstring + pcall and send the result back through the MVar.
processDebugCommands ∷ Lua.State → TQueue DebugCommand → IO ()
processDebugCommands lst debugQueue = do
    mCmd ← pollDebugCommand debugQueue
    case mCmd of
        Nothing → return ()
        Just (DebugCommand cmdText responseMVar) → do
            result ← executeDebugLua lst cmdText
            putMVar responseMVar result
            processDebugCommands lst debugQueue

-- | Debug-console BUILT-INS, run on the per-connection client thread
--   (see 'startDebugServer') so they never block the single Lua thread.
--
--   'world.waitForInit' / 'world.waitForChunks' only poll world-state
--   IORefs — no Lua state — so handling them here means: (a) the Lua
--   thread stays free (a second connection can poll progress mid-wait,
--   ticks keep running); (b) there's no debug-server response cap on the
--   wait (the old 30 s 'takeMVar' timeout used to make 'waitForInit(300)'
--   over netcat spuriously report a timeout on any world taking >30 s to
--   generate). Returns 'Nothing' for anything it doesn't recognise, so
--   the command falls through to the Lua thread unchanged.
debugBuiltin ∷ EngineEnv → Text → IO (Maybe Text)
debugBuiltin env cmd =
    let t0 = T.strip cmd
        t1 = maybe t0 T.strip (T.stripPrefix "return " t0)
        t2 = T.strip (fromMaybe t1 (T.stripSuffix ";" t1))
        isQuit = t2 ≡ "engine.quit"
               ∨ case matchCall "engine.quit" t2 of Just _ → True; Nothing → False
    in if isQuit
       -- Handle quit HERE, on the client thread, so the ack is sent before
       -- the Lua thread (which would otherwise answer this command) is torn
       -- down. Round-tripping quit through the thread it's about to kill is
       -- the shutdown race that left the client blocked on the full 30 s
       -- response timeout. Same effect as quitFn: just flip the lifecycle
       -- flag; the main/headless loop drives the actual teardown.
       then writeIORef (lifecycleRef env) CleaningUp ≫ return (Just "shutting down")
       else case matchCall "world.waitForInit" t2 of
           Just arg → Just <$> runWaitForInit env (fromMaybe 600 arg)
           Nothing  → case matchCall "world.waitForChunks" t2 of
               Just arg → Just <$> runWaitForChunks env (fromMaybe 120 arg)
               Nothing  → return Nothing

-- | Match @<fn>(<int?>)@ exactly. @Just Nothing@ = no/empty arg (use the
--   caller's default); @Just (Just n)@ = explicit timeout; @Nothing@ =
--   not this call, or a non-integer arg → fall through to Lua.
matchCall ∷ Text → Text → Maybe (Maybe Int)
matchCall fn t = case T.stripPrefix fn t of
    Nothing → Nothing
    Just rest →
        let r = T.strip rest
        in if not (T.null r) ∧ T.head r ≡ '(' ∧ T.last r ≡ ')'
           then let inner = T.strip (T.init (T.drop 1 r))
                in if T.null inner
                   then Just Nothing
                   else case T.decimal inner of
                          Right (n, rm) | T.null (T.strip rm) → Just (Just n)
                          _                                   → Nothing
           else Nothing

-- | Poll the active world's load phase until done (or timeout), then
--   return the same tab-joined progress 'world.getInitProgress' yields.
runWaitForInit ∷ EngineEnv → Int → IO Text
runWaitForInit env timeoutSec = loop (timeoutSec * 4) ⌦ \_ → fmtInitProgress env
  where
    loop ∷ Int → IO ()
    loop 0 = return ()
    loop n = do
        mWs ← activeWorldState env
        case mWs of
            Just ws → do
                phase ← readIORef (wsLoadPhaseRef ws)
                case phase of
                    LoadDone → return ()
                    _        → threadDelay 250000 ≫ loop (n - 1)
            Nothing → threadDelay 250000 ≫ loop (n - 1)

-- | Poll the active world's init queue until empty (or timeout); return
--   the remaining chunk count (matches 'world.waitForChunks').
runWaitForChunks ∷ EngineEnv → Int → IO Text
runWaitForChunks env timeoutSec = T.pack ∘ show ⊚ loop (timeoutSec * 4)
  where
    remaining ∷ IO Int
    remaining = do
        mWs ← activeWorldState env
        case mWs of
            Just ws → length ⊚ readIORef (wsInitQueueRef ws)
            Nothing → return 0
    loop ∷ Int → IO Int
    loop 0 = remaining
    loop n = do
        r ← remaining
        if r ≡ 0 then return 0 else threadDelay 250000 ≫ loop (n - 1)

-- | Format the active world's load phase as the four tab-separated
--   values 'world.getInitProgress' returns: phase, current, total, stage.
fmtInitProgress ∷ EngineEnv → IO Text
fmtInitProgress env = do
    mWs ← activeWorldState env
    case mWs of
        Just ws → do
            phase ← readIORef (wsLoadPhaseRef ws)
            return $ case phase of
                LoadIdle           → fmt 0 0 0 "idle"
                LoadPhase1 c t     → fmt 1 c t "setup"
                LoadPhase2 rm t    → fmt 2 (t - rm) t "chunks"
                LoadDone           → fmt 3 1 1 "done"
        Nothing → return (fmt 0 0 0 "idle")
  where
    -- Match 'world.getInitProgress' over the console exactly: the stage
    -- string is rendered quoted by 'luaValueToText', so quote it here.
    fmt ∷ Int → Int → Int → Text → Text
    fmt a b c s = T.intercalate "\t" [tshow a, tshow b, tshow c, "\"" <> s <> "\""]
    tshow = T.pack ∘ show

-- | Execute a Lua string and return the result as text.
--   Uses loadstring to compile, then pcall to run safely.
--   Captures return values and any errors.
--   Tables are automatically serialized to JSON format.
executeDebugLua ∷ Lua.State → Text → IO Text
executeDebugLua lst cmdText = Lua.runWith lst $ do
    let code = TE.encodeUtf8 cmdText
        chunkName = Lua.Name ("=" <> code)
    -- Try wrapping in "return ..." first for expressions
    let returnWrapped = "return " <> code
    status ← Lua.loadbuffer returnWrapped chunkName
    status' ← if status ≡ Lua.OK
        then return Lua.OK
        else do
            Lua.pop 1  -- pop error from failed load
            Lua.loadbuffer code chunkName
    case status' of
        Lua.OK → do
            -- Run the loaded chunk with pcall
            callStatus ← Lua.pcall 0 Lua.multret Nothing
            case callStatus of
                Lua.OK → do
                    -- Collect all return values
                    top ← Lua.gettop
                    if top ≡ 0
                        then return "ok"
                        else do
                            parts ← forM [1..top] $ \i →
                                luaValueToText 0 i
                            Lua.settop 0
                            return (T.intercalate "\t" parts)
                _ → do
                    err ← Lua.tostring (-1)
                    Lua.pop 1
                    return $ "error: " <> maybe "unknown" TE.decodeUtf8Lenient err
        _ → do
            err ← Lua.tostring (-1)
            Lua.pop 1
            return $ "syntax error: " <> maybe "unknown" TE.decodeUtf8Lenient err

-- | Debug console command handling for the Lua thread: TCP debug-server
--   builtins (handled off the Lua thread) plus loadstring+pcall execution
--   of ordinary debug-console commands (handled on it).
module Engine.Scripting.Lua.Thread.Console
  ( processDebugCommands
  , debugBuiltin
  , executeDebugLua
  ) where

import UPrelude
import Engine.Scripting.Lua.DebugServer
    ( DebugCommand(..), claimDebugCommand, completeDebugCommand
    , pollDebugCommand )
import Engine.Scripting.Lua.API.Shell (luaValueToText)
import Engine.Core.State
    (EngineEnv(..), EngineLifecycle(..), activeWorldState, chunkTargetWorld)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState, wsLoadPhaseRef, wsInitQueueRef, LoadPhase(..))
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TQueue (TQueue)

-- | Process all pending debug commands from the TCP server.
--   Each command is a line of Lua code. We execute it via
--   loadstring + pcall and send the result back through the MVar.
--
--   Dequeuing is not executing (#2282). A command is run only if
--   'claimDebugCommand' succeeds, which is the single point at which
--   this drain wins the race against a client whose response wait
--   expired, against the load handoff, and against a shutdown drain.
--   A failed claim means the command was cancelled first: it is
--   discarded UNRUN, its reply already belongs to whoever cancelled it,
--   and the drain moves straight on to the next one. That — and not the
--   emptiness of the queue — is what stops a stale command mutating a
--   session whose client was told it had not.
--
--   The answer is published with 'completeDebugCommand', which never
--   blocks: by now the client may already have given up, and the old
--   @putMVar@ on a full-or-abandoned channel would have wedged this
--   single Lua thread.
processDebugCommands ∷ Lua.State → TQueue DebugCommand → IO ()
processDebugCommands lst debugQueue = do
    mCmd ← pollDebugCommand debugQueue
    case mCmd of
        Nothing → return ()
        Just cmd → do
            claimed ← claimDebugCommand cmd
            when claimed $ do
                result ← executeDebugLua lst (dcCommand cmd)
                completeDebugCommand cmd result
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
           Nothing  → case matchWaitForChunks t2 of
               Just (arg, mPage) →
                   Just <$> runWaitForChunks env (fromMaybe 120 arg) mPage
               Nothing  → return Nothing

-- | Match @<fn>(<args>)@ exactly, handing back the raw argument text.
--   @Nothing@ = not this call at all → fall through to the Lua thread.
matchArgs ∷ Text → Text → Maybe Text
matchArgs fn t = case T.stripPrefix fn t of
    Nothing → Nothing
    Just rest →
        let r = T.strip rest
        in if not (T.null r) ∧ T.head r ≡ '(' ∧ T.last r ≡ ')'
           then Just (T.strip (T.init (T.drop 1 r)))
           else Nothing

-- | Match @<fn>(<int?>)@ exactly. @Just Nothing@ = no/empty arg (use the
--   caller's default); @Just (Just n)@ = explicit timeout; @Nothing@ =
--   not this call, or a non-integer arg → fall through to Lua.
matchCall ∷ Text → Text → Maybe (Maybe Int)
matchCall fn t = matchArgs fn t ⌦ decimalArg

-- | Match @world.waitForChunks(<int?>[, '<page>'])@ exactly (#2310), so
--   the optional page argument the Lua binding accepts is served by this
--   off-Lua-thread fast path too rather than falling through to the
--   single Lua thread and blocking it for the whole wait.
--
--   Deliberately conservative: only a bare decimal (or @nil@, or
--   nothing) for the timeout, and only an unescaped single- or
--   double-quoted literal for the page. Anything else — an expression, a
--   variable, an escape — is not this call, and Lua's own parser
--   handles it.
matchWaitForChunks ∷ Text → Maybe (Maybe Int, Maybe WorldPageId)
matchWaitForChunks t = matchArgs "world.waitForChunks" t ⌦ \inner →
    case T.breakOn "," inner of
        (only, rest) | T.null rest → (\n → (n, Nothing)) ⊚ decimalArg only
        (before, rest) → do
            -- An explicit page with the timeout left to the default is
            -- spelled 'nil' in Lua, and belongs on the fast path like
            -- any other recognised form. 'decimalArg' alone does not
            -- accept it, and must not start to: it also serves
            -- 'world.waitForInit', whose accepted spellings are not this
            -- issue's to widen.
            n   ← if T.strip before ≡ "nil"
                    then Just Nothing
                    else decimalArg before
            pid ← quotedPageArg (T.drop 1 rest)
            pure (n, Just pid)

-- | A timeout argument slot: empty means "use the caller's default", a
--   bare decimal names one, anything else is not this call.
decimalArg ∷ Text → Maybe (Maybe Int)
decimalArg raw
    | T.null s  = Just Nothing
    | otherwise = case T.decimal s of
        Right (n, rm) | T.null (T.strip rm) → Just (Just n)
        _                                   → Nothing
  where s = T.strip raw

-- | A single- or double-quoted Lua string literal with no escapes and no
--   embedded quote, read as a page id. Anything else is 'Nothing', which
--   sends the whole command to Lua rather than guessing at its meaning.
quotedPageArg ∷ Text → Maybe WorldPageId
quotedPageArg raw
    | T.length s ≥ 2 ∧ (q ≡ '\'' ∨ q ≡ '"') ∧ T.last s ≡ q
    , body ← T.init (T.drop 1 s)
    , not (T.any (\c → c ≡ '\\' ∨ c ≡ q) body)
    = Just (WorldPageId body)
    | otherwise = Nothing
  where
    s = T.strip raw
    q = if T.null s then ' ' else T.head s

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

-- | Poll ONE page's init queue until empty (or timeout); return the
--   remaining chunk count (matches 'world.waitForChunks' exactly,
--   including its #2310 page binding).
--
--   The target is resolved once, here, and every poll then reads that
--   page's queue. This used to call 'activeWorldState' inside the loop,
--   so a @WorldShow@ landing mid-wait moved the wait onto the incoming
--   page and let it report completion against an empty queue while the
--   outgoing page was still generating — the defect, on the very path
--   the probes actually take (their exact @world.waitForChunks(...)@
--   commands are recognised here and never reach the Lua thread).
runWaitForChunks ∷ EngineEnv → Int → Maybe WorldPageId → IO Text
runWaitForChunks env timeoutSec mPage = do
    mTarget ← chunkTargetWorld mPage env
    case mTarget of
        Nothing      → return "0"
        Just (_, ws) → T.pack ∘ show ⊚ loop ws (timeoutSec * 4)
  where
    remaining ∷ WorldState → IO Int
    remaining ws = length ⊚ readIORef (wsInitQueueRef ws)
    loop ∷ WorldState → Int → IO Int
    loop ws 0 = remaining ws
    loop ws n = do
        r ← remaining ws
        if r ≡ 0 then return 0 else threadDelay 250000 ≫ loop ws (n - 1)

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

-- | Execute a Lua string and return the result as text.
--   Uses loadstring to compile, then pcall to run safely.
--   Captures return values and any errors.
--   Tables are serialized to JSON format by 'luaValueToText'.
--
--   __That serialization is not key-preserving (#1955).__ Distinct Lua
--   keys can convert to one JSON member name — numeric @1@ and string
--   @\"1\"@ are the standard case — so a table is emitted as a JSON
--   object only when its member names come out distinct. One that does
--   not comes back as the JSON /string/ @\"\<duplicate key ...\>\"@
--   rather than as an object silently missing entries, which is what a
--   'json.loads' consumer such as @tools\/probelib.py@'s @send_json@
--   would otherwise be handed. All-string-keyed tables and consecutive
--   @1..n@ arrays — every shape the probes actually consume — are
--   unaffected. 'luaValueToText' states the full contract.
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

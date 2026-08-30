-- | Shared fixture for the two env-driven input-injection specs:
--   'Test.Headless.Input.Followup' (#697/#727/#773 fence and ack
--   mechanics) and 'Test.Headless.Input.InjectOwnership' (#1927 split-
--   hold modifier ownership). Both drive the REAL 'processInputs' and
--   'processLuaMsg' against the live headless 'EngineEnv', playing the
--   input and Lua thread drain loops by hand — the harness starts
--   neither, so both queues belong to the test — and both need the
--   same bootstrapping: a reset input side, a real Lua backend with
--   the full API registered and scripts/input_followup_fixture.lua
--   loaded, and a way to read back what that fixture's callbacks
--   observed AT CALLBACK TIME.
--
--   Extracted verbatim from Followup's own helpers when #1927 added
--   the second spec; duplicating a Lua-backend bootstrapper is exactly
--   how two copies drift.
module Test.Headless.Input.InjectHarness
  ( shiftMod
  , shiftState
  , drainLua
  , resetInput
  , inputTick
  , withFakeInputThread
  , newTestLuaBackend
  , newTestLuaBackendWith
  , readFixtureBool
  , readFixtureBoolIn
  , settleTimeoutMicros
  , isFollowupMsg
  ) where

import UPrelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import qualified HsLua as Lua
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Exception (finally)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Input.Inject (noMods)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Script (loadModuleRef)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaMsg(..), LuaBackendState(..), LuaScript(..))

shiftMod ∷ ([GLFW.Key], GLFW.ModifierKeys)
shiftMod = ([GLFW.Key'LeftShift], noMods { GLFW.modifierKeysShift = True })

-- | The DIRECT owner's own entry for left shift — 'inpKeyStates' read
--   literally, so a 'Nothing' (never touched) stays distinguishable
--   from a recorded release. Deliberately NOT the #1927 published
--   union ('Engine.Input.Types.keyHeld'): the click/tap specs this
--   serves assert the direct owner's own lifecycle.
shiftState ∷ InputState → Maybe Bool
shiftState st = keyPressed ⊚ Map.lookup GLFW.Key'LeftShift (inpKeyStates st)

-- | Drain every queued LuaMsg (nothing else consumes this queue
--   headless; earlier specs may have left worldgen chatter behind).
drainLua ∷ EngineEnv → IO [LuaMsg]
drainLua env = go []
  where
    go acc = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Just msg → go (msg : acc)
            Nothing  → pure (reverse acc)

-- | Reset the input-side state these specs touch and give the click
--   path a non-degenerate viewport (headless boots with zero sizes,
--   which would route every press as swallowed).
resetInput ∷ EngineEnv → IO ()
resetInput env = do
    writeIORef (inputStateRef env) defaultInputState
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)
    _ ← drainLua env
    pure ()

-- | One input-thread tick: drain the input queue through the real
--   'processInputs' (which also publishes to inputStateRef, #697).
inputTick ∷ EngineEnv → IO ()
inputTick env = do
    st ← readIORef (inputStateRef env)
    _ ← processInputs env st
    pure ()

-- | Stand in for the un-started real input thread: pumps the REAL
--   'processInputs' on a tight poll for the duration of the action, so
--   'injectAndSettle's blocking waits on the barrier — which nothing
--   headless would otherwise ever advance — resolve promptly instead
--   of running out their full timeout.
withFakeInputThread ∷ EngineEnv → IO α → IO α
withFakeInputThread env act = do
    stopRef ← newIORef False
    let pump = do
            stop ← readIORef stopRef
            unless stop $ do
                st ← readIORef (inputStateRef env)
                _ ← processInputs env st
                threadDelay 500
                pump
    tid ← forkIO pump
    act `finally` (writeIORef stopRef True ≫ killThread tid)

-- | A real Lua backend + thread-control ref, with the FULL Lua API
--   registered (so @engine.isKeyDown@ etc. exist) and
--   scripts/input_followup_fixture.lua loaded as its one script — the
--   REAL 'broadcastToModules' dispatch path (#727 review: a
--   script-less backend proves the queue/timing mechanics but can
--   never show a real callback observing callback-time state, since
--   broadcasting to zero scripts is a no-op). Returns the fixture's
--   module ref so callers can read back what its callbacks captured
--   via 'readFixtureBool'.
newTestLuaBackend ∷ EngineEnv → IO (LuaBackendState, IORef ThreadControl, Lua.Reference)
newTestLuaBackend env = newTestLuaBackendWith env env

-- | 'newTestLuaBackend', but the Lua API is registered against
--   @apiEnv@ rather than the env owning the queues. #1927's Lua-
--   boundary specs need a copy whose 'ecHeadless' is False — the
--   input.* verbs refuse to run at all under a headless config, so an
--   argument-level contract is unreachable through the real verb
--   otherwise — while every queue, ref and state the verb touches
--   stays the harness's own live env.
newTestLuaBackendWith ∷ EngineEnv → EngineEnv
                      → IO (LuaBackendState, IORef ThreadControl, Lua.Reference)
newTestLuaBackendWith apiEnv env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) apiEnv ls stateRef
    eRef ← Lua.runWith (lbsLuaState ls) $
        loadModuleRef "scripts/input_followup_fixture.lua"
    ref ← case eRef of
        Right r → pure r
        Left err → error $
            "failed to load scripts/input_followup_fixture.lua: "
            ⧺ T.unpack err
    atomically $ modifyTVar' (lbsScripts ls) $ Map.insert 1 LuaScript
        { scriptId        = 1
        , scriptPath      = "scripts/input_followup_fixture.lua"
        , scriptTickRate  = 1000000  -- never auto-ticks during the test
        , scriptNextTick  = 1000000
        , scriptModuleRef = ref
        , scriptPaused    = False
        }
    pure (ls, stateRef, ref)

-- | Read a boolean field off the fixture's @M.state@ table — what
--   scripts/input_followup_fixture.lua's callbacks captured via
--   @engine.isKeyDown@ at THEIR call time, i.e. what a real Lua
--   callback actually observed (#727 review), not a proxy.
--   'Nothing' if the field is absent/not a boolean (e.g. the callback
--   hasn't fired yet — the fixture leaves it @nil@).
readFixtureBool ∷ LuaBackendState → Lua.Reference → BS.ByteString → IO (Maybe Bool)
readFixtureBool ls ref field = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.getref Lua.registryindex ref ∷ Lua.LuaE Lua.Exception Lua.Type
    tyState ← Lua.getfield (-1) (Lua.Name "state")
    result ← if tyState ≡ Lua.TypeTable
        then do
            tyField ← Lua.getfield (-1) (Lua.Name field)
            r ← if tyField ≡ Lua.TypeBoolean
                    then Just ⊚ Lua.toboolean (-1)
                    else pure Nothing
            Lua.pop 1
            pure r
        else pure Nothing
    Lua.pop 2
    pure result

-- | 'readFixtureBool' one level deeper: @M.state[table][key]@. The
--   per-key tables exist because a single flat field cannot survive a
--   sequence that fires the SAME callback twice (#1927: a key split
--   hold's up half broadcasts @onKeyUp@ for the primary key and then,
--   behind the fence, for the released modifier — a flat field would
--   only ever report the second one).
readFixtureBoolIn ∷ LuaBackendState → Lua.Reference → BS.ByteString
                  → BS.ByteString → IO (Maybe Bool)
readFixtureBoolIn ls ref table key = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.getref Lua.registryindex ref ∷ Lua.LuaE Lua.Exception Lua.Type
    tyState ← Lua.getfield (-1) (Lua.Name "state")
    result ← if tyState ≡ Lua.TypeTable
        then do
            tyTable ← Lua.getfield (-1) (Lua.Name table)
            r ← if tyTable ≡ Lua.TypeTable
                    then do
                        tyField ← Lua.getfield (-1) (Lua.Name key)
                        v ← if tyField ≡ Lua.TypeBoolean
                                then Just ⊚ Lua.toboolean (-1)
                                else pure Nothing
                        Lua.pop 1
                        pure v
                    else pure Nothing
            Lua.pop 1
            pure r
        else pure Nothing
    Lua.pop 2
    pure result

-- | Timeout for 'injectAndSettle' calls — generous relative to the
--   500us fake-pump poll interval above.
settleTimeoutMicros ∷ Int
settleTimeoutMicros = 5 * 1000 * 1000

isFollowupMsg ∷ LuaMsg → Bool
isFollowupMsg (LuaInjectFollowup _) = True
isFollowupMsg _                     = False

{-# LANGUAGE ScopedTypeVariables #-}
-- | \"pause preserves the chosen world speed\" (#1599).
--
--   @scripts\/pause.lua@ promises that a chosen fast-forward survives a
--   pause cycle and resumes at the speed the player chose. It used to
--   keep that promise only for a pause Lua itself imposed: every
--   engine-side writer of the pause flag — a @pause: true@ notification
--   category, @engine.saveWorld@'s acceptance, the world thread's
--   re-assertion, a load publish — ran no Lua at all, so the resume
--   branch handed back whatever the last Lua-imposed pause had recorded
--   (1.0 in an ordinary session).
--
--   These examples drive the REAL engine and the REAL
--   @scripts\/pause.lua@ against it: pages are live 'WorldState's in the
--   live world manager, the pause comes from the production writer under
--   test, and the resume goes through @pause.toggle@ \/ @pause.set(false)@
--   on a Lua state carrying the full registered API. The observables are
--   the engine's own: 'enginePausedRef', the page's 'wsTimeScaleRef' and
--   the pause epoch's 'wsResumeScaleRef'.
--
--   __What is deliberately NOT driven end to end here.__
--   @engine.saveWorld@ itself cannot run in this harness: it blocks on
--   'Engine.Save.Barrier.waitForOwners' for @SaveUnit@, @SaveBuilding@,
--   @SaveCombat@ and @SaveSimulation@, none of whose threads the headless
--   harness starts, so every call would time out after five seconds
--   regardless of the behaviour under test
--   ("Test.Headless.Save.AutosaveGuards" avoids it for the same reason).
--   The manual-save examples therefore enter at 'acceptSaveRequest' —
--   the function @engine.saveWorld@ calls to impose its pause, and the
--   only place in that path where the speed is captured or lost. The
--   whole-verb proof (@engine.saveWorld@ returning true and the
--   transaction reaching a terminal outcome before the resume) belongs
--   to @tools\/save_pause_probe.py@, which boots a real process with
--   every owner thread live.
module Test.Headless.World.PauseSpeed (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (readMVar)
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.Capability.Events (toEventsCapability, EventsCapability(..))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Load.Status (beginLoad, failLoad)
import Engine.PlayerEvent (CategoryCfg(..))
import Engine.PlayerEvent.Emit (emitEvent)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Save (acceptSaveRequest)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Pause (beginPauseEpoch, imposePause, releasePause, setPauseResumeScale)
import World.Types

-- | The active page every example fast-forwards, and the bystander page
--   requirement 8 is about.
pageA, pageB ∷ WorldPageId
pageA = WorldPageId "pausespeed_active"
pageB = WorldPageId "pausespeed_bystander"

-- | The bystander's speed. Distinct from every scale the active page is
--   ever given, so \"B still reads its own sentinel\" cannot pass by
--   coincidence.
bystanderScale ∷ Float
bystanderScale = 3

-- | Install a two-page session: @pageA@ visible (so it is
--   @world.getActiveWorldId()@ and the page a pause epoch takes hold
--   of), @pageB@ registered but not visible.
--
--   Both pages are bare 'emptyWorldState's with NO generation
--   parameters, which is what keeps the harness's real world worker away
--   from them: every page walk it runs skips a page whose
--   'wsGenParamsRef' is 'Nothing'. The worker still has to be running,
--   because @world.setTimeScale@ is a queued command and one example
--   needs it drained.
installSession ∷ EngineEnv → Float → IO (WorldState, WorldState)
installSession env activeScale = do
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    writeIORef (wsTimeScaleRef wsA) activeScale
    writeIORef (wsTimeScaleRef wsB) bystanderScale
    writeIORef (worldManagerRef env) WorldManager
        { wmWorlds  = [(pageA, wsA), (pageB, wsB)]
        , wmVisible = [pageA] }
    writeIORef (enginePausedRef env) False
    pure (wsA, wsB)

-- | A Lua state with the full engine API registered, ready to
--   @require(\"scripts.pause\")@ — the same registration the real Lua
--   thread performs, so @engine.setPaused@, @world.getTimeScale@ and
--   @world.getActiveWorldId@ are the production bindings.
newBackend ∷ EngineEnv → IO LuaBackendState
newBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run a chunk on the backend's canonical state, failing the example
--   with Lua's own message on error.
runLua_ ∷ LuaBackendState → BS.ByteString → IO ()
runLua_ ls src = do
    result ← Lua.runWith (lbsLuaState ls) $ do
        status ← Lua.dostring src
        case status of
            Lua.OK → pure Nothing
            _ → do
                err ← Lua.tostring (-1)
                Lua.pop 1
                pure (Just (maybe "unknown error" TE.decodeUtf8Lenient err))
    forM_ result $ \msg → expectationFailure (T.unpack msg)

-- | Read one number back out of Lua.
luaNumber ∷ LuaBackendState → BS.ByteString → IO (Maybe Double)
luaNumber ls expr = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.dostring ("return " <> expr)
    n ← Lua.tonumber (-1)
    Lua.pop 1
    pure ((\(Lua.Number d) → d) <$> n)

-- | Read one boolean back out of Lua.
luaBool ∷ LuaBackendState → BS.ByteString → IO Bool
luaBool ls expr = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.dostring ("return " <> expr)
    b ← Lua.toboolean (-1)
    Lua.pop 1
    pure b

-- | Emit through the real notification path under a category whose
--   @pause: true@ default is what makes it flip the engine flag.
pauseByNotification ∷ EngineEnv → IO ()
pauseByNotification env =
    emitEvent env "survival_critical" "Test.PauseSpeed" "a critical event"

-- | Wait (briefly) for a condition the world thread has to satisfy —
--   used only for @world.setTimeScale@, which is queued rather than
--   applied in place.
waitUntil ∷ String → IO Bool → IO ()
waitUntil label check = go (300 ∷ Int)
  where
    go 0 = expectationFailure ("timed out waiting for " ⧺ label)
    go n = do
        ok ← check
        unless ok (threadDelay 10000 >> go (n - 1))

spec ∷ SpecWith EngineEnv
spec = describe "pause preserves the chosen world speed (#1599)" $ do

    it "a pause: true notification captures the active page's speed and \
       \the player's own resume gives it back" $ \env → do
        (wsA, wsB) ← installSession env 10
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"

        pauseByNotification env

        -- Requirement 3: the pair is coherent while paused. Asserted on
        -- the notification's own result, with no pause.set(true) run as
        -- a healing step first.
        readIORef (enginePausedRef env) `shouldReturn` True
        readIORef (wsTimeScaleRef wsA) `shouldReturn` 0
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 10
        -- Requirement 8: the bystander is untouched, epoch included.
        readIORef (wsTimeScaleRef wsB) `shouldReturn` bystanderScale
        readIORef (wsResumeScaleRef wsB) `shouldReturn` Nothing

        -- Requirement 1: resume the way the player does — Space reaches
        -- pause.toggle (scripts/init_keys.lua).
        runLua_ ls "require('scripts.pause').toggle()"

        readIORef (enginePausedRef env) `shouldReturn` False
        readIORef (wsTimeScaleRef wsA) `shouldReturn` 10
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Nothing
        readIORef (wsTimeScaleRef wsB) `shouldReturn` bystanderScale

    it "re-imposing a pause on an already-paused session never replaces \
       \the captured speed with the zero it reads" $ \env → do
        (wsA, _) ← installSession env 10
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"

        pauseByNotification env
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 10

        -- Requirement 4, driven through three different re-imposers:
        -- a second notification, a save acceptance, and the module's own
        -- pause.set(true) — each of which finds the clock already at 0.
        pauseByNotification env
        mgr ← readIORef (worldManagerRef env)
        _ ← acceptSaveRequest env mgr False
        runLua_ ls "require('scripts.pause').set(true)"

        readIORef (wsTimeScaleRef wsA) `shouldReturn` 0
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 10

        runLua_ ls "require('scripts.pause').set(false)"
        readIORef (wsTimeScaleRef wsA) `shouldReturn` 10

    it "a manual save's acceptance captures the speed it zeroes, and the \
       \world thread's later re-assertion leaves it alone" $ \env → do
        (wsA, wsB) ← installSession env 7
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"

        -- Requirement 2. This is the function engine.saveWorld calls to
        -- impose its pause; see the module haddock for why the whole
        -- verb cannot run in this harness.
        mgr ← readIORef (worldManagerRef env)
        req ← acceptSaveRequest env mgr False
        req `shouldBe` Nothing   -- a MANUAL save carries no AutosaveRequest

        readIORef (enginePausedRef env) `shouldReturn` True
        readIORef (wsTimeScaleRef wsA) `shouldReturn` 0
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 7

        -- World.Thread.Command.Save.WriteWorld re-asserts the pause when
        -- the queued WorldSave reaches it; that must be inert.
        imposePause (toWorldSimCapability env)
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 7

        runLua_ ls "require('scripts.pause').set(false)"
        readIORef (wsTimeScaleRef wsA) `shouldReturn` 7
        readIORef (wsTimeScaleRef wsB) `shouldReturn` bystanderScale

    it "resuming restores the page the pause epoch took hold of, not \
       \whichever page is active by then" $ \env → do
        (wsA, wsB) ← installSession env 10
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"

        pauseByNotification env

        -- Requirement 8's non-vacuous half: make the BYSTANDER active
        -- before resuming. A resume that retimed "the current world"
        -- would now write pageB.
        modifyIORef' (worldManagerRef env) $ \m → m { wmVisible = [pageB] }
        runLua_ ls "require('scripts.pause').set(false)"

        readIORef (wsTimeScaleRef wsA) `shouldReturn` 10
        readIORef (wsTimeScaleRef wsB) `shouldReturn` bystanderScale
        readIORef (wsResumeScaleRef wsB) `shouldReturn` Nothing

    it "a published load resumes at the default speed, and \
       \pause.onSaveLoaded is what pins it there" $ \env → do
        (wsOld, _) ← installSession env 10
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"

        -- engine.loadSave pauses the OUTGOING session at acceptance.
        imposePause (toWorldSimCapability env)
        readIORef (wsResumeScaleRef wsOld) `shouldReturn` Just 10

        -- World.Load.Publish replaces the session and starts a fresh
        -- epoch over the new active page, whose clock is the 1.0 every
        -- loaded page comes up at (time scale is never persisted).
        wsNew ← emptyWorldState
        writeIORef (worldManagerRef env) WorldManager
            { wmWorlds = [(pageA, wsNew)], wmVisible = [pageA] }
        beginPauseEpoch (toWorldSimCapability env)
        readIORef (wsTimeScaleRef wsNew) `shouldReturn` 0
        readIORef (wsResumeScaleRef wsNew) `shouldReturn` Just 1

        -- Poison the epoch so the assertion below cannot pass merely
        -- because the publish already happened to record 1.0: what is
        -- under test is that onSaveLoaded RESETS the resume speed, which
        -- is requirement 5's load policy.
        setPauseResumeScale wsNew 9
        runLua_ ls "require('scripts.pause').onSaveLoaded({}, {})"
        waitUntil "pause.onSaveLoaded's queued world.setTimeScale" $
            (≡ Just 1) <$> readIORef (wsResumeScaleRef wsNew)

        -- Still coherently paused while it does so.
        readIORef (enginePausedRef env) `shouldReturn` True
        readIORef (wsTimeScaleRef wsNew) `shouldReturn` 0

        runLua_ ls "require('scripts.pause').set(false)"
        readIORef (wsTimeScaleRef wsNew) `shouldReturn` 1

    it "scripts.pause.set(false) rejected mid-load leaves the flag, the \
       \Lua mirror, the captured speed and the page clock untouched" $ \env → do
        (wsA, _) ← installSession env 10
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"

        pauseByNotification env
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 10

        mirrorBefore ← luaBool ls "require('scripts.pause').paused"
        prevBefore ← luaNumber ls "require('scripts.pause').prevTimeScale"

        Right reqId ← beginLoad (loadStatusRef env) "pausespeed_gate"
        runLua_ ls "require('scripts.pause').set(false)"

        -- #763's "nothing changed" contract, through the real module
        -- rather than the raw binding.
        readIORef (enginePausedRef env) `shouldReturn` True
        readIORef (wsTimeScaleRef wsA) `shouldReturn` 0
        readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 10
        luaBool ls "require('scripts.pause').paused" `shouldReturn` mirrorBefore
        luaNumber ls "require('scripts.pause').prevTimeScale"
            `shouldReturn` prevBefore

        failLoad (loadStatusRef env) reqId "test cleanup"
        runLua_ ls "require('scripts.pause').set(false)"
        readIORef (wsTimeScaleRef wsA) `shouldReturn` 10

    it "engine-imposed pauses do not advance the player-intent \
       \generation, and an applied player pause still does" $ \env → do
        _ ← installSession env 10
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"

        genStart ← readMVar (playerIntentGenRef env)
        pauseByNotification env
        releasePause (toWorldSimCapability env)
        mgr ← readIORef (worldManagerRef env)
        _ ← acceptSaveRequest env mgr False
        readMVar (playerIntentGenRef env) `shouldReturn` genStart

        releasePause (toWorldSimCapability env)
        runLua_ ls "require('scripts.pause').set(true)"
        genAfterPlayer ← readMVar (playerIntentGenRef env)
        genAfterPlayer `shouldSatisfy` (> genStart)

    it "an autosave restored to a non-default speed, then paused by a \
       \pause-configured save_load event, resumes to that speed" $ \env → do
        (wsA, _) ← installSession env 12
        ls ← newBackend env
        runLua_ ls "require('scripts.pause')"
        withPausingCategory env "save_load" $ do
            mgr ← readIORef (worldManagerRef env)
            req ← acceptSaveRequest env mgr True
            arPreScale req `shouldBe` Just 12

            -- The two writes World.Thread.Command.Save.WriteWorld's
            -- restoreAfterAutosave performs once the transaction has
            -- succeeded and the player has stayed idle: hand the
            -- pre-request speed to the epoch, then close it.
            forM_ (arPreScale req) $ setPauseResumeScale wsA
            releasePause (toWorldSimCapability env)
            readIORef (wsTimeScaleRef wsA) `shouldReturn` 12

            -- The success event fires next, and this category is
            -- configured to pause on it. Its result is authoritative —
            -- and it must capture the speed the restore just handed back.
            emitEvent env "save_load" "Test.PauseSpeed" "Game saved: x"
            readIORef (enginePausedRef env) `shouldReturn` True
            readIORef (wsTimeScaleRef wsA) `shouldReturn` 0
            readIORef (wsResumeScaleRef wsA) `shouldReturn` Just 12

            runLua_ ls "require('scripts.pause').set(false)"
            readIORef (wsTimeScaleRef wsA) `shouldReturn` 12

-- | The pre-request scale an 'acceptSaveRequest' result carries, without
--   importing the DTO's field name into every example.
arPreScale ∷ Maybe AutosaveRequest → Maybe Float
arPreScale = fmap arPreTimeScale

-- | Run @body@ with one notification category temporarily configured to
--   pause, restoring the session's real registry afterwards.
withPausingCategory ∷ EngineEnv → Text → IO α → IO α
withPausingCategory env catId body = do
    let ref = ecNotificationCfgRef (toEventsCapability env)
    before ← readIORef ref
    writeIORef ref (HM.adjust (\c → c { ccPause = True }) catId before)
    body `finally` writeIORef ref before

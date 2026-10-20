{-# LANGUAGE Strict #-}
-- | Shared exclusion for "Scenario.Capture" (#2714, D-48 / D-51): a
--   capture, an ordinary save, an autosave, an ordinary load, a scenario
--   load and an unpause admit at most one at a time; a conflicting request
--   is answered immediately and changes nothing about the operation that
--   holds the line, queues nothing, and the simulation stays paused.
--
--   Every example holds a request deterministically at a named seam — never
--   a sleep — so the competing requests are made against a request that is
--   genuinely in flight, in the stage the example names.
module Test.Headless.Scenario.Capture.Exclusion (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (AsyncException(..), fromException)
import Data.IORef (readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.InputView (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
import Engine.Save.Barrier
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsgs)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Scenario.Capture.Service (requestScenarioCapture)
import Scenario.Capture.Types
import Scenario.Load.Service (requestScenarioLoad)
import Scenario.Load.Types
import Test.Headless.Scenario.Capture.Support
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Load.Persist (integratedDoc)
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import World.Command.Types (WorldCommand(..))

spec ∷ SpecWith H
spec = describe "shared exclusion" $ do

    it "rejects every conflicting operation at once while a capture is held inside the capture window" $ \h → do
        let env = fxEnv (hFx h)
            sl = toSaveLoadCapability env
            lsr = slLoadStatusRef sl
            barrier = slSaveBarrierRef sl
        path ← writeScenario "capture-excl-held.yaml" integratedDoc
        reloadFrom h path
        dir ← freshDir "excl-held"
        let dest = dir </> "held.yaml"
        hold ← newHold
        armCaptureHold h CpBoundary hold
        withPump h $ do
            n ← requestCapture h dest False
            takeMVar (holdReached hold)
            -- the request is accepted, paused, and not finished
            activeBefore ← captureStatusOf h
            barrierBefore ← readSaveStatus barrier
            loadBefore ← readLoadStatus lsr
            pausedBefore ← readIORef (enginePausedRef env)
            pausedBefore `shouldBe` True
            fmap cstStage activeBefore `shouldBe` Just CaptureSnapshotting
            -- 1. another capture, and 2. a scenario load: refused at once with
            --    NOTHING queued behind the active request, on either queue
            luaQ ← Q.queueStats (ivLuaQueue (toInputViewCapability env))
            worldQ ← Q.queueStats (wsWorldQueue (toWorldSimCapability env))
            requestScenarioCapture env (CaptureRequest (dir </> "second.yaml") False) `shouldReturn` Left BusySave
            requestScenarioLoad env (SourceFile path) `shouldReturn` Left BusySave
            luaQ' ← Q.queueStats (ivLuaQueue (toInputViewCapability env))
            worldQ' ← Q.queueStats (wsWorldQueue (toWorldSimCapability env))
            Q.qsEnqueued luaQ' `shouldBe` Q.qsEnqueued luaQ
            Q.qsEnqueued worldQ' `shouldBe` Q.qsEnqueued worldQ
            -- 3. an ordinary load
            lua h "return tostring(engine.loadSave('anything'))" `shouldReturn` "false"
            -- 4. an ordinary save, and 5. an autosave
            lua h "return tostring(engine.saveWorld('test_arena', 'manual-x'))" `shouldReturn` "false"
            lua h "return tostring(engine.saveWorld('test_arena', 'autosave_incoming', {autosave = true}))"
                `shouldReturn` "false"
            -- 6. an unpause, through the Lua verb
            lua h "return tostring(engine.setPaused(false))" `shouldReturn` "false"
            readIORef (enginePausedRef env) `shouldReturn` True
            lua h "return tostring(engine.isPaused())" `shouldReturn` "true"
            -- nothing about the active request moved, and nothing was queued
            captureStatusOf h `shouldReturn` activeBefore
            readSaveStatus barrier `shouldReturn` barrierBefore
            readLoadStatus lsr `shouldReturn` loadBefore
            -- the Lua verbs' refusals queued no request either (they never touch the world queue)
            worldQ'' ← Q.queueStats (wsWorldQueue (toWorldSimCapability env))
            Q.qsEnqueued worldQ'' `shouldBe` Q.qsEnqueued worldQ
            doesFileExist (dir </> "second.yaml") `shouldReturn` False
            -- release: the SAME request finishes, with the identity it was given
            putMVar (holdRelease hold) ()
            s ← awaitCapture h n 60
            disarmCapture h
            cstRequestId s `shouldBe` n
            void (captureOf s)
            doesFileExist dest `shouldReturn` True
        -- paused through the whole capture and after it
        readIORef (enginePausedRef env) `shouldReturn` True
        lua h "return tostring(engine.isPaused())" `shouldReturn` "true"

    it "refuses an unpause through storage too, and allows it once the capture has ended" $ \h → do
        let env = fxEnv (hFx h)
        path ← writeScenario "capture-excl-store.yaml" integratedDoc
        reloadFrom h path
        dir ← freshDir "excl-store"
        hold ← newHold
        -- held after the capture lock was released: the owners run again, the
        -- bytes are immutable, and the operation still holds the line
        armCaptureHold h CpReleased hold
        withPump h $ do
            n ← requestCapture h (dir </> "stored.yaml") False
            takeMVar (holdReached hold)
            fmap cstStage <$> captureStatusOf h `shouldReturn` Just CaptureStoring
            lua h "return tostring(engine.setPaused(false))" `shouldReturn` "false"
            readIORef (enginePausedRef env) `shouldReturn` True
            requestScenarioCapture env (CaptureRequest (dir </> "again.yaml") False) `shouldReturn` Left BusySave
            lua h "return tostring(engine.saveWorld('test_arena', 'during-store'))" `shouldReturn` "false"
            putMVar (holdRelease hold) ()
            s ← awaitCapture h n 60
            disarmCapture h
            void (captureOf s)
        -- ended: the arena is still paused, and a script may now resume it
        readIORef (enginePausedRef env) `shouldReturn` True
        lua h "return tostring(engine.setPaused(false))" `shouldReturn` "true"
        readIORef (enginePausedRef env) `shouldReturn` False
        lua h "return tostring(engine.setPaused(true))" `shouldReturn` "true"

    it "refuses an unpause while the request still waits for the Lua thread" $ \h → do
        let env = fxEnv (hFx h)
        path ← writeScenario "capture-excl-lua.yaml" integratedDoc
        reloadFrom h path
        dir ← freshDir "excl-lua"
        hold ← newHold
        armCaptureHold h CpPreflight hold
        withPump h $ do
            n ← requestCapture h (dir </> "lua.yaml") False
            takeMVar (holdReached hold)
            fmap cstStage <$> captureStatusOf h `shouldReturn` Just CapturePreflight
            lua h "return tostring(engine.setPaused(false))" `shouldReturn` "false"
            putMVar (holdRelease hold) ()
            s ← awaitCapture h n 60
            disarmCapture h
            void (captureOf s)
        readIORef (enginePausedRef env) `shouldReturn` True

    it "pauses synchronously at acceptance, before any coordination" $ \h → do
        let env = fxEnv (hFx h)
        path ← writeScenario "capture-excl-accept.yaml" integratedDoc
        reloadFrom h path
        dir ← freshDir "excl-accept"
        -- a running arena, as a player left it
        writeIORef (enginePausedRef env) False
        s0 ← requestScenarioCapture env (CaptureRequest (dir </> "accepted.yaml") False)
        n ← either (\b → fail ("refused: " ⧺ show b)) pure s0
        -- nothing has been pumped: accepted, paused, and not started
        readIORef (enginePausedRef env) `shouldReturn` True
        fmap cstStage <$> captureStatusOf h `shouldReturn` Just CaptureAdmitted
        s ← withPump h (awaitCapture h n 60)
        void (captureOf s)
        readIORef (enginePausedRef env) `shouldReturn` True

    it "refuses a capture while a scenario load, an ordinary load or a save holds the line" $ \h → do
        let env = fxEnv (hFx h)
            sl = toSaveLoadCapability env
        path ← writeScenario "capture-excl-by.yaml" (doc ["units: []"])
        reloadFrom h path
        dir ← freshDir "excl-by"
        let req = CaptureRequest (dir </> "refused.yaml") False
        -- a scenario load in flight
        hold ← newHold
        armSeams h (holdAt PointPreflight hold)
        src ← SourceFile <$> writeScenario "capture-excl-by-load.yaml" (doc ["units: []"])
        n ← request h src
        pump ← startPump h
        takeMVar (holdReached hold)
        loadBefore ← statusOf h
        requestScenarioCapture env req `shouldReturn` Left BusyLoad
        statusOf h `shouldReturn` loadBefore
        putMVar (holdRelease hold) ()
        _ ← await h n 60
        stopPump pump
        disarm h
        -- an ordinary load in flight
        Right lid ← claimLoad (slLoadStatusRef sl) (saveInProgress (slSaveBarrierRef sl)) "held-load"
        requestScenarioCapture env req `shouldReturn` Left BusyLoad
        finishLoad (slLoadStatusRef sl) lid
        -- an ordinary save (or autosave) in flight
        Right sid ← beginSave (slSaveBarrierRef sl) (Set.fromList [SaveLua])
        saveBefore ← readSaveStatus (slSaveBarrierRef sl)
        requestScenarioCapture env req `shouldReturn` Left BusySave
        readSaveStatus (slSaveBarrierRef sl) `shouldReturn` saveBefore
        finishSave (slSaveBarrierRef sl) sid
        -- none of them wrote anything, and once they end the same request is admitted
        doesFileExist (dir </> "refused.yaml") `shouldReturn` False
        s ← runCaptureWith h req
        void (captureOf s)

    it "keeps a finished capture's result until the next accepted operation replaces it" $ \h → do
        let env = fxEnv (hFx h)
            sl = toSaveLoadCapability env
        path ← writeScenario "capture-excl-kept.yaml" (doc ["units: []"])
        reloadFrom h path
        dir ← freshDir "excl-kept"
        s1 ← runCapture h (dir </> "kept.yaml") False
        captureStatusOf h `shouldReturn` Just s1
        -- a refused request is not an operation
        Right sid ← beginSave (slSaveBarrierRef sl) (Set.fromList [SaveLua])
        captureStatusOf h `shouldReturn` Nothing
        finishSave (slSaveBarrierRef sl) sid
        -- (a newer barrier transaction has superseded it: no stale result is
        -- ever reported under the newer operation's identity)
        s2 ← runCapture h (dir </> "kept2.yaml") False
        cstRequestId s2 `shouldSatisfy` (> cstRequestId s1)
        captureStatusOf h `shouldReturn` Just s2

    it "ignores a command that outlived its transaction: a stale completion can neither finish nor overwrite" $ \h → do
        let env = fxEnv (hFx h)
        path ← writeScenario "capture-excl-stale.yaml" integratedDoc
        reloadFrom h path
        dir ← freshDir "excl-stale"
        s1 ← runCapture h (dir </> "real.yaml") False
        realBytes ← BS.readFile (dir </> "real.yaml")
        -- the world thread's and the Lua thread's turns of a request that has
        -- ended, and of one that never existed, with other destinations
        let stale d = CaptureRequest (dir </> d) True
        withPump h $ do
            Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldScenarioCapture (cstRequestId s1) (stale "stale-same.yaml"))
            Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldScenarioCapture 99999 (stale "stale-never.yaml"))
            Q.writeQueue (ivLuaQueue (toInputViewCapability env)) (LuaScenarioCapture (cstRequestId s1) (stale "stale-lua.yaml"))
            Q.writeQueue (ivLuaQueue (toInputViewCapability env)) (LuaScenarioCapture 99999 (stale "stale-lua-never.yaml"))
            -- let both threads take their turns
            threadDelay 200000
        forM_ ["stale-same.yaml", "stale-never.yaml", "stale-lua.yaml", "stale-lua-never.yaml"] $ \f →
            doesFileExist (dir </> f) `shouldReturn` False
        BS.readFile (dir </> "real.yaml") `shouldReturn` realBytes
        captureStatusOf h `shouldReturn` Just s1
        -- and the line is free
        loadInProgress (slLoadStatusRef (toSaveLoadCapability env)) `shouldReturn` False
        saveInProgress (slSaveBarrierRef (toSaveLoadCapability env)) `shouldReturn` False

    it "lets a thread cancellation through instead of reporting it as a failed capture" $ \h → do
        let env = fxEnv (hFx h)
            sl = toSaveLoadCapability env
        path ← writeScenario "capture-excl-async.yaml" integratedDoc
        reloadFrom h path
        dir ← freshDir "excl-async"
        hold ← newHold
        armCaptureHold h CpPreflight hold
        done ← newEmptyMVar
        pumpThread ← forkFinally
            (forever (processLuaMsgs env (fxLua (hFx h)) (hCtl h) >> threadDelay 500))
            (putMVar done)
        n ← requestCapture h (dir </> "killed.yaml") False
        takeMVar (holdReached hold)
        -- shutdown reaches the thread the request is running on
        killThread pumpThread
        outcome ← takeMVar done
        case outcome of
            Left e → (fromException e ∷ Maybe AsyncException) `shouldBe` Just ThreadKilled
            Right () → expectationFailure "the pump ended without an exception"
        disarmCapture h
        -- the cancellation was not turned into a failure of the request: it is
        -- exactly as it was when it stopped (a shutdown owes it no result)
        fmap cstStage <$> captureStatusOf h `shouldReturn` Just CapturePreflight
        fmap (isNothing ∘ cstOutcome) <$> captureStatusOf h `shouldReturn` Just True
        doesFileExist (dir </> "killed.yaml") `shouldReturn` False
        -- clean up what a restarted engine would simply never have: end the
        -- abandoned transaction so the shared line is free for the next example
        terminateCapture (slLoadStatusRef sl) n
            (CaptureUnsuccessful (CaptureFailure CapturePreflight FailCoordination Nothing "" NotPublished "abandoned by the example" [] []))
        failSave (slSaveBarrierRef sl) n "abandoned by the example"

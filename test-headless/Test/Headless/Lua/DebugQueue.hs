-- | The 'LuaSaveLoaded' stale-debug-command gate (round 10 review,
--   issue #763): the debug-console TCP server keeps accepting commands
--   onto 'lbsDebugQueue' regardless of the save barrier's capture-lock
--   state, so a command issued while a load transaction holds that
--   boundary can still be sitting in the queue once the transaction
--   publishes and 'Engine.Scripting.Lua.Thread.Dispatch' processes the
--   'LuaSaveLoaded' reconciliation message — at which point it targets
--   a session that no longer exists. 'processLuaMsg' must cancel any
--   such command right there (resolving its response 'MVar' rather
--   than leaving the caller hanging or, worse, letting a LATER
--   'processDebugCommands' call execute it against the replacement
--   session). No real load transaction is driven here — that full
--   engine.saveWorld / engine.loadSave round trip is
--   tools/transactional_load_probe.py's job; this is a direct,
--   isolated call against 'LuaSaveLoaded', mirroring how
--   "Test.Headless.Input.Followup" drives 'processLuaMsg' directly.
module Test.Headless.Lua.DebugQueue (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (tryTakeMVar)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Concurrent.STM.TQueue (newTQueue, writeTQueue, tryReadTQueue)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Load.Status
    ( LoadPhase(..), LoadOutcome(..), LoadStatus(..)
    , ReconciliationFailure(..), beginLoad, readLoadStatus, loadInProgress )
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.DebugServer
    ( DebugCommand(..), DebugCommandState(..), claimDebugCommand
    , newDebugCommand, readDebugCommandState )
import Engine.Scripting.Lua.Thread (createLuaBackendState, drainDebugQueue)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..), LuaScript(..))
-- Issue #1589: the reconciliation context 'LuaSaveLoaded' now carries.
-- These cases drive the DISPATCHER, not any reconciling module, so the
-- honest value is a real empty session's context.
import World.Save.Payload (emptyLoadReconcileContext)

-- | A bare Lua backend (full API registered, no script loaded — the
--   'LuaSaveLoaded' handler's 'broadcastToModules' call is a no-op
--   with zero scripts, which is fine here since this test only cares
--   about the debug-queue side effect), with its own private debug
--   queue swapped in so the test can push fake commands onto it and
--   read back what happened to their response 'MVar's.
newBareBackendWithDebugQueue ∷ EngineEnv → IO LuaBackendState
newBareBackendWithDebugQueue env = do
    ls0 ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                 (assetPoolRef env) (nextObjectIdRef env)
                                 (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls0) env ls0 stateRef
    privateQueue ← atomically newTQueue
    pure ls0 { lbsDebugQueue = privateQueue }

-- | Register one real Lua module against @ls@ under @scriptId@ and
--   @path@, its module table produced by evaluating @chunk@ (which must
--   @return@ a table, exactly like a script file loaded through
--   'Engine.Scripting.Lua.Script.loadModuleRef' — the only difference
--   is where the source came from, which nothing on the broadcast path
--   can observe).
--
--   Built here rather than as @scripts/*_fixture.lua@ files because the
--   #1204 case needs THREE modules whose failure behaviour and module
--   IDENTITIES differ, and identity on this path is exactly the
--   'scriptPath' recorded below — the file a chunk happens to live in
--   never reaches the aggregated status.
registerLuaModule ∷ LuaBackendState → Word32 → FilePath → Text → IO ()
registerLuaModule ls sid path chunk = do
    ref ← Lua.runWith (lbsLuaState ls) $ do
        status ← Lua.dostring (TE.encodeUtf8 chunk) ∷ Lua.LuaE Lua.Exception Lua.Status
        case status of
            Lua.OK → Lua.ref Lua.registryindex
            _      → error $ "fixture chunk failed to load: " ⧺ path
    atomically $ modifyTVar' (lbsScripts ls) $ Map.insert sid LuaScript
        { scriptId        = sid
        , scriptPath      = path
        , scriptTickRate  = 1000000
        , scriptNextTick  = 1000000
        , scriptModuleRef = ref
        , scriptPaused    = False
        }

-- | A backend with three real modules registered, mirroring the shipped
--   @onSaveLoaded@ shape: two that MUTATE their own singleton state and
--   then raise (distinct identities, distinct messages), plus a third
--   ordered after both that completes normally. 'broadcastToModules'
--   walks 'lbsScripts' in key order, so the script ids fix that
--   ordering. Each module records that it ran in a shared marker table
--   so the test can prove a callback executed even when it then threw.
newReconcilingBackend ∷ EngineEnv → IO LuaBackendState
newReconcilingBackend env = do
    ls ← newBareBackendWithDebugQueue env
    _ ← Lua.runWith (lbsLuaState ls)
            (Lua.dostring "__reconcileMarks = {}"
                ∷ Lua.LuaE Lua.Exception Lua.Status)
    registerLuaModule ls 1 "scripts/reconcile_alpha.lua"
        "return { onSaveLoaded = function(units, buildings) \
        \  __reconcileMarks.alpha = true \
        \  error('alpha reconcile blew up') \
        \end }"
    registerLuaModule ls 2 "scripts/reconcile_beta.lua"
        "return { onSaveLoaded = function(units, buildings) \
        \  __reconcileMarks.beta = true \
        \  error('beta reconcile blew up') \
        \end }"
    registerLuaModule ls 3 "scripts/reconcile_gamma.lua"
        "return { onSaveLoaded = function(units, buildings) \
        \  __reconcileMarks.gamma = true \
        \end }"
    pure ls

-- | Did the named module's @onSaveLoaded@ actually run?
markSet ∷ LuaBackendState → Text → IO Bool
markSet ls name = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.getglobal "__reconcileMarks" ∷ Lua.LuaE Lua.Exception Lua.Type
    _ ← Lua.getfield (-1) (Lua.Name (TE.encodeUtf8 name))
    set ← Lua.toboolean (-1)
    Lua.pop 2
    pure set

spec ∷ SpecWith EngineEnv
spec = do
    staleDebugCommandSpec
    shutdownDrainSpec
    reconciliationFailureSpec

-- | The load handoff's rejection, verbatim. Pinned rather than matched
--   loosely (#2282): it is protocol text a client reads, and \"contains
--   REJECTED\" would survive a rewording that broke every consumer.
loadHandoffRejection ∷ T.Text
loadHandoffRejection =
    "REJECTED: a load transaction replaced the session while this \
    \command was queued"

-- | Queue one command on @ls@'s private debug queue and hand it back,
--   so a case can inspect its response channel AND its lifecycle
--   afterwards. Built through 'newDebugCommand' — the same constructor
--   the real client loop uses — so its lifecycle starts where a real
--   command's does.
queueDebugCommand ∷ LuaBackendState → T.Text → IO DebugCommand
queueDebugCommand ls cmdText = do
    cmd ← newDebugCommand cmdText
    atomically $ writeTQueue (lbsDebugQueue ls) cmd
    pure cmd

staleDebugCommandSpec ∷ SpecWith EngineEnv
staleDebugCommandSpec = describe "LuaSaveLoaded stale debug-command cancellation (round 10 review, issue #763)" $ do
    it "cancels a debug command still queued at the load handoff, \
       \resolving its response MVar instead of leaving it to execute \
       \against the replacement session" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        stateRef ← newIORef ThreadRunning
        cmd ← queueDebugCommand ls "world.setDate('some_page', 9999, 1, 1)"

        processLuaMsg env ls stateRef
            (LuaSaveLoaded 123456 [] [] emptyLoadReconcileContext)

        resp ← tryTakeMVar (dcResponse cmd)
        case resp of
            Nothing → expectationFailure
                "stale debug command's response MVar was never resolved \
                \-- its caller (netcat, a script) would hang"
            Just msg → do
                -- Exact, not \"contains REJECTED\": #2282 rewrote the
                -- cancellation path underneath this reply and left the
                -- reply itself alone, which is only worth anything if
                -- something says so.
                msg `shouldBe` loadHandoffRejection
                -- The command itself must never have been evaluated —
                -- only its MVar was resolved with a rejection message.
                msg `shouldSatisfy` (not . T.isInfixOf "setDate")

    it "makes a rejected command permanently unclaimable, so no later \
       \processDebugCommands call can run it however it got requeued" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        stateRef ← newIORef ThreadRunning
        cmd ← queueDebugCommand ls "world.setDate('some_page', 9999, 1, 1)"

        processLuaMsg env ls stateRef
            (LuaSaveLoaded 123456 [] [] emptyLoadReconcileContext)

        -- #2282: being off the queue was never what stopped this
        -- command; the lifecycle transition is. The drain's own claim
        -- must fail on it, and the state it fails against is terminal.
        readDebugCommandState cmd `shouldReturn` DebugCommandCancelled
        claimDebugCommand cmd `shouldReturn` False
        readDebugCommandState cmd `shouldReturn` DebugCommandCancelled

    it "cancels every stale command queued at the handoff, not just the \
       \first" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        stateRef ← newIORef ThreadRunning
        cmd1 ← queueDebugCommand ls "return 1"
        cmd2 ← queueDebugCommand ls "return 2"

        processLuaMsg env ls stateRef
            (LuaSaveLoaded 654321 [] [] emptyLoadReconcileContext)

        r1 ← tryTakeMVar (dcResponse cmd1)
        r2 ← tryTakeMVar (dcResponse cmd2)
        r1 `shouldBe` Just loadHandoffRejection
        r2 `shouldBe` Just loadHandoffRejection
        claimDebugCommand cmd1 `shouldReturn` False
        claimDebugCommand cmd2 `shouldReturn` False

    it "leaves the debug queue empty afterward (no leftover stale \
       \commands for a later processDebugCommands call to pick up)" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        stateRef ← newIORef ThreadRunning
        _ ← queueDebugCommand ls "return 1"

        processLuaMsg env ls stateRef
            (LuaSaveLoaded 42 [] [] emptyLoadReconcileContext)

        remaining ← atomically $ tryReadTQueue (lbsDebugQueue ls)
        isNothing remaining `shouldBe` True

shutdownDrainSpec ∷ SpecWith EngineEnv
shutdownDrainSpec = describe "debug-queue shutdown drain (issue #2282)" $ do
    it "keeps the orderly-teardown reply and makes the drained command \
       \unclaimable" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        cmd ← queueDebugCommand ls "return 1"

        drainDebugQueue (lbsDebugQueue ls) "engine shutting down"

        tryTakeMVar (dcResponse cmd) `shouldReturn` Just "engine shutting down"
        readDebugCommandState cmd `shouldReturn` DebugCommandCancelled
        claimDebugCommand cmd `shouldReturn` False
        remaining ← atomically $ tryReadTQueue (lbsDebugQueue ls)
        isNothing remaining `shouldBe` True

    it "keeps the crash-teardown reply, which names the exception and \
       \is the one place a crash may still be claimed" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        cmd ← queueDebugCommand ls "return 1"

        drainDebugQueue (lbsDebugQueue ls)
            "ERROR: Lua thread crashed: divide by zero"

        tryTakeMVar (dcResponse cmd)
            `shouldReturn` Just "ERROR: Lua thread crashed: divide by zero"
        claimDebugCommand cmd `shouldReturn` False

-- | Issue #1204. The @onSaveLoaded@ broadcast is the load transaction's
--   last step, and its callbacks do correctness-critical reconciliation
--   (pruning orphaned rows, scrubbing typed references, rebuilding
--   derived statistics, rebinding Lua's world/HUD ids). Every one of
--   them is pcall-guarded, and that guard used to SWALLOW what it
--   caught: the dispatcher called 'finishLoad' unconditionally, so a
--   callback that half-reconciled its singleton and then raised still
--   reported an unqualified 'LoadSucceeded'.
--
--   These drive the REAL 'processLuaMsg' path with real registered
--   modules — the specs above deliberately register none, which makes
--   their broadcast a no-op that cannot exercise callback failure at
--   all.
reconciliationFailureSpec ∷ SpecWith EngineEnv
reconciliationFailureSpec = describe "LuaSaveLoaded reconciliation failure disposition (issue #1204)" $ do
    it "still reports a fully reconciled load as LoadPublished / \
       \LoadSucceeded, with no recorded failures" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        _ ← Lua.runWith (lbsLuaState ls)
                (Lua.dostring "__reconcileMarks = {}"
                    ∷ Lua.LuaE Lua.Exception Lua.Status)
        registerLuaModule ls 1 "scripts/reconcile_gamma.lua"
            "return { onSaveLoaded = function(units, buildings) \
            \  __reconcileMarks.gamma = true \
            \end }"
        stateRef ← newIORef ThreadRunning
        requestId ← beginLoadOrFail env

        processLuaMsg env ls stateRef
            (LuaSaveLoaded requestId [] [] emptyLoadReconcileContext)

        markSet ls "gamma" `shouldReturn` True
        status ← readLoadStatus (loadStatusRef env)
        lsPhase <$> status `shouldBe` Just LoadPublished
        lsOutcome <$> status `shouldBe` Just (Just LoadSucceeded)
        lsReconciliationFailures <$> status `shouldBe` Just []
        loadInProgress (loadStatusRef env) `shouldReturn` False

    it "reports the distinct LoadReconciliationFailed disposition when \
       \callbacks raise, aggregating EVERY failing module with its own \
       \error text, while the callback ordered after them still runs" $ \env → do
        ls ← newReconcilingBackend env
        stateRef ← newIORef ThreadRunning
        requestId ← beginLoadOrFail env

        processLuaMsg env ls stateRef
            (LuaSaveLoaded requestId [] [] emptyLoadReconcileContext)

        -- Isolation is intact: both failing callbacks got far enough to
        -- mutate their own state before raising (which is exactly the
        -- half-reconciled singleton this disposition exists to report),
        -- and the module ordered AFTER both of them still ran — a
        -- failure must not stop the broadcast at the first one.
        markSet ls "alpha" `shouldReturn` True
        markSet ls "beta"  `shouldReturn` True
        markSet ls "gamma" `shouldReturn` True

        status ← readLoadStatus (loadStatusRef env)
        lsPhase <$> status `shouldBe` Just LoadReconciliationFailed
        -- Distinguishable from BOTH existing terminals, not merely from
        -- success: reusing the pre-publication 'LoadFailed' shape would
        -- claim the old session survived unchanged, which is false once
        -- the session swap and barrier release have happened.
        lsPhase <$> status `shouldNotBe` Just LoadPublished
        lsPhase <$> status `shouldNotBe` Just LoadFailed
        lsFailedAtPhase <$> status `shouldBe` Just Nothing
        -- Terminal, so the transaction is over and the next save/load
        -- request is not blocked behind it forever.
        loadInProgress (loadStatusRef env) `shouldReturn` False

        case lsOutcome =≪ status of
            Just (LoadReconciliationIncomplete summary) → do
                summary `shouldSatisfy` T.isInfixOf "scripts/reconcile_alpha.lua"
                summary `shouldSatisfy` T.isInfixOf "alpha reconcile blew up"
                summary `shouldSatisfy` T.isInfixOf "scripts/reconcile_beta.lua"
                summary `shouldSatisfy` T.isInfixOf "beta reconcile blew up"
            other → expectationFailure $
                "expected a LoadReconciliationIncomplete outcome, got "
                ⧺ show other

        -- The structured list is the unambiguous module-to-error
        -- association: two failures, each paired with the identity of
        -- the module that produced it, and the module that succeeded
        -- absent entirely.
        case maybe [] lsReconciliationFailures status of
            [alpha, beta] → do
                rfModule alpha `shouldBe` "scripts/reconcile_alpha.lua"
                rfError alpha `shouldSatisfy` T.isInfixOf "alpha reconcile blew up"
                rfModule beta `shouldBe` "scripts/reconcile_beta.lua"
                rfError beta `shouldSatisfy` T.isInfixOf "beta reconcile blew up"
            other → expectationFailure $
                "expected exactly the two failing modules, got " ⧺ show other

-- | Start a real load transaction against the shared env's status ref
--   so the dispatcher's terminal call has a live request to end.
beginLoadOrFail ∷ EngineEnv → IO Int
beginLoadOrFail env = do
    started ← beginLoad (loadStatusRef env) "reconciliation_spec"
    case started of
        Right requestId → pure requestId
        Left err → error $
            "could not begin a load transaction: " ⧺ T.unpack err

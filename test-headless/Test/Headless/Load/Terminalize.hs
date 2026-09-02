-- | #2162: an ACCEPTED load whose Lua-thread half throws must still end.
--
--   @engine.loadSave@ accepts a request ('Engine.Load.Status.beginLoad'),
--   imposes the load pause, and only then does the decode work. Before
--   #2162 one bare IO site inside that window — the legacy flat-file
--   'Data.ByteString.readFile' in 'World.Save.Serialize.loadWorld' — let
--   an 'IOException' escape past 'failLoad': the request stayed parked
--   at 'LoadPaused' with no outcome, 'loadInProgress' kept answering
--   'True', and every later @engine.saveWorld@ / @engine.loadSave@ was
--   rejected as "a load transaction is already active" until the
--   process restarted.
--
--   Two groups here, reachable together as
--   @--match "load exception terminalization"@:
--
--     * The END-TO-END case drives the REGISTERED @engine.loadSave@ (the
--       real 'Engine.Scripting.Lua.API.registerLuaAPI' table, nothing
--       stubbed) against a legacy save whose read throws. The fixture is
--       a Unix-domain socket bound at @saves/<name>.synworld@:
--       @doesFileExist@ is true, it is not a symlink, and @open(2)@ fails
--       on it for EVERY user — CI runs as root (@.github/ci/Dockerfile@
--       sets no @USER@), where a permission-bit fixture reads fine and
--       the example would pass vacuously. The expected OS error text is
--       observed independently ('BS.readFile' on the same path) rather
--       than guessed, so the diagnostic assertion pins the real error on
--       both macOS and Linux.
--
--     * The TAXONOMY cases drive 'guardAcceptedLoad' — the exported
--       boundary @loadSaveFn@ wraps the whole accepted interval in — with
--       injected exceptions, because no production site raises each
--       kind on demand: a synchronous non-IO 'ErrorCall' terminalizes the
--       request at its recorded phase, an asynchronous 'ThreadKilled'
--       propagates untouched, and a 'Lua.Exception' terminalizes AND
--       keeps propagating. A fix confined to the legacy read alone
--       cannot pass these.
--
--   The end-to-end example wraps
--   'Test.Headless.Harness.Isolation.withIsolatedResourceRoot' AROUND
--   'Test.Headless.Harness.withHeadlessEngine' (#1357): the socket is
--   planted in the fixture's OWN @saves/@ directory, never through a
--   link into the developer's gitignored one.
module Test.Headless.Load.Terminalize (spec) where

import UPrelude
import Test.Hspec
import Control.Exception
    ( AsyncException(ThreadKilled), ErrorCall(..), IOException
    , displayException, throwIO, toException, try )
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as T
import qualified HsLua as Lua
import qualified Network.Socket as N
import System.Directory
    (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import System.FilePath ((</>))
import Engine.Core.Log (LoggerState, LogConfig(..), defaultLogConfig, initLogger)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Load.Status
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Save
    (guardAcceptedLoad, renderEscapedLoadException)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation
    (isInsideIsolatedResourceRoot, withIsolatedResourceRoot)
import Test.Headless.Harness.Log (quietLogBackend)

-- | A bare Lua backend with the real engine API registered — the same
--   fixture 'Test.Headless.UI.SettingsRevert' uses — so the example
--   calls the production @engine.loadSave@ / @engine.getLoadStatus@
--   boundary, guard included, rather than 'loadSaveFn' by hand.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one debug-console line and fail the example on a Lua error.
--   Every scenario below returns a boolean or a number, never a bare
--   string, since the console renders strings quoted.
evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← executeDebugLua (lbsLuaState ls) code
    when ("error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t) $
        expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig { lcBackend = quietLogBackend }

-- | Bind — and immediately close — a Unix-domain socket at @path@. The
--   filesystem entry outlives the descriptor, which is the whole
--   fixture: an existing non-symlink, non-directory entry that no
--   process can @open(2)@ for reading. Bound RELATIVE to the working
--   directory (the scratch root) because @sun_path@ is 104 bytes on
--   macOS and a temp-directory-absolute path can exceed that.
bindUnixSocket ∷ FilePath → IO ()
bindUnixSocket path = do
    sock ← N.socket N.AF_UNIX N.Stream N.defaultProtocol
    N.bind sock (N.SockAddrUnix path)
    N.close sock

-- | The save name the end-to-end example loads, and the legacy flat-file
--   path 'World.Save.Serialize.loadWorld' resolves it to (no slot
--   directory exists, so it falls through to @saves/<name>.synworld@).
socketSaveName ∷ Text
socketSaveName = "sock"

socketSavePath ∷ FilePath
socketSavePath = "saves" </> (T.unpack socketSaveName ⧺ ".synworld")

-- | A status handle holding one accepted request already advanced to
--   @phase@, plus the request id — the state 'guardAcceptedLoad' is
--   entered in.
acceptedAt ∷ LoadPhase → IO (LoadStatusRef, Int, LoggerState)
acceptedAt phase = do
    ref ← newLoadStatusRef
    Right n ← beginLoad ref "boom"
    advanceLoad ref n phase
    logger ← quietLogger
    pure (ref, n, logger)

spec ∷ Spec
spec = describe "load exception terminalization" $ do
    it "a legacy flat-file read that throws after acceptance ends in \
       \LoadFailed at LoadPaused, answers a single false, keeps the pause, \
       \and frees the transaction for the next request" $
        withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
            isInsideIsolatedResourceRoot `shouldReturn` True
            -- The fixture owns saves/ outright: the socket must never be
            -- planted through a link into the developer's directory.
            doesDirectoryExist "saves" `shouldReturn` True
            pathIsSymbolicLink "saves" `shouldReturn` False

            bindUnixSocket socketSavePath
            -- Precondition: exactly the shape 'loadWorld' selects the
            -- legacy path on — an existing, non-symlink file — whose
            -- read throws. Observed here independently so the message
            -- assertion below pins the REAL OS error, not a guess.
            doesFileExist socketSavePath `shouldReturn` True
            pathIsSymbolicLink socketSavePath `shouldReturn` False
            readOutcome ← try (BS.readFile socketSavePath)
            osError ← case readOutcome of
                Left (e ∷ IOException) → pure (T.pack (displayException e))
                Right _ → do
                    expectationFailure
                        "the socket fixture was readable; the example \
                        \cannot exercise the throwing read"
                    pure ""
            T.null osError `shouldBe` False

            readIORef (enginePausedRef env) `shouldReturn` False
            ls ← newBareLuaBackend env

            -- Requirement 2: pcall succeeds — no Lua error — and the API
            -- answers exactly one value, false (table.pack's n counts
            -- the pcall status too).
            result ← evalOk ls $ T.unwords
                [ "local r = table.pack(pcall(engine.loadSave, '"
                    <> socketSaveName <> "'));"
                , "return r[1] == true and r.n == 2 and r[2] == false" ]
            result `shouldBe` "true"

            -- Requirement 1: terminal, at the phase acceptance reached,
            -- naming the save and the OS error.
            Just st ← readLoadStatus (loadStatusRef env)
            lsSaveName st `shouldBe` socketSaveName
            lsPhase st `shouldBe` LoadFailed
            lsFailedAtPhase st `shouldBe` Just LoadPaused
            case lsOutcome st of
                Just (LoadAborted msg) → do
                    msg `shouldSatisfy`
                        T.isInfixOf ("'" <> socketSaveName <> "'")
                    msg `shouldSatisfy` T.isInfixOf osError
                other → expectationFailure
                    ("expected a LoadAborted outcome, got " ⧺ show other)
            luaStatus ← evalOk ls $ T.unwords
                [ "local s = engine.getLoadStatus();"
                , "return s.phase == 'LoadFailed'"
                , "and s.failedAtPhase == 'LoadPaused'"
                , "and s.saveName == '" <> socketSaveName <> "'" ]
            luaStatus `shouldBe` "true"

            -- Requirement 5: the pause imposed at acceptance stays.
            readIORef (enginePausedRef env) `shouldReturn` True

            -- Requirement 3: the transaction is over, and the next
            -- request is ACCEPTED (fresh id, its own ordinary failure)
            -- rather than rejected as already active.
            loadInProgress (loadStatusRef env) `shouldReturn` False
            next ← evalOk ls "return engine.loadSave('absent')"
            next `shouldBe` "false"
            Just st2 ← readLoadStatus (loadStatusRef env)
            lsRequestId st2 `shouldBe` lsRequestId st + 1
            lsSaveName st2 `shouldBe` "absent"
            lsOutcome st2 `shouldBe` Just (LoadAborted "Save not found: absent")
            loadInProgress (loadStatusRef env) `shouldReturn` False

    describe "guardAcceptedLoad exception taxonomy" $ do
        it "a synchronous non-IO exception terminalizes the request at \
           \its recorded phase and answers false" $ do
            (ref, n, logger) ← acceptedAt LoadSnapshotAssembled
            let boom = ErrorCall "materials directory vanished"
            answer ← Lua.run @Lua.Exception $
                guardAcceptedLoad ref logger n "boom"
                    (Lua.liftIO (throwIO boom))
            answer `shouldBe` False
            Just st ← readLoadStatus ref
            lsPhase st `shouldBe` LoadFailed
            lsFailedAtPhase st `shouldBe` Just LoadSnapshotAssembled
            lsOutcome st `shouldBe` Just (LoadAborted
                (renderEscapedLoadException "boom" (toException boom)))
            case lsOutcome st of
                Just (LoadAborted msg) → do
                    msg `shouldSatisfy` T.isInfixOf "'boom'"
                    msg `shouldSatisfy`
                        T.isInfixOf "materials directory vanished"
                other → expectationFailure
                    ("expected a LoadAborted outcome, got " ⧺ show other)
            loadInProgress ref `shouldReturn` False

        it "an asynchronous exception propagates unchanged and is not \
           \recorded as a load failure" $ do
            (ref, n, logger) ← acceptedAt LoadPaused
            Lua.run @Lua.Exception
                (guardAcceptedLoad ref logger n "boom"
                    (Lua.liftIO (throwIO ThreadKilled)))
                `shouldThrow` (≡ ThreadKilled)
            Just st ← readLoadStatus ref
            lsPhase st `shouldBe` LoadPaused
            lsOutcome st `shouldBe` Nothing
            lsFailedAtPhase st `shouldBe` Nothing

        it "a Lua exception terminalizes the request and keeps \
           \propagating for hslua's own conversion" $ do
            (ref, n, logger) ← acceptedAt LoadPaused
            Lua.run @Lua.Exception
                (guardAcceptedLoad ref logger n "boom"
                    (Lua.liftIO (throwIO (Lua.Exception "lua side"))))
                `shouldThrow` (\(Lua.Exception msg) → msg ≡ "lua side")
            Just st ← readLoadStatus ref
            lsPhase st `shouldBe` LoadFailed
            lsFailedAtPhase st `shouldBe` Just LoadPaused
            case lsOutcome st of
                Just (LoadAborted msg) → do
                    msg `shouldSatisfy` T.isInfixOf "'boom'"
                    msg `shouldSatisfy` T.isInfixOf "lua side"
                other → expectationFailure
                    ("expected a LoadAborted outcome, got " ⧺ show other)
            loadInProgress ref `shouldReturn` False

        it "an interval that completes passes its answer through with \
           \the transaction untouched" $ do
            (ref, n, logger) ← acceptedAt LoadContentValidated
            answer ← Lua.run @Lua.Exception $
                guardAcceptedLoad ref logger n "boom" (pure True)
            answer `shouldBe` True
            Just st ← readLoadStatus ref
            lsPhase st `shouldBe` LoadContentValidated
            lsOutcome st `shouldBe` Nothing

        it "never overwrites an outcome the interval already recorded" $ do
            (ref, n, logger) ← acceptedAt LoadPaused
            answer ← Lua.run @Lua.Exception $
                guardAcceptedLoad ref logger n "boom" $ do
                    Lua.liftIO (failLoad ref n "Save not found: boom")
                    Lua.liftIO (throwIO (ErrorCall "late"))
            answer `shouldBe` False
            Just st ← readLoadStatus ref
            lsOutcome st `shouldBe` Just (LoadAborted "Save not found: boom")
            lsFailedAtPhase st `shouldBe` Just LoadPaused

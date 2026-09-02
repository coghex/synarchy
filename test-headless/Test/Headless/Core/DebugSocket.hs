-- | The debug console's socket supervision (#2170).
--
--   @Test.Headless.Core.DebugListener@ covers #1190's pure per-mode
--   policy — whether a console that never STARTED aborts the boot. This
--   is the other half: a console that DID start, printed @READY@, and
--   then had to survive a transient accept failure, refuse an
--   over-the-cap connection, drop an oversized line, close an idle
--   client, notice its listening socket dying for good, and finally be
--   stopped by name.
--
--   Every example here drives the REAL 'startDebugServer' over REAL
--   loopback sockets, with no engine and no Lua state:
--   'startDebugServer' takes a config carrying a port and a built-in
--   callback, and needs neither. That is deliberate — the supervision
--   under test is precisely the part a mocked transport would not
--   exercise, and before #2170 nothing in the tree opened a socket
--   against this module at all.
--
--   The port is EPHEMERAL in the sense of \"dynamically chosen, and
--   never zero\": port 0 is issue #46's no-listener sentinel and keeps
--   that meaning, which is itself one of the cases below.
module Test.Headless.Core.DebugSocket (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue)
import Control.Exception
    ( ArithException(..), Exception, IOException, SomeException, bracket
    , finally, throwIO, toException, try )
import Data.IORef
    (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Exception (IOErrorType(..))
import System.IO.Error (mkIOError)
import System.Timeout (timeout)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Engine.Core.State (EngineLifecycle(..), requestEngineCleanup)
import Engine.Core.Types (BootMode(..))
import Engine.Scripting.Lua.DebugServer

spec ∷ Spec
spec = describe "debug-console socket supervision (#2170)" $ do
    ownerSpec
    acceptSupervisionSpec
    lossPolicySpec
    connectionCapSpec
    lineCapSpec
    idleSpec
    classifySpec

-- ---------------------------------------------------------------- --
-- The owner
-- ---------------------------------------------------------------- --

ownerSpec ∷ Spec
ownerSpec = describe "the listener owner" $ do

  it "stops cleanly: the port stops listening, an open client is \
     \released, and the stop is idempotent and silent" $ do
    diags ← newDiagnostics
    withServer (testConfig diags id) $ \port console → do
        withClient port $ \client → do
            banner ← readUntilContains oneSecond "> " client
            banner `shouldSatisfy` BS.isPrefixOf "synarchy debug console"
            stopDebugConsole console
            -- Idempotent: a second stop must not throw and must not
            -- re-close anything already closed.
            stopDebugConsole console
            (_, sawEof) ← drainFor twoSeconds client
            sawEof `shouldBe` True
        stillListening ← portAccepts port
        stillListening `shouldBe` False
        -- An intentional stop is not a loss: neither diagnostic fires.
        readIORef (diagRetries diags) `shouldReturn` []
        readIORef (diagLosses diags) `shouldReturn` []

  it "honours issue #46's port-0 sentinel: no listener, nothing to \
     \stop" $ do
    started ← startDebugServer (defaultDebugServerConfig 0 neverBuiltin)
    case started of
        Left err → expectationFailure $
            "port 0 must yield an inert console, not " <> T.unpack err
        Right console → do
            isNothing (consoleListener console) `shouldBe` True
            -- The no-op stop is what lets the shutdown path treat an
            -- inert console like any other.
            stopDebugConsole console

-- ---------------------------------------------------------------- --
-- Accept supervision
-- ---------------------------------------------------------------- --

acceptSupervisionSpec ∷ Spec
acceptSupervisionSpec = describe "the accept loop" $ do

  it "survives a transient accept failure and then serves a real \
     \connection" $ do
    diags ← newDiagnostics
    failedOnce ← newIORef False
    let injectOnce sock = do
            alreadyFailed ← atomicModifyIORef' failedOnce (\f → (True, f))
            if alreadyFailed
                then accept sock
                -- ResourceExhausted is EMFILE/ENFILE: the descriptor
                -- table momentarily full, which the DEFAULT classifier
                -- must call recoverable.
                else throwIO (ioErrorOfType ResourceExhausted)
        tweak cfg = cfg
            { dscAccept = injectOnce
            , dscLimits = (dscLimits cfg) { dslAcceptRetryDelayMicros = 0 }
            }
    withServer (testConfig diags tweak) $ \port _ → do
        withClient port $ \client → do
            banner ← readUntilContains twoSeconds "> " client
            banner `shouldSatisfy` BS.isPrefixOf "synarchy debug console"
        retries ← readIORef (diagRetries diags)
        case retries of
            [(remaining, cause)] → do
                remaining `shouldBe` defaultAcceptRetryBudget
                -- The line names the port, the cause, and the fact that
                -- the listener is still going.
                let line = listenerRetryMessage port remaining cause
                line `shouldSatisfy` T.isInfixOf (tshow port)
                line `shouldSatisfy` T.isInfixOf "resource exhausted"
                line `shouldSatisfy` T.isInfixOf "retrying"
            other → expectationFailure $
                "expected exactly one retry diagnostic, got " <> show other
        readIORef (diagLosses diags) `shouldReturn` []

  it "reports terminal loss exactly once, stops accepting, and stays \
     \silent through the later stop" $ do
    diags ← newDiagnostics
    -- A UserError is what @network@ raises for an already-closed
    -- socket: positively terminal, so it is reported without spending
    -- the retry budget first.
    let tweak cfg = cfg { dscAccept = \_ → throwIO deadListener }
    withServer (testConfig diags tweak) $ \port console → do
        losses ← waitForLosses diags 1
        case losses of
            [cause] → do
                cause `shouldSatisfy` T.isInfixOf "listening socket is gone"
                listenerLostMessage port cause
                    `shouldSatisfy` T.isInfixOf (tshow port)
            other → expectationFailure $
                "expected exactly one loss diagnostic, got " <> show other
        readIORef (diagRetries diags) `shouldReturn` []
        closed ← waitUntil twoSeconds (not ⊚ portAccepts port)
        closed `shouldBe` True
        stopDebugConsole console
        readIORef (diagLosses diags) `shouldReturn` losses

  it "spends the whole retry budget on a persistently recoverable \
     \failure, then reports loss once" $ do
    diags ← newDiagnostics
    let budget = 3
        tweak cfg = cfg
            { dscAccept = \_ → throwIO (ioErrorOfType ResourceBusy)
            , dscLimits = (dscLimits cfg)
                { dslAcceptRetryBudget      = budget
                , dslAcceptRetryDelayMicros = 0
                }
            }
    withServer (testConfig diags tweak) $ \_ _ → do
        losses ← waitForLosses diags 1
        retries ← readIORef (diagRetries diags)
        -- The budget counts down to one and no further, and the loss
        -- says which budget was exhausted.
        map fst retries `shouldBe` [budget, budget - 1 .. 1]
        case losses of
            [cause] → cause `shouldSatisfy`
                T.isInfixOf ("exhausted " <> tshow budget)
            other → expectationFailure $
                "expected exactly one loss diagnostic, got " <> show other

-- ---------------------------------------------------------------- --
-- What loss means per boot mode
-- ---------------------------------------------------------------- --

lossPolicySpec ∷ Spec
lossPolicySpec = describe "the per-mode response to a lost listener" $ do

  it "names the port and the cause in every mode, and asks for \
     \shutdown in exactly the console-required ones" $ do
    let shutdownModes =
          [ m | m ← allModes, llrShutdown (listenerLossResponse m 9008 "boom") ]
    shutdownModes `shouldBe` [ModeHeadless, ModeOffscreen]
    forM_ allModes $ \m → do
        let msg = llrMessage (listenerLossResponse m 9008 "boom")
        msg `shouldSatisfy` T.isInfixOf "9008"
        msg `shouldSatisfy` T.isInfixOf "boom"

  it "advances a console-required engine to CleaningUp when the real \
     \listener is lost" $
    lifecycleAfterLoss ModeHeadless `shouldReturn` CleaningUp

  it "leaves a console-optional engine running when the real listener \
     \is lost" $
    lifecycleAfterLoss ModeGraphical `shouldReturn` EngineRunning

  it "shuts a console-required engine down even when the diagnostic \
     \cannot be written" $ do
    -- The report used to come FIRST and hand the decision back for the
    -- caller to act on, so a closed stderr took the shutdown with it.
    lifecycle ← newIORef EngineRunning
    handleDebugListenerLossWith
        (\_ → throwIO (userError "stderr is gone"))
        ModeHeadless 9008
        (void (requestEngineCleanup lifecycle))
        "boom"
    readIORef lifecycle `shouldReturn` CleaningUp

  it "advances the lifecycle monotonically and answers whether it was \
     \this call that did it" $ do
    let step from = do
            ref ← newIORef from
            changed ← requestEngineCleanup ref
            after ← readIORef ref
            return (changed, after)
    step EngineStarting `shouldReturn` (True, CleaningUp)
    step EngineRunning `shouldReturn` (True, CleaningUp)
    step CleaningUp `shouldReturn` (False, CleaningUp)
    step EngineStopped `shouldReturn` (False, EngineStopped)

-- | Drive a REAL listener to terminal loss with the loss hook wired
--   exactly as "Engine.Scripting.Lua.Thread" wires it — report on
--   stderr, then ask the engine to stop iff the mode requires the
--   console — and read back the lifecycle it left behind.
--
--   The stderr line this writes is the point, not noise: the report has
--   to happen in BOTH modes and only the shutdown differs.
lifecycleAfterLoss ∷ BootMode → IO EngineLifecycle
lifecycleAfterLoss mode = do
    lifecycle ← newIORef EngineRunning
    reported ← newIORef (0 ∷ Int)
    let tweak port cfg = cfg
            { dscBuiltin = neverBuiltin
            , dscAccept  = \_ → throwIO deadListener
            , dscOnLoss  = \cause → do
                handleDebugListenerLoss mode port
                    (void (requestEngineCleanup lifecycle)) cause
                atomicModifyIORef' reported (\n → (n + 1, ()))
            }
    withServer tweak $ \_ _ → do
        fired ← waitUntil twoSeconds ((≥ 1) ⊚ readIORef reported)
        fired `shouldBe` True
    readIORef lifecycle

-- ---------------------------------------------------------------- --
-- The connection cap
-- ---------------------------------------------------------------- --

connectionCapSpec ∷ Spec
connectionCapSpec = describe "the connection cap" $

  it "admits up to the cap, answers an over-cap connection with only \
     \the refusal line and no evaluator work, and reclaims the slot \
     \when an admitted client leaves" $ do
    diags ← newDiagnostics
    calls ← newIORef (0 ∷ Int)
    let cap = 2
        countingBuiltin cmd = do
            atomicModifyIORef' calls (\n → (n + 1, ()))
            return (Just ("echo:" <> cmd))
        tweak cfg = cfg
            { dscBuiltin = countingBuiltin
            , dscLimits  = (dscLimits cfg)
                { dslMaxConnections    = cap
                , dslIdleTimeoutMicros = tenSeconds
                }
            }
    withServer (testConfig diags tweak) $ \port console →
      withClient port $ \admittedA →
      withClient port $ \admittedB → do
        greetingA ← readUntilContains oneSecond "> " admittedA
        greetingA `shouldSatisfy` BS.isPrefixOf "synarchy debug console"
        greetingB ← readUntilContains oneSecond "> " admittedB
        greetingB `shouldSatisfy` BS.isPrefixOf "synarchy debug console"
        -- The third connection is accepted by the kernel and then
        -- refused by the console.
        withClient port $ \overCap → do
            -- Sending anyway is the point: a refused connection must
            -- reach neither the built-in table nor the command queue.
            void (trySend overCap "return 1\n")
            (bytes, sawEof) ← drainFor twoSeconds overCap
            bytes `shouldBe`
                TE.encodeUtf8 (connectionRefusedMessage cap <> "\n")
            sawEof `shouldBe` True
        readIORef calls `shouldReturn` 0
        queued ← atomically (tryReadTQueue (consoleQueue console))
        isNothing queued `shouldBe` True
        -- An admitted client leaving gives its slot back.
        close admittedA
        reclaimed ← waitUntil twoSeconds $ do
            attempt ← try (connectTo port)
            case attempt of
                Left (_ ∷ SomeException) → return False
                Right sock → do
                    greeting ← readUntilContains oneSecond "> " sock
                    close sock
                    return (BS.isPrefixOf "synarchy debug console" greeting)
        reclaimed `shouldBe` True
        -- The client that stayed was served all along.
        sendAll admittedB "ping\n"
        reply ← readUntilContains twoSeconds "echo:ping" admittedB
        reply `shouldSatisfy` BS.isInfixOf "echo:ping"
        readIORef calls `shouldReturn` 1

-- ---------------------------------------------------------------- --
-- The line cap
-- ---------------------------------------------------------------- --

lineCapSpec ∷ Spec
lineCapSpec = describe "the line cap" $ do

  it "accepts a line of exactly the cap in raw bytes, excluding the \
     \newline" $
    withLineCap 32 $ \port → withClient port $ \client → do
        void $ readUntilContains oneSecond "> " client
        sendAll client (BS.replicate 32 0x61 <> "\n")
        reply ← readUntilContains twoSeconds "len=" client
        reply `shouldSatisfy` BS.isInfixOf "len=32"

  it "refuses a TERMINATED line one byte over the cap and disconnects" $
    withLineCap 32 $ \port → withClient port $ \client → do
        void $ readUntilContains oneSecond "> " client
        sendAll client (BS.replicate 33 0x61 <> "\n")
        (bytes, sawEof) ← drainFor twoSeconds client
        bytes `shouldSatisfy`
            BS.isInfixOf (TE.encodeUtf8 (lineTooLongMessage 32))
        bytes `shouldSatisfy` (not ∘ BS.isInfixOf "len=")
        sawEof `shouldBe` True

  it "refuses an UNTERMINATED buffer past the cap without waiting for \
     \a newline that may never come" $
    withLineCap 32 $ \port → withClient port $ \client → do
        void $ readUntilContains oneSecond "> " client
        sendAll client (BS.replicate 33 0x61)
        (bytes, sawEof) ← drainFor twoSeconds client
        bytes `shouldSatisfy`
            BS.isInfixOf (TE.encodeUtf8 (lineTooLongMessage 32))
        bytes `shouldSatisfy` (not ∘ BS.isInfixOf "len=")
        sawEof `shouldBe` True

-- | A console whose only non-default bound is the line cap, with a
--   built-in that reports the decoded length so an accepted line is
--   distinguishable from a refused one.
withLineCap ∷ Int → (Int → IO α) → IO α
withLineCap cap act = do
    diags ← newDiagnostics
    let tweak cfg = cfg
            { dscBuiltin = \cmd → return (Just ("len=" <> tshow (T.length cmd)))
            , dscLimits  = (dscLimits cfg)
                { dslMaxLineBytes      = cap
                , dslIdleTimeoutMicros = tenSeconds
                }
            }
    withServer (testConfig diags tweak) (\port _ → act port)

-- ---------------------------------------------------------------- --
-- The idle timeout
-- ---------------------------------------------------------------- --

idleSpec ∷ Spec
idleSpec = describe "the idle timeout" $ do

  it "closes a connection that sits with no command in flight" $ do
    diags ← newDiagnostics
    let tweak cfg = cfg
            { dscLimits = (dscLimits cfg)
                { dslIdleTimeoutMicros = 200000 } }
    withServer (testConfig diags tweak) $ \port _ →
        withClient port $ \client → do
            void $ readUntilContains oneSecond "> " client
            (bytes, sawEof) ← drainFor fiveSeconds client
            bytes `shouldSatisfy` BS.isInfixOf "idle for"
            sawEof `shouldBe` True

  it "does not count a command in flight as idle: a built-in that \
     \outlasts the timeout still answers, and the connection survives" $ do
    diags ← newDiagnostics
    let tweak cfg = cfg
            { dscBuiltin = \cmd → do
                when (cmd ≡ "slow") $ threadDelay 1500000
                return (Just ("ok:" <> cmd))
            , dscLimits  = (dscLimits cfg)
                { dslIdleTimeoutMicros = 800000 } }
    withServer (testConfig diags tweak) $ \port _ →
        withClient port $ \client → do
            void $ readUntilContains oneSecond "> " client
            sendAll client "slow\n"
            slow ← readUntilContains fiveSeconds "ok:slow" client
            slow `shouldSatisfy` BS.isInfixOf "ok:slow"
            slow `shouldSatisfy` (not ∘ BS.isInfixOf "idle for")
            -- Still serving: the idle clock restarted when the command
            -- finished rather than having expired underneath it.
            sendAll client "quick\n"
            quick ← readUntilContains twoSeconds "ok:quick" client
            quick `shouldSatisfy` BS.isInfixOf "ok:quick"

  it "does not count a QUEUED evaluator command as idle either: a \
     \response that outlasts the timeout still arrives, over the \
     \unchanged 30-second production wait" $ do
    diags ← newDiagnostics
    let idleMicros = 400000
        -- Nothing is a built-in here, so every line takes the queue.
        tweak cfg = cfg
            { dscBuiltin = neverBuiltin
            , dscLimits  = (dscLimits cfg)
                { dslIdleTimeoutMicros = idleMicros } }
    withServer (testConfig diags tweak) $ \port console →
        withClient port $ \client → do
            void $ readUntilContains oneSecond "> " client
            -- Stand in for the Lua thread: dequeue the command, then
            -- answer it only well after the idle timeout would have
            -- fired had the wait been counted as idle.
            dequeued ← newIORef Nothing
            responder ← forkIO $ do
                mCmd ← waitForCommand (consoleQueue console) twoSeconds
                forM_ mCmd $ \(DebugCommand text mvar) → do
                    writeIORef dequeued (Just text)
                    threadDelay (idleMicros * 3)
                    putMVar mvar "queued-ok"
            sendAll client "return 1\n"
            reply ← readUntilContains fiveSeconds "queued-ok" client
            killThread responder
            reply `shouldSatisfy` BS.isInfixOf "queued-ok"
            reply `shouldSatisfy` (not ∘ BS.isInfixOf "idle for")
            -- It really went through the QUEUE, not the built-in table.
            readIORef dequeued `shouldReturn` Just "return 1"

  it "leaves the queued-command response wait at the production 30 \
     \seconds, which the idle timeout neither shortens nor replaces" $
    commandResponseTimeoutMicros `shouldBe` 30000000

-- | Poll the command queue the way the Lua thread's own tick does.
waitForCommand ∷ TQueue DebugCommand → Int → IO (Maybe DebugCommand)
waitForCommand queue budget = do
    found ← newIORef Nothing
    void ∘ waitUntil budget $ do
        mCmd ← pollDebugCommand queue
        case mCmd of
            Nothing  → return False
            Just cmd → writeIORef found (Just cmd) >> return True
    readIORef found

-- ---------------------------------------------------------------- --
-- The default classification
-- ---------------------------------------------------------------- --

classifySpec ∷ Spec
classifySpec = describe "classifyAcceptFailure" $ do

  it "retries the failures that describe the environment rather than \
     \the socket" $
    forM_ [ResourceExhausted, ResourceBusy, Interrupted, TimeExpired
          , OtherError] $ \t →
        classifyAcceptFailure (asSomeException (ioErrorOfType t))
            `shouldBe` AcceptRetry

  it "treats a dead or already-closed listening socket as terminal" $ do
    forM_ [InvalidArgument, ResourceVanished, NoSuchThing, PermissionDenied]
      $ \t → classifyAcceptFailure (asSomeException (ioErrorOfType t))
            `shouldBe` AcceptFatal
    classifyAcceptFailure (asSomeException deadListener) `shouldBe` AcceptFatal

  it "treats a non-IO exception as terminal rather than guessing" $
    classifyAcceptFailure (asSomeException Overflow) `shouldBe` AcceptFatal

-- ---------------------------------------------------------------- --
-- Fixtures
-- ---------------------------------------------------------------- --

allModes ∷ [BootMode]
allModes = [minBound .. maxBound]

-- | The @userError@ @network@ raises for an already-closed socket.
deadListener ∷ IOError
deadListener = userError "listening socket is gone"

ioErrorOfType ∷ IOErrorType → IOError
ioErrorOfType t =
    mkIOError t "Test.Headless.Core.DebugSocket.accept" Nothing Nothing

asSomeException ∷ Exception e ⇒ e → SomeException
asSomeException = toException

-- | The two diagnostics a test watches instead of stderr.
data Diagnostics = Diagnostics
    { diagRetries ∷ IORef [(Int, Text)]
    , diagLosses  ∷ IORef [Text]
    }

newDiagnostics ∷ IO Diagnostics
newDiagnostics = Diagnostics ⊚ newIORef [] ⊛ newIORef []

-- | The production config with the two stderr hooks redirected into
--   'Diagnostics', a built-in that answers nothing, and then whatever
--   the example itself overrides.
testConfig ∷ Diagnostics → (DebugServerConfig → DebugServerConfig)
           → Int → DebugServerConfig → DebugServerConfig
testConfig diags tweak _ cfg = tweak cfg
    { dscBuiltin = neverBuiltin
    , dscOnRetry = \remaining cause → atomicModifyIORef'
        (diagRetries diags) (\xs → (xs <> [(remaining, cause)], ()))
    , dscOnLoss  = \cause → atomicModifyIORef'
        (diagLosses diags) (\xs → (xs <> [cause], ()))
    }

neverBuiltin ∷ Text → IO (Maybe Text)
neverBuiltin _ = return Nothing

-- | Start a console on a dynamically chosen, non-zero loopback port,
--   run the example against it, and stop it however the example ends.
--
--   The port is chosen by binding to 0, reading the assignment back and
--   releasing it, so a bind that loses the ensuing race is retried
--   rather than failing the example.
withServer ∷ (Int → DebugServerConfig → DebugServerConfig)
           → (Int → DebugConsole → IO α) → IO α
withServer tweak act = go (5 ∷ Int)
  where
    go 0 = fail "no free loopback port for the debug console after 5 tries"
    go n = do
        port ← freePort
        started ← startDebugServer $
            tweak port (defaultDebugServerConfig port neverBuiltin)
        case started of
            Left _        → go (n - 1)
            Right console → act port console `finally` stopDebugConsole console

freePort ∷ IO Int
freePort = do
    addr ← loopbackAddr [AI_PASSIVE] "0"
    bracket (openSocket addr) close $ \sock → do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 1
        name ← getSocketName sock
        case name of
            SockAddrInet p _ → return (fromIntegral p)
            other → fail $ "expected an IPv4 loopback bind, got " <> show other

loopbackAddr ∷ [AddrInfoFlag] → String → IO AddrInfo
loopbackAddr flags service = do
    let hints = defaultHints { addrFlags = flags, addrSocketType = Stream }
    addrs ← getAddrInfo (Just hints) (Just "127.0.0.1") (Just service)
    case addrs of
        (a:_) → return a
        []    → fail "getAddrInfo returned no loopback addresses"

connectTo ∷ Int → IO Socket
connectTo port = do
    addr ← loopbackAddr [] (show port)
    sock ← openSocket addr
    attempt ← try (connect sock (addrAddress addr))
    case attempt of
        Right () → return sock
        Left (e ∷ SomeException) → close sock >> throwIO e

withClient ∷ Int → (Socket → IO α) → IO α
withClient port = bracket (connectTo port) close

-- | Whether the port still has something listening on it. Used only to
--   assert the NEGATIVE, so a stray success closes what it opened.
portAccepts ∷ Int → IO Bool
portAccepts port = do
    attempt ← try (connectTo port)
    case attempt of
        Left (_ ∷ SomeException) → return False
        Right sock               → close sock >> return True

trySend ∷ Socket → BS.ByteString → IO (Either SomeException ())
trySend sock bytes = try (sendAll sock bytes)

-- | Read until the accumulated bytes contain @needle@, the peer closes,
--   or the budget expires. Returns whatever arrived either way, so the
--   caller asserts on content rather than on having timed out.
readUntilContains ∷ Int → BS.ByteString → Socket → IO BS.ByteString
readUntilContains budget needle sock =
    fst ⊚ readWhile budget (not ∘ BS.isInfixOf needle) sock

-- | Read until the peer closes or the budget expires, reporting whether
--   the close actually happened.
drainFor ∷ Int → Socket → IO (BS.ByteString, Bool)
drainFor budget = readWhile budget (const True)

readWhile ∷ Int → (BS.ByteString → Bool) → Socket → IO (BS.ByteString, Bool)
readWhile budget keepGoing sock = do
    start ← getMonotonicTimeNSec
    let loop acc
          | not (keepGoing acc) = return (acc, False)
          | otherwise = do
              left ← remainingMicros start budget
              if left ≤ 0 then return (acc, False) else do
                  chunk ← timeout left (tryRecv sock)
                  case chunk of
                      Nothing → return (acc, False)
                      Just bytes
                          | BS.null bytes → return (acc, True)
                          | otherwise     → loop (acc <> bytes)
    loop BS.empty

-- | A reset connection reads as a close: the peer is gone either way,
--   and which of the two the kernel reports is not this module's
--   contract.
--
--   'IOException' and NOT 'SomeException': the caller wraps this in
--   'timeout', whose expiry arrives as an ASYNC exception, and a
--   catch-all here would swallow it and hand back an empty string that
--   'readWhile' would read as a clean close. That is not hypothetical —
--   it silently turned every "the peer closed" assertion in this
--   module into a tautology until a mutation run caught it.
tryRecv ∷ Socket → IO BS.ByteString
tryRecv sock = do
    attempt ← try (recv sock 4096)
    case attempt of
        Left (_ ∷ IOException) → return BS.empty
        Right bytes            → return bytes

-- | Poll a predicate until it holds or the budget expires.
waitUntil ∷ Int → IO Bool → IO Bool
waitUntil budget check = loop
  where
    loop = do
        start ← getMonotonicTimeNSec
        go start
    go start = do
        held ← check
        if held then return True else do
            left ← remainingMicros start budget
            if left ≤ 0 then return False
                        else threadDelay (min 20000 left) >> go start

waitForLosses ∷ Diagnostics → Int → IO [Text]
waitForLosses diags wanted = do
    void ∘ waitUntil twoSeconds $
        (≥ wanted) ∘ length ⊚ readIORef (diagLosses diags)
    readIORef (diagLosses diags)

remainingMicros ∷ Word64 → Int → IO Int
remainingMicros start budget = do
    now ← getMonotonicTimeNSec
    let elapsed = fromIntegral ((now - start) `div` 1000) ∷ Int
    return (budget - elapsed)

oneSecond, twoSeconds, fiveSeconds, tenSeconds ∷ Int
oneSecond   = 1000000
twoSeconds  = 2000000
fiveSeconds = 5000000
tenSeconds  = 10000000

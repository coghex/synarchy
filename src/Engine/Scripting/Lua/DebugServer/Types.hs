{-# LANGUAGE Strict #-}
-- | The debug console's shared vocabulary: the command record the Lua
--   thread polls, the finite bounds every connection is held to, the
--   injection seam a test uses to drive those bounds without waiting,
--   and the two handles the listener owner is expressed in terms of.
--
--   Split out of "Engine.Scripting.Lua.DebugServer" for the ordinary
--   Base\/Types reason: this module imports nothing of the engine, so
--   both the listener ("Engine.Scripting.Lua.DebugServer.Listener") and
--   the per-connection loop
--   ("Engine.Scripting.Lua.DebugServer.Client") can name these types
--   without either importing the other.
--
--   The trust model these bounds sit inside is stated once, on
--   "Engine.Scripting.Lua.DebugServer". Read it before relaxing
--   anything here: the caps are a RESOURCE bound on a channel that is
--   deliberately unauthenticated, never a security boundary, and
--   raising one cannot make the console less trusted than it already
--   is by design.
module Engine.Scripting.Lua.DebugServer.Types
    ( DebugCommand(..)
      -- * The command lifecycle
    , DebugCommandState(..)
    , newDebugCommand
    , claimDebugCommand
    , cancelDebugCommand
    , completeDebugCommand
    , readDebugCommandState
    , commandCancelledMessage
    , commandUnknownOutcomeMessage
      -- * Bounds
    , DebugServerLimits(..)
    , defaultDebugServerLimits
    , defaultMaxConnections
    , defaultMaxLineBytes
    , defaultIdleTimeoutMicros
    , defaultAcceptRetryBudget
    , defaultAcceptRetryDelayMicros
    , commandResponseTimeoutMicros
    , acceptRetryDelayFor
      -- * Accept-failure classification
    , AcceptDisposition(..)
    , classifyAcceptFailure
      -- * Diagnostics
    , connectionRefusedMessage
    , lineTooLongMessage
    , idleTimeoutMessage
    , listenerRetryMessage
    , listenerLostMessage
      -- * Configuration
    , DebugServerConfig(..)
    , defaultDebugServerConfig
      -- * Handles
    , DebugConsole(..)
    , DebugListener(..)
    , ClientHandle(..)
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, fromException)
import GHC.IO.Exception (IOErrorType(..))
import Network.Socket (Socket, SockAddr, accept)
import System.IO (hPutStrLn, hFlush, stderr)
import System.IO.Error (ioeGetErrorType, tryIOError)

-- | One line of Lua handed from a client connection to the Lua thread,
--   with the channel its answer comes back on and the lifecycle cell
--   that decides, once and for all, whether it ever runs.
data DebugCommand = DebugCommand
    { dcCommand  ∷ !Text      -- ^ Lua code to evaluate
    , dcResponse ∷ !(MVar Text)  -- ^ Response channel
    , dcState    ∷ !(TVar DebugCommandState)
      -- ^ The one linearization point every party contends through
      --   ('claimDebugCommand' \/ 'cancelDebugCommand'). Never written
      --   directly: the two operations are what make execution and
      --   cancellation mutually exclusive (#2282).
    }

-- | Where a queued command is in its one-way lifecycle.
--
--   Exactly one transition out of 'DebugCommandQueued' ever succeeds,
--   and both destinations are terminal, which is the whole content of
--   #2282's requirement 2: a command is either EXECUTED (claimed, then
--   run and answered by whoever claimed it) or CANCELLED BEFORE
--   EXECUTION (a client whose response wait expired, a load handoff, a
--   shutdown drain) — never both, and never neither.
--
--   A cancelled command therefore stays on the queue until a drain
--   dequeues it, and that drain's 'claimDebugCommand' fails: an
--   unclaimed cancelled command is permanently unclaimable, so no later
--   tick, on any page, can run it.
data DebugCommandState
    = DebugCommandQueued
      -- ^ On the queue and still eligible — the only state either
      --   transition below accepts as its precondition.
    | DebugCommandClaimed
      -- ^ Claimed for execution immediately before the evaluator is
      --   invoked. "Started" means exactly this, and nothing about how
      --   far the evaluator has got.
    | DebugCommandCancelled
      -- ^ Cancelled before it was ever claimed. Its reply has already
      --   been published by the canceller.
    deriving (Eq, Show)

-- | A fresh command, queued, with an empty response channel.
--
--   The single constructor for a real 'DebugCommand': assembling the
--   record by hand is how a caller ends up with a lifecycle cell in the
--   wrong state, so 'Engine.Scripting.Lua.DebugServer.Client' and every
--   test go through this.
newDebugCommand ∷ Text → IO DebugCommand
newDebugCommand cmdText = do
    responseMVar ← newEmptyMVar
    stateVar ← newTVarIO DebugCommandQueued
    return (DebugCommand cmdText responseMVar stateVar)

-- | Claim a dequeued command for execution, atomically.
--
--   'True' means this caller — and only this caller — may now evaluate
--   it and publish its answer with 'completeDebugCommand'. 'False'
--   means it was cancelled first and must be DISCARDED unrun; its
--   reply already belongs to whoever cancelled it.
claimDebugCommand ∷ DebugCommand → IO Bool
claimDebugCommand cmd = atomically $ do
    st ← readTVar (dcState cmd)
    case st of
        DebugCommandQueued → do
            writeTVar (dcState cmd) DebugCommandClaimed
            return True
        _ → return False

-- | Cancel a command that has not been claimed, answering its waiting
--   client with @reply@.
--
--   The ONE cancellation operation: the response-wait expiry, the load
--   handoff (#763) and the shutdown\/crash drains all go through it, so
--   there is a single place where a cancellation can lose to a claim.
--   A cancellation that loses returns 'False' and touches neither the
--   lifecycle nor the response channel — the claimed command's own
--   answer is the only thing that may ever land there.
cancelDebugCommand ∷ DebugCommand → Text → IO Bool
cancelDebugCommand cmd reply = do
    won ← atomically $ do
        st ← readTVar (dcState cmd)
        case st of
            DebugCommandQueued → do
                writeTVar (dcState cmd) DebugCommandCancelled
                return True
            _ → return False
    when won $ void (tryPutMVar (dcResponse cmd) reply)
    return won

-- | Publish a CLAIMED command's answer without blocking.
--
--   'Control.Concurrent.MVar.tryPutMVar' rather than @putMVar@ on
--   purpose: by the time the evaluator finishes, the client that
--   queued the command may already have given up on it and reported
--   'commandUnknownOutcomeMessage'. A blocking put there would wedge
--   the single Lua thread on an answer nobody is coming back for
--   (#2282 requirement 4). Nothing is lost by the drop — the response
--   channel is per-command and that client never reads it again.
completeDebugCommand ∷ DebugCommand → Text → IO ()
completeDebugCommand cmd result = void (tryPutMVar (dcResponse cmd) result)

-- | The command's current lifecycle state, for a caller that needs to
--   observe rather than transition.
readDebugCommandState ∷ DebugCommand → IO DebugCommandState
readDebugCommandState = readTVarIO ∘ dcState

-- | What a client is told when its response wait expired and the
--   command had NOT been claimed — so it is now cancelled and can
--   never run.
--
--   Stable protocol text. It says CANCELLED rather than ERROR because
--   nothing failed and nothing happened: the session is exactly as the
--   client left it, so re-sending the line is safe.
commandCancelledMessage ∷ Text
commandCancelledMessage =
    "CANCELLED: command timed out before execution started"

-- | What a client is told when its response wait expired and the
--   command HAD been claimed.
--
--   Stable protocol text. It is deliberately not the old "Lua thread
--   may have crashed": the overwhelmingly likelier cause is a Lua tick
--   that ran long (a slow @update@ callback, a save\/load holding the
--   capture lock), and the command may well complete normally after
--   this line is sent. Re-sending it would apply the mutation twice.
commandUnknownOutcomeMessage ∷ Text
commandUnknownOutcomeMessage =
    "ERROR: command timed out after execution started; outcome unknown"

-- | Every finite bound the console holds a client to.
--
--   Production uses 'defaultDebugServerLimits' and nothing else; the
--   fields exist so a test can shrink a bound rather than wait one out
--   (a 300-second idle timeout is not a thing an hspec example can
--   observe, and a 64-connection cap is not a thing it should open).
--
--   Every field is finite by construction. \"No limit\" is deliberately
--   not representable: the unbounded versions of the first five are
--   exactly what issue #2170 was filed about, and the sixth
--   ('dslCommandResponseMicros') was already finite in production
--   before #2282 made it injectable.
data DebugServerLimits = DebugServerLimits
    { dslMaxConnections ∷ !Int
      -- ^ How many client connections may be ADMITTED at once, counted
      --   from admission through handler cleanup. A connection accepted
      --   past this receives 'connectionRefusedMessage' and is closed
      --   without a banner, without reaching the built-in table, and
      --   without ever occupying a slot.
    , dslMaxLineBytes ∷ !Int
      -- ^ The largest command line the console will assemble, in RAW
      --   RECEIVED BYTES EXCLUDING the newline — measured before UTF-8
      --   decoding and before whitespace trimming, so the bound is on
      --   the memory actually held rather than on whatever the bytes
      --   happen to decode to. A line of exactly this many bytes is
      --   valid; one byte more is refused, whether or not a newline
      --   ever arrives.
    , dslIdleTimeoutMicros ∷ !Int
      -- ^ How long a connection may sit with NO command in flight
      --   before it is closed. Idle time is time spent waiting on
      --   @recv@ and nothing else: a built-in that blocks for minutes
      --   ('world.waitForInit') and the
      --   'commandResponseTimeoutMicros' wait for the Lua thread are
      --   both time with a command in flight, and neither is measured
      --   here.
    , dslAcceptRetryBudget ∷ !Int
      -- ^ How many CONSECUTIVE recoverable @accept@ failures the
      --   listener absorbs before it declares the listening socket lost
      --   for good. Reset to zero by every successful accept, so a
      --   transient burst never accumulates across a healthy day.
    , dslAcceptRetryDelayMicros ∷ !Int
      -- ^ The base backoff between accept retries, doubled per
      --   consecutive failure ('acceptRetryDelayFor'). Non-zero in
      --   production so a persistent failure cannot spin the loop;
      --   zero in tests so the budget is exercised without waiting.
    , dslCommandResponseMicros ∷ !Int
      -- ^ How long a client waits for the LUA THREAD to answer a
      --   queued command before it gives up and tries to cancel it.
      --   'commandResponseTimeoutMicros' — 30 seconds — in production
      --   and nowhere else; the field exists (#2282) so an hspec
      --   example can drive the expiry in milliseconds rather than
      --   sitting out half a minute per case. Deliberately separate
      --   from 'dslIdleTimeoutMicros': this is time with a command IN
      --   FLIGHT and is never counted as idle.
    } deriving (Eq, Show)

-- | At most this many client connections at once.
--
--   @tools\/probelib.py@ opens ONE short-lived connection per console
--   command and closes it before the next, and @tools\/run_probes.py@
--   @--jobs@ concurrency runs separate engine PROCESSES on separate
--   ports, so the real concurrent demand against a single engine is one
--   connection plus whatever a developer has open by hand. 64 is two
--   orders of magnitude of headroom over that while still being a
--   number a file-descriptor table notices.
defaultMaxConnections ∷ Int
defaultMaxConnections = 64

-- | At most this many bytes in one command line, excluding the newline.
--
--   The console is single-line by contract (see the module haddock on
--   "Engine.Scripting.Lua.DebugServer"), and the longest thing anyone
--   sends through it is a chained one-liner. 64 KiB is far beyond that
--   and still bounds a hostile client's per-connection footprint at
--   something the process does not notice.
defaultMaxLineBytes ∷ Int
defaultMaxLineBytes = 65536

-- | Five minutes with no command in flight ends a connection.
--
--   Long enough that a human reading output between commands is never
--   dropped mid-session, short enough that a client which vanished
--   without a FIN (a sleeping laptop, a killed container) does not hold
--   a slot until the process exits.
defaultIdleTimeoutMicros ∷ Int
defaultIdleTimeoutMicros = 300000000

-- | Consecutive recoverable accept failures tolerated before the
--   listening socket is declared lost.
defaultAcceptRetryBudget ∷ Int
defaultAcceptRetryBudget = 8

-- | Base accept backoff: 50 ms, doubling per consecutive failure, so
--   the whole default budget spans a few seconds rather than spinning.
defaultAcceptRetryDelayMicros ∷ Int
defaultAcceptRetryDelayMicros = 50000

-- | How long a client waits for the LUA THREAD to answer a command it
--   forwarded: the PRODUCTION value of 'dslCommandResponseMicros', and
--   unchanged by #2170 and #2282 alike.
--
--   Deliberately separate from 'dslIdleTimeoutMicros': this is a
--   command IN FLIGHT. The guard exists because the Lua thread may
--   never answer at all — it died after claiming the command, or it is
--   simply taking longer than a client is willing to wait — which an
--   unbounded wait would turn into a permanently stuck connection.
--   What happens WHEN it expires is #2282's contract, not this
--   number's: see 'cancelDebugCommand'.
commandResponseTimeoutMicros ∷ Int
commandResponseTimeoutMicros = 30000000

-- | The backoff before the @n@-th consecutive accept retry (@n@ counted
--   from zero), doubling the base delay and capping the shift so a
--   large budget cannot overflow into a negative delay.
acceptRetryDelayFor ∷ DebugServerLimits → Int → Int
acceptRetryDelayFor limits n =
    dslAcceptRetryDelayMicros limits * (2 ^ min 8 (max 0 n ∷ Int))

-- | The production bounds, all six at once.
defaultDebugServerLimits ∷ DebugServerLimits
defaultDebugServerLimits = DebugServerLimits
    { dslMaxConnections         = defaultMaxConnections
    , dslMaxLineBytes           = defaultMaxLineBytes
    , dslIdleTimeoutMicros      = defaultIdleTimeoutMicros
    , dslAcceptRetryBudget      = defaultAcceptRetryBudget
    , dslAcceptRetryDelayMicros = defaultAcceptRetryDelayMicros
    , dslCommandResponseMicros  = commandResponseTimeoutMicros
    }

-- | Whether a failed @accept@ is worth retrying.
data AcceptDisposition
    = AcceptRetry
      -- ^ Transient: the listening socket is presumed still good, so
      --   the loop backs off and tries again while the budget lasts.
    | AcceptFatal
      -- ^ Positively terminal: the listening socket cannot serve
      --   another connection, so the loss is reported immediately
      --   rather than after the budget drains.
    deriving (Eq, Show)

-- | The default classification, by @IOError@ type.
--
--   Retryable are the errors that describe the ENVIRONMENT rather than
--   the socket: a momentarily exhausted descriptor table
--   (@EMFILE@\/@ENFILE@), a busy resource, an interrupted syscall, an
--   expired wait, and the unclassified @OtherError@ bucket that a
--   platform errno with no GHC mapping lands in — @ECONNABORTED@, the
--   one every accept loop must survive, is among those.
--
--   Everything else is terminal, and the two that matter are named
--   deliberately: @InvalidArgument@ is a listening socket whose
--   descriptor is gone, and @UserError@ is what @network@ raises for an
--   already-closed 'Network.Socket.Socket'. Neither can succeed on a
--   later attempt, so retrying them would only delay the report.
--
--   Being generous here is safe: 'dslAcceptRetryBudget' is finite, so a
--   misclassified terminal error costs a bounded backoff and is then
--   reported as terminal anyway. Being stingy is not — a recoverable
--   error classified as fatal takes a @--headless@ engine down.
classifyAcceptFailure ∷ SomeException → AcceptDisposition
classifyAcceptFailure e = case fromException e of
    Nothing              → AcceptFatal
    Just (ioe ∷ IOError) → case ioeGetErrorType ioe of
        ResourceExhausted → AcceptRetry
        ResourceBusy      → AcceptRetry
        Interrupted       → AcceptRetry
        TimeExpired       → AcceptRetry
        OtherError        → AcceptRetry
        _                 → AcceptFatal

-- | What a connection past 'dslMaxConnections' is told, verbatim and
--   alone: no banner precedes it and nothing follows it.
connectionRefusedMessage ∷ Int → Text
connectionRefusedMessage cap =
    "ERROR: debug console is at its connection limit ("
      <> tshow cap <> "); try again once a client disconnects"

-- | What a client that overran 'dslMaxLineBytes' is told before it is
--   disconnected.
lineTooLongMessage ∷ Int → Text
lineTooLongMessage cap =
    "ERROR: command line exceeds " <> tshow cap
      <> " bytes; connection closed"

-- | What an idle client is told before it is disconnected.
idleTimeoutMessage ∷ Int → Text
idleTimeoutMessage micros =
    "ERROR: idle for " <> tshow (max 1 (micros `div` 1000000))
      <> "s with no command in flight; connection closed"

-- | The stderr line for a recoverable accept failure. Names the port,
--   the cause, and — the part that makes it distinguishable from a
--   terminal loss at a glance — that the listener is still going.
listenerRetryMessage ∷ Int → Int → Text → Text
listenerRetryMessage port remaining cause =
    "synarchy: debug console listener on port " <> tshow port
      <> " failed to accept a connection: " <> cause
      <> " -- retrying (" <> tshow remaining <> " attempt"
      <> (if remaining ≡ 1 then "" else "s") <> " left)."

-- | The stderr line for a listening socket lost for good. Names the
--   port and the cause; whether the engine then stops is the boot
--   MODE's decision, stated in
--   'Engine.Scripting.Lua.DebugServer.listenerLossResponse'.
listenerLostMessage ∷ Int → Text → Text
listenerLostMessage port cause =
    "synarchy: debug console listener on port " <> tshow port
      <> " was lost and cannot accept further connections: " <> cause

-- | Everything 'Engine.Scripting.Lua.DebugServer.Listener.startDebugServer'
--   needs, in one record so a test can override one field and inherit
--   the rest from 'defaultDebugServerConfig'.
data DebugServerConfig = DebugServerConfig
    { dscPort ∷ !Int
      -- ^ The loopback port to bind. @0@ is issue #46's sentinel: NO
      --   listener at all, honoured for any caller, because this layer
      --   sees a number and no boot mode. Refusing that 0 where it is
      --   wrong is 'Engine.Scripting.Lua.DebugServer.listenerAction's
      --   job (#1190).
    , dscBuiltin ∷ !(Text → IO (Maybe Text))
      -- ^ Consulted on the client thread BEFORE a command is marshaled
      --   to the Lua thread: @Just resp@ is handled here and answered,
      --   @Nothing@ falls through to the queue.
    , dscLimits ∷ !DebugServerLimits
    , dscAccept ∷ !(Socket → IO (Socket, SockAddr))
      -- ^ The accept seam, 'Network.Socket.accept' in production. A
      --   test substitutes an action that fails once and then delegates,
      --   which is the only way to prove the supervision loop survives a
      --   transient error without waiting for the operating system to
      --   produce one.
    , dscClassify ∷ !(SomeException → AcceptDisposition)
      -- ^ The classification seam, 'classifyAcceptFailure' in
      --   production.
    , dscOnRetry ∷ !(Int → Text → IO ())
      -- ^ Called once per recoverable accept failure with the attempts
      --   remaining and the cause. Never called for an intentional
      --   'Engine.Scripting.Lua.DebugServer.Listener.stopDebugConsole'.
    , dscOnLoss ∷ !(Text → IO ())
      -- ^ Called AT MOST ONCE, with the cause, when the listening
      --   socket is lost for good — budget exhausted or a positively
      --   terminal error. Never called for an intentional stop. The
      --   production caller reports it on stderr and, in a
      --   'Engine.Scripting.Lua.DebugServer.ConsoleRequired' mode, asks
      --   the engine to shut down.
    }

-- | The production configuration for a port and a built-in table:
--   'defaultDebugServerLimits', the real 'Network.Socket.accept', the
--   default classification, and stderr diagnostics for both the retry
--   and the loss.
--
--   A caller that needs more than stderr on loss — the Lua thread,
--   which must also stop a @--headless@ engine — overrides 'dscOnLoss'
--   on top of this rather than assembling a record of its own, so the
--   four fields it does not care about cannot drift from production.
defaultDebugServerConfig ∷ Int → (Text → IO (Maybe Text)) → DebugServerConfig
defaultDebugServerConfig port builtin = DebugServerConfig
    { dscPort     = port
    , dscBuiltin  = builtin
    , dscLimits   = defaultDebugServerLimits
    , dscAccept   = accept
    , dscClassify = classifyAcceptFailure
    , dscOnRetry  = \remaining cause →
        putStderrLine (listenerRetryMessage port remaining cause)
    , dscOnLoss   = \cause →
        putStderrLine (listenerLostMessage port cause)
    }

-- | Write a diagnostic to stderr, BEST EFFORT.
--
--   A closed stderr, or a consumer that went away, must not become an
--   exception in the accept loop: the retry hook would stop retrying
--   and the loss hook would skip whatever the caller layered after it.
--   A diagnostic nobody can read is a lost diagnostic, not a fault.
putStderrLine ∷ Text → IO ()
putStderrLine t = void ∘ tryIOError $
    hPutStrLn stderr (T.unpack t) >> hFlush stderr

-- | What a started console hands back: the queue the Lua thread polls,
--   and the listener to stop at shutdown.
--
--   'consoleListener' is 'Nothing' for exactly two consoles, and both
--   are inert by construction: the port-0 sentinel (#46) and the
--   never-fed queue a 'ConsoleOptional' mode falls back to when its
--   bind failed (#1190). 'stopDebugConsole' on either is a no-op, so
--   the shutdown path needs no case of its own.
data DebugConsole = DebugConsole
    { consoleQueue    ∷ !(TQueue DebugCommand)
    , consoleListener ∷ !(Maybe DebugListener)
    }

-- | The named owner of the accept loop and of every admitted client.
--
--   Existing so that shutdown is a thing someone DOES rather than a
--   thing the process exit happens to accomplish: before #2170 the
--   accept thread's handle was discarded at the fork
--   (@_ <- forkIO ...@) and so was every client's, which is why a
--   listener could die unobserved and why an accept thread could
--   outlive the Lua worker that started it.
data DebugListener = DebugListener
    { dlSocket ∷ !Socket
      -- ^ The listening socket. Closed by 'stopDebugConsole' and by the
      --   accept loop's own @finally@, whichever happens first.
    , dlStopping ∷ !(TVar Bool)
      -- ^ Set by 'stopDebugConsole' BEFORE the socket is closed, which
      --   is what lets the accept loop tell the resulting @accept@
      --   failure from an unexpected one: an intentional stop emits
      --   neither a retry diagnostic nor a loss callback.
    , dlLossReported ∷ !(TVar Bool)
      -- ^ Latches the at-most-once loss callback.
    , dlClients ∷ !(TVar (Map.Map Int ClientHandle))
      -- ^ Every ADMITTED client, by slot id. Its size is the connection
      --   count the cap is compared against: an entry is inserted
      --   before the handler is forked and removed in that handler's
      --   own @finally@, so the count spans admission through cleanup
      --   with no window either side.
    , dlNextClientId ∷ !(TVar Int)
    , dlAcceptThread ∷ !(TVar (Maybe ThreadId))
    , dlAcceptDone ∷ !(MVar ())
      -- ^ Filled exactly once when the accept loop exits, so a stop
      --   genuinely JOINS the thread rather than reading back the flag
      --   it just set.
    }

-- | One admitted client, enough of it to close, kill and join.
data ClientHandle = ClientHandle
    { chSocket ∷ !Socket
    , chThread ∷ !(MVar ThreadId)
      -- ^ Filled immediately after the fork. Read with
      --   'Control.Concurrent.MVar.tryReadMVar' at shutdown, which is
      --   why the slot is registered BEFORE the fork: the handler's own
      --   deregistration can then never race ahead of its registration.
    , chDone ∷ !(MVar ())
    }

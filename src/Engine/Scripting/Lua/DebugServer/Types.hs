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
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (SomeException, fromException)
import GHC.IO.Exception (IOErrorType(..))
import Network.Socket (Socket, SockAddr, accept)
import System.IO (hPutStrLn, hFlush, stderr)
import System.IO.Error (ioeGetErrorType, tryIOError)

-- | One line of Lua handed from a client connection to the Lua thread,
--   with the channel its answer comes back on.
data DebugCommand = DebugCommand
    { dcCommand  ∷ !Text      -- ^ Lua code to evaluate
    , dcResponse ∷ !(MVar Text)  -- ^ Response channel
    }

-- | Every finite bound the console holds a client to.
--
--   Production uses 'defaultDebugServerLimits' and nothing else; the
--   fields exist so a test can shrink a bound rather than wait one out
--   (a 300-second idle timeout is not a thing an hspec example can
--   observe, and a 64-connection cap is not a thing it should open).
--
--   Every field is finite by construction. \"No limit\" is deliberately
--   not representable: the unbounded versions of all five are exactly
--   what issue #2170 was filed about.
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
--   forwarded. Unchanged by #2170 and deliberately separate from
--   'dslIdleTimeoutMicros': this is a command IN FLIGHT, and the guard
--   exists for a Lua thread that dequeued a command and then died
--   before filling the response, which an unbounded wait would turn
--   into a permanently stuck connection.
commandResponseTimeoutMicros ∷ Int
commandResponseTimeoutMicros = 30000000

-- | The backoff before the @n@-th consecutive accept retry (@n@ counted
--   from zero), doubling the base delay and capping the shift so a
--   large budget cannot overflow into a negative delay.
acceptRetryDelayFor ∷ DebugServerLimits → Int → Int
acceptRetryDelayFor limits n =
    dslAcceptRetryDelayMicros limits * (2 ^ min 8 (max 0 n ∷ Int))

-- | The production bounds, all five at once.
defaultDebugServerLimits ∷ DebugServerLimits
defaultDebugServerLimits = DebugServerLimits
    { dslMaxConnections         = defaultMaxConnections
    , dslMaxLineBytes           = defaultMaxLineBytes
    , dslIdleTimeoutMicros      = defaultIdleTimeoutMicros
    , dslAcceptRetryBudget      = defaultAcceptRetryBudget
    , dslAcceptRetryDelayMicros = defaultAcceptRetryDelayMicros
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

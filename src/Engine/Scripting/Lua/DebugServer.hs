{-# LANGUAGE Strict #-}
-- | The debug console: a TCP line protocol that evaluates Lua inside a
--   running engine.
--
--   == Trust model
--
--   This is stated here because nothing in the tree said it, and every
--   bound below reads as a security control if you do not know that it
--   is not one.
--
--   * __The transport IS the boundary.__ The listener binds
--     @127.0.0.1@ and only @127.0.0.1@, so reaching it already requires
--     code execution on the host. Binding any other address is out of
--     scope by decision (#2170), not by omission.
--   * __The host user is the security principal.__ There is no
--     authentication, no capability negotiation, and no per-client
--     trust decision, because every client is by construction the
--     person who is already running the process.
--   * __Every connection holds full evaluator authority.__ A line that
--     is not a built-in reaches the complete Lua state — the whole
--     engine API, the filesystem through it, and @engine.quit()@. There
--     is no reduced surface for a \"less trusted\" client, because
--     there is no such client.
--   * __The bounds are resource limits, not access control.__ The
--     connection cap, the line cap and the idle timeout
--     ("Engine.Scripting.Lua.DebugServer.Types") exist so a stuck or
--     buggy client cannot exhaust the process, and raising one cannot
--     make the console less trusted than it already is by design.
--
--   That shape is deliberate for a developer-and-agent console on a
--   pre-release game. Changing it — authentication, a narrowed Lua
--   surface, a non-loopback bind — is a product decision, not a
--   hardening pass.
--
--   == Layout
--
--   * "Engine.Scripting.Lua.DebugServer.Types" — the command record,
--     the finite bounds, the injection seams, the handles.
--   * "Engine.Scripting.Lua.DebugServer.Client" — one connection, from
--     banner to close.
--   * "Engine.Scripting.Lua.DebugServer.Listener" — the supervised
--     accept loop and its owner.
--
--   This module keeps the per-boot-mode policy (#1190, #2170) and
--   re-exports the rest, so every existing importer is unchanged.
module Engine.Scripting.Lua.DebugServer
    ( -- * Re-exported console
      DebugCommand(..)
    , DebugConsole(..)
    , DebugListener
    , DebugServerConfig(..)
    , DebugServerLimits(..)
    , defaultDebugServerConfig
    , defaultDebugServerLimits
    , defaultMaxConnections
    , defaultMaxLineBytes
    , defaultIdleTimeoutMicros
    , defaultAcceptRetryBudget
    , defaultAcceptRetryDelayMicros
    , commandResponseTimeoutMicros
    , AcceptDisposition(..)
    , classifyAcceptFailure
    , connectionRefusedMessage
    , lineTooLongMessage
    , idleTimeoutMessage
    , listenerRetryMessage
    , listenerLostMessage
    , startDebugServer
    , stopDebugConsole
    , inertDebugConsole
    , pollDebugCommand
      -- * Per-boot-mode listener policy (#1190)
    , DebugConsolePolicy(..)
    , debugConsolePolicy
    , ListenerAction(..)
    , listenerAction
    , DebugListenerFailure(..)
    , debugListenerFailureMessage
    , reportDebugListenerFailure
    , reportBootCleanup
      -- * Per-boot-mode response to a listener LOST after boot (#2170)
    , ListenerLossResponse(..)
    , listenerLossResponse
    , handleDebugListenerLoss
    , handleDebugListenerLossWith
    ) where

import UPrelude
import Engine.Core.Types (BootMode(..), bootModeName)
import Engine.Scripting.Lua.DebugServer.Types
import Engine.Scripting.Lua.DebugServer.Listener
import qualified Data.Text as T
import Control.Exception (SomeException, try)
import System.IO (hPutStrLn, hFlush, stderr)
import System.IO.Error (tryIOError)

-- | Whether a boot mode can run without a debug console (#1190).
--
--   @--headless@ and @--offscreen@ have no window and no other
--   interactive surface, so the TCP console is their ONLY control
--   channel: with a dead listener there is no way to reach
--   @engine.quit()@, and no @READY@ line for the documented
--   wait-for-boot pattern to ever match. The other three modes each
--   have a real alternative — @--dump@ writes its JSON and exits,
--   @--graphical@ and @--preview@ have a window and a keyboard — so a
--   missing console is a degradation there, not a dead end.
data DebugConsolePolicy
    = ConsoleRequired
    | ConsoleOptional
    deriving (Eq, Show)

-- | The policy for each boot mode, stated one constructor at a time so
--   a new mode is a compile error here rather than a silent inheritance
--   of whichever branch a wildcard happened to point at.
debugConsolePolicy ∷ BootMode → DebugConsolePolicy
debugConsolePolicy ModeDump      = ConsoleOptional
debugConsolePolicy ModeHeadless  = ConsoleRequired
debugConsolePolicy ModeOffscreen = ConsoleRequired
debugConsolePolicy ModeGraphical = ConsoleOptional
debugConsolePolicy ModePreview   = ConsoleOptional

-- | What a boot mode must do about its debug console, decided from the
--   mode and its effective port BEFORE any socket is touched.
data ListenerAction
    = TolerateListener
      -- ^ Start whatever 'startDebugServer' gives back, including the
      --   port-0 no-listener branch, and survive a 'Left'.
    | RequireListener
      -- ^ Attempt the bind; a 'Left' is fatal to the boot.
    | RejectPortZero
      -- ^ Fail without touching a socket: port 0 means \"no listener at
      --   all\", which this mode cannot survive.
    deriving (Eq, Show)

-- | The mode-aware port-0 dispatch. Port 0 is @--dump@'s sentinel
--   (issue #46) and 'startDebugServer' honours it for any caller, which
--   is exactly why the decision has to be made HERE, with the mode in
--   hand, rather than inside a function that only ever sees a number.
listenerAction ∷ BootMode → Int → ListenerAction
listenerAction mode port = case debugConsolePolicy mode of
    ConsoleOptional → TolerateListener
    ConsoleRequired
        | port ≡ 0  → RejectPortZero
        | otherwise → RequireListener

-- | Why a console-required boot has no usable debug console.
data DebugListenerFailure
    = ListenerPortZero
      -- ^ The effective port was 0 — @--dump@'s no-listener sentinel,
      --   applied outside @--dump@.
    | ListenerBindFailed !Text
      -- ^ 'startDebugServer' returned 'Left': no address, a malformed
      --   or out-of-range service, or a port already in use.
    deriving (Eq, Show)

-- | The stderr diagnostic a console-required boot dies with. Names the
--   selected mode, the effective port, and the specific cause — enough
--   for an operator (or an agent whose @READY@ wait just failed) to
--   tell an occupied port from a mistyped one.
debugListenerFailureMessage ∷ BootMode → Int → DebugListenerFailure → Text
debugListenerFailureMessage mode port failure =
    "synarchy: " <> bootModeName mode
      <> " mode requires a working debug console, but " <> reason
      <> " -- boot aborted."
  where
    reason = case failure of
        ListenerPortZero →
            "the effective debug port is " <> tshow port
              <> ", which requests no TCP listener at all (that sentinel "
              <> "belongs to --dump only)"
        ListenerBindFailed err →
            "the debug listener on port " <> tshow port
              <> " failed to start: " <> err

-- | Write 'debugListenerFailureMessage' to stderr.
--
--   Boot markers for the console live on the std handles, not the
--   engine log: 'startDebugServer' already prints @READY@ that way
--   (stdout for a live listener, stderr for the port-0 no-listener
--   branch) because that is the channel the documented agent boot
--   pattern watches. A failure that means no @READY@ will EVER arrive
--   belongs on the same channel — stderr, so a mode whose stdout is
--   reserved for data is unaffected.
reportDebugListenerFailure ∷ BootMode → Int → DebugListenerFailure → IO ()
reportDebugListenerFailure mode port failure =
    putStderrLine (debugListenerFailureMessage mode port failure)

-- | One line of the boot-failure cleanup trace, on the same stderr
--   channel as 'reportDebugListenerFailure'.
--
--   Every line is written BY the step that performed the cleanup, at
--   the point it performed it, so the trace is evidence rather than a
--   claim: the process exits either way, and the OS reclaims threads
--   and descriptors regardless, so @ps@ alone can never show that a
--   partially started worker was actually stopped or that a Lua state
--   was actually closed.
reportBootCleanup ∷ Text → IO ()
reportBootCleanup detail =
    putStderrLine ("synarchy: boot cleanup: " <> detail)

-- | What a boot mode does about a listener lost AFTER it bound (#2170),
--   as data rather than as effects, so both halves are assertable
--   without capturing a handle.
--
--   The two failures are deliberately separate: #1190's
--   'DebugListenerFailure' is a listener that never STARTED and aborts
--   the boot, while this is one that started, printed @READY@, served
--   connections, and then died. A client that followed the documented
--   wait-for-@READY@ contract has already connected by then, so
--   aborting the boot is not available — the engine has to be shut
--   down instead.
data ListenerLossResponse = ListenerLossResponse
    { llrMessage ∷ !Text
      -- ^ The stderr line. Names the port and the cause in EVERY mode:
      --   a console-optional engine goes on running, but the operator
      --   still has to be told the console it may be about to use is
      --   gone.
    , llrShutdown ∷ !Bool
      -- ^ Whether the engine must now stop. True exactly for a
      --   'ConsoleRequired' mode, whose only control surface this was.
    } deriving (Eq, Show)

-- | The response for a mode, port and cause.
listenerLossResponse ∷ BootMode → Int → Text → ListenerLossResponse
listenerLossResponse mode port cause = case debugConsolePolicy mode of
    ConsoleRequired → ListenerLossResponse
        { llrMessage = base
            <> " -- " <> bootModeName mode
            <> " mode has no other control surface, so the engine is "
            <> "shutting down."
        , llrShutdown = True
        }
    ConsoleOptional → ListenerLossResponse
        { llrMessage = base
            <> " -- " <> bootModeName mode
            <> " mode continues without a console."
        , llrShutdown = False
        }
  where
    base = listenerLostMessage port cause

-- | React to a lost listener: run @onShutdown@ if the mode requires it,
--   then report the loss on stderr.
--
--   That order, and the best-effort write, are both load-bearing. This
--   used to report FIRST and hand the decision back as a 'Bool' for the
--   caller to act on — so a closed stderr, or a consumer that had gone
--   away, raised out of the diagnostic and the shutdown never
--   happened. In @--headless@ that recreates the exact failure the hook
--   exists to prevent: a live engine, a dead listener, and no reachable
--   @engine.quit()@. The transition cannot depend on a diagnostic
--   landing.
--
--   @onShutdown@ is the caller's, because this module deliberately
--   knows nothing of 'Engine.Core.State.EngineEnv'; it is run at most
--   once per loss, and only for a 'ConsoleRequired' mode.
handleDebugListenerLoss ∷ BootMode → Int → IO () → Text → IO ()
handleDebugListenerLoss = handleDebugListenerLossWith putStderrLine

-- | 'handleDebugListenerLoss' with the report sink injected, so a test
--   can prove the shutdown survives a sink that raises.
handleDebugListenerLossWith ∷ (Text → IO ()) → BootMode → Int → IO ()
                            → Text → IO ()
handleDebugListenerLossWith sink mode port onShutdown cause = do
    let response = listenerLossResponse mode port cause
    when (llrShutdown response) onShutdown
    void ∘ tryAnyIO $ sink (llrMessage response)

putStderrLine ∷ Text → IO ()
putStderrLine t = void ∘ tryIOError $
    hPutStrLn stderr (T.unpack t) >> hFlush stderr

tryAnyIO ∷ IO α → IO (Either SomeException α)
tryAnyIO = try

{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.DebugServer
    ( DebugCommand(..)
    , startDebugServer
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
    ) where

import UPrelude
import Engine.Core.Types (BootMode(..), bootModeName)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Concurrent (forkIO)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.Timeout (timeout)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Exception (SomeException, try, onException, finally)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

data DebugCommand = DebugCommand
    { dcCommand  ∷ !Text      -- ^ Lua code to evaluate
    , dcResponse ∷ !(MVar Text)  -- ^ Response channel
    }

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
            "the effective debug port is " <> T.pack (show port)
              <> ", which requests no TCP listener at all (that sentinel "
              <> "belongs to --dump only)"
        ListenerBindFailed err →
            "the debug listener on port " <> T.pack (show port)
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
reportDebugListenerFailure mode port failure = do
    hPutStrLn stderr $ T.unpack (debugListenerFailureMessage mode port failure)
    hFlush stderr

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
reportBootCleanup detail = do
    hPutStrLn stderr $ T.unpack ("synarchy: boot cleanup: " <> detail)
    hFlush stderr

-- | Start the debug TCP server on the given port.
--   Returns a TQueue that the Lua thread polls for commands, or the
--   error text when the server could not start (port already in use,
--   no address). Binding happens synchronously so a failure reaches
--   the caller — previously it killed a forked thread silently while
--   the engine logged "listening". Only the accept loop is forked.
--
--   @builtin@ is consulted on the per-connection client thread BEFORE a
--   command is marshaled to the Lua thread: if it returns @Just resp@
--   the command is handled here (off the Lua thread) and @resp@ is sent
--   back; @Nothing@ falls through to the Lua thread as before. This is
--   how long-blocking ops ('world.waitForInit'/'waitForChunks') avoid
--   monopolising the single Lua thread — they only poll world-state
--   refs, so the client thread can run them while the Lua thread keeps
--   serving other connections.
startDebugServer ∷ Int → (Text → IO (Maybe Text))
                 → IO (Either Text (TQueue DebugCommand))
startDebugServer 0 _ = do
    -- Port 0 means no TCP listener at all. Binding to port 0 would ask
    -- the OS for an ephemeral port, contradicting the "no TCP server"
    -- dump-mode contract (#46) and opening a network surface.
    -- Emit the ready marker on stderr (stdout is reserved for JSON) and
    -- hand back an inert queue that nothing ever feeds.
    --
    -- This function has no boot-mode context, so it cannot tell dump's
    -- deliberate sentinel from a console-required mode that was handed
    -- a 0: that is 'listenerAction's job, and a 'ConsoleRequired' mode
    -- never reaches this branch (#1190).
    hPutStrLn stderr "READY port=0"
    hFlush stderr
    Right <$> atomically newTQueue
startDebugServer port builtin = do
    cmdQueue ← atomically newTQueue
    r ← try $ do
        let hints = defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
        addrs ← getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
        addr ← case addrs of
            (a:_) → return a
            []    → ioError (userError "getAddrInfo returned no addresses")
        sock ← openSocket addr
        (do setSocketOption sock ReuseAddr 1
            bind sock (addrAddress addr)
            listen sock 4) `onException` close sock
        return sock
    case r of
        Left (e ∷ SomeException) → return (Left (T.pack (show e)))
        Right sock → do
            -- Ready signal on stdout — agents can wait for this line
            -- to know the debug console is accepting connections.
            -- (Dump mode, port 0, is handled above and never reaches here.)
            hPutStrLn stdout ("READY port=" <> show port)
            hFlush stdout
            _ ← forkIO $ acceptLoop sock cmdQueue builtin `finally` close sock
            return (Right cmdQueue)

pollDebugCommand ∷ TQueue DebugCommand → IO (Maybe DebugCommand)
pollDebugCommand = atomically . tryReadTQueue

acceptLoop ∷ Socket → TQueue DebugCommand → (Text → IO (Maybe Text)) → IO ()
acceptLoop sock cmdQueue builtin = do
    (conn, _) ← accept sock
    _ ← forkIO $ handleClient conn cmdQueue builtin
    acceptLoop sock cmdQueue builtin

handleClient ∷ Socket → TQueue DebugCommand → (Text → IO (Maybe Text)) → IO ()
handleClient conn cmdQueue builtin =
    (do sendAll conn "synarchy debug console\n> "
        clientLoop conn cmdQueue builtin BS.empty
    ) `finally` close conn

clientLoop ∷ Socket → TQueue DebugCommand → (Text → IO (Maybe Text))
          → BS.ByteString → IO ()
clientLoop conn cmdQueue builtin leftover = do
    chunk ← recv conn 4096
    if BS.null chunk
        then return ()  -- client disconnected
        else do
            let buf = leftover <> chunk
            processLines conn cmdQueue builtin buf

processLines ∷ Socket → TQueue DebugCommand → (Text → IO (Maybe Text))
            → BS.ByteString → IO ()
processLines conn cmdQueue builtin buf =
    case BS8.elemIndex '\n' buf of
        Nothing → clientLoop conn cmdQueue builtin buf  -- no complete line yet
        Just idx →
            let (line, rest) = BS.splitAt idx buf
                remaining = BS.drop 1 rest  -- skip the \n
                cmdText = T.strip $ TE.decodeUtf8Lenient line
            in if T.null cmdText
               then do
                   sendAll conn "> "
                   processLines conn cmdQueue builtin remaining
               else do
                   -- Built-ins (long-blocking waits) run HERE, on the
                   -- client thread, so they never freeze the Lua thread.
                   mBuiltin ← builtin cmdText
                   result ← case mBuiltin of
                       Just r  → return r
                       Nothing → do
                           responseMVar ← newEmptyMVar
                           atomically $ writeTQueue cmdQueue
                                          (DebugCommand cmdText responseMVar)
                           -- Wait for Lua thread to process and respond.
                           -- Timeout guards against deadlock: if the Lua
                           -- thread crashes after dequeuing the command
                           -- but before filling the MVar, an unbounded
                           -- takeMVar would block forever (the crash
                           -- handler only drains the TQueue, not
                           -- already-dequeued commands).
                           mResult ← timeout 30000000 (takeMVar responseMVar)
                           return $ fromMaybe
                             "ERROR: command timed out (Lua thread may have crashed)"
                             mResult
                   sendAll conn (TE.encodeUtf8 result)
                   sendAll conn "\n> "
                   processLines conn cmdQueue builtin remaining

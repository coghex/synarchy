{-# LANGUAGE Strict #-}
-- | The supervised accept loop and its named owner (#2170).
--
--   Before this, 'startDebugServer' forked the accept loop and threw
--   the handle away, and the loop had no exception boundary at all: the
--   first @accept@ failure ended it, the @finally@ closed the listening
--   socket, and nothing else ever found out. In @--headless@ and
--   @--offscreen@ — where #1190 already made a listener that cannot
--   START fatal — a listener that DIED after @READY@ left a live
--   process with no reachable @engine.quit()@. Each accepted connection
--   was equally unmanaged: no cap, no idle timeout, and a line buffer
--   that grew for as long as a client withheld a newline.
--
--   What this module owns now:
--
--   * a finite retry budget with backoff around @accept@, so a
--     transient failure is logged and survived
--     ('Engine.Scripting.Lua.DebugServer.Types.classifyAcceptFailure'
--     decides which failures those are);
--   * one at-most-once loss callback when the socket is lost for good,
--     which the Lua thread turns into a stderr line and, in a
--     console-required mode, an engine shutdown;
--   * a 'DebugListener' handle whose 'stopDebugConsole' is idempotent,
--     closes the listening socket, joins the accept thread, and closes,
--     kills and joins every admitted client — so no console thread
--     outlives the Lua worker that started it;
--   * the connection cap, counted from admission through handler
--     cleanup.
--
--   An INTENTIONAL stop is not a loss: 'dlStopping' is set before the
--   socket is closed, so the @accept@ failure that close provokes emits
--   neither a retry diagnostic nor the loss callback.
module Engine.Scripting.Lua.DebugServer.Listener
    ( startDebugServer
    , stopDebugConsole
    , pollDebugCommand
    , inertDebugConsole
    ) where

import UPrelude
import Engine.Scripting.Lua.DebugServer.Types
import Engine.Scripting.Lua.DebugServer.Client (serveClient, refuseClient)
import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar
    (newEmptyMVar, readMVar, tryPutMVar, tryReadMVar)
import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar, modifyTVar')
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, tryReadTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException, try, onException, finally)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.Timeout (timeout)
import Network.Socket

-- | Start the debug TCP server described by the config.
--
--   Returns the queue the Lua thread polls plus the listener to stop at
--   shutdown, or the error text when the server could not start (port
--   already in use, no address). Binding happens synchronously so a
--   failure reaches the caller — it used to kill a forked thread
--   silently while the engine logged \"listening\". Only the accept
--   loop is forked.
--
--   Port @0@ is issue #46's sentinel: NO TCP listener at all. The ready
--   marker goes to stderr (stdout is reserved for @--dump@'s JSON) and
--   the caller gets an inert queue nothing ever feeds. This function
--   has no boot-mode context, so it cannot tell dump's deliberate
--   sentinel from a console-required mode that was handed a @0@; that
--   is 'Engine.Scripting.Lua.DebugServer.listenerAction's job, and a
--   console-required mode never reaches this branch (#1190).
startDebugServer ∷ DebugServerConfig → IO (Either Text DebugConsole)
startDebugServer cfg
  | dscPort cfg ≡ 0 = do
      hPutStrLn stderr "READY port=0"
      hFlush stderr
      Right ⊚ inertDebugConsole
  | otherwise = do
      cmdQueue ← atomically newTQueue
      r ← try $ do
          let hints = defaultHints
                  { addrFlags = [AI_PASSIVE]
                  , addrSocketType = Stream
                  }
          addrs ← getAddrInfo (Just hints) (Just "127.0.0.1")
                              (Just (show (dscPort cfg)))
          addr ← case addrs of
              (a:_) → return a
              []    → ioError (userError "getAddrInfo returned no addresses")
          sock ← openSocket addr
          (do setSocketOption sock ReuseAddr 1
              bind sock (addrAddress addr)
              -- The backlog is a KERNEL queue depth for connections not
              -- yet accepted, never a concurrency limit; the limit is
              -- 'dslMaxConnections', enforced at admission below.
              listen sock 4) `onException` close sock
          return sock
      case r of
          Left (e ∷ SomeException) → return (Left (tshow e))
          Right sock → do
              listener ← newListener sock
              -- Ready signal on stdout — agents can wait for this line
              -- to know the debug console is accepting connections.
              -- (Port 0 is handled above and never reaches here.)
              hPutStrLn stdout ("READY port=" <> show (dscPort cfg))
              hFlush stdout
              tid ← forkIO $
                  acceptLoop cfg listener cmdQueue 0
                    `finally` (close sock >> void (tryPutMVar (dlAcceptDone listener) ()))
              atomically $ writeTVar (dlAcceptThread listener) (Just tid)
              return $ Right DebugConsole
                  { consoleQueue    = cmdQueue
                  , consoleListener = Just listener
                  }

-- | A console with a queue nothing ever feeds and no listener to stop.
--
--   Two callers, both deliberately inert: the port-0 sentinel above,
--   and a 'Engine.Scripting.Lua.DebugServer.ConsoleOptional' mode whose
--   bind failed and which keeps running without a console (#1190).
inertDebugConsole ∷ IO DebugConsole
inertDebugConsole = do
    q ← atomically newTQueue
    return DebugConsole { consoleQueue = q, consoleListener = Nothing }

pollDebugCommand ∷ TQueue DebugCommand → IO (Maybe DebugCommand)
pollDebugCommand = atomically ∘ tryReadTQueue

newListener ∷ Socket → IO DebugListener
newListener sock = DebugListener sock
    ⊚ newTVarIO False
    ⊛ newTVarIO False
    ⊛ newTVarIO Map.empty
    ⊛ newTVarIO 1
    ⊛ newTVarIO Nothing
    ⊛ newEmptyMVar

-- | Stop the console: idempotent, and a no-op for an inert one.
--
--   Order is load-bearing. 'dlStopping' is set FIRST, so the accept
--   failure the close provokes is recognised as intentional and reports
--   nothing. Then the listening socket is closed (no new admissions),
--   then the accept thread is joined, and only then is every admitted
--   client closed, killed and joined — closing a client's socket
--   releases it from @recv@, and the kill releases one parked in the
--   30-second response wait, which a close alone would not.
--
--   The whole teardown is bounded: a client that somehow refuses to die
--   must not hold up engine shutdown, so each join is time-boxed and
--   the process's own exit reclaims whatever is left.
stopDebugConsole ∷ DebugConsole → IO ()
stopDebugConsole console = case consoleListener console of
    Nothing       → return ()
    Just listener → do
        alreadyStopping ← atomically $ swapTVarBool (dlStopping listener) True
        unless alreadyStopping $ do
            void ∘ tryAny ∘ close $ dlSocket listener
            joinAcceptThread listener
            clients ← atomically $ do
                current ← readTVar (dlClients listener)
                writeTVar (dlClients listener) Map.empty
                return (Map.elems current)
            mapM_ stopClient clients

-- | Wait for the accept loop to exit, killing it if the close alone did
--   not wake it.
joinAcceptThread ∷ DebugListener → IO ()
joinAcceptThread listener = do
    joined ← timeout listenerJoinMicros (readMVar (dlAcceptDone listener))
    when (isNothing joined) $ do
        mTid ← readTVarIO (dlAcceptThread listener)
        forM_ mTid $ \tid → void ∘ tryAny $ killThread tid
        void ∘ timeout listenerJoinMicros ∘ readMVar $ dlAcceptDone listener

stopClient ∷ ClientHandle → IO ()
stopClient handle = do
    void ∘ tryAny ∘ close $ chSocket handle
    mTid ← tryReadMVar (chThread handle)
    forM_ mTid $ \tid → void ∘ tryAny $ killThread tid
    void ∘ timeout listenerJoinMicros ∘ readMVar $ chDone handle

-- | How long a shutdown waits on any one console thread. Generous
--   relative to what these threads do after a close-and-kill, and
--   finite so a wedged one cannot hold the engine open.
listenerJoinMicros ∷ Int
listenerJoinMicros = 2000000

-- | The supervised accept loop. @failures@ is the count of CONSECUTIVE
--   recoverable failures so far; a successful accept resets it, so a
--   transient burst never accumulates across a healthy run.
acceptLoop ∷ DebugServerConfig → DebugListener → TQueue DebugCommand
           → Int → IO ()
acceptLoop cfg listener cmdQueue failures = do
    r ← try (dscAccept cfg (dlSocket listener))
    case r of
        Right (conn, _) → do
            admit cfg listener cmdQueue conn
            acceptLoop cfg listener cmdQueue 0
        Left (e ∷ SomeException) → do
            stopping ← readTVarIO (dlStopping listener)
            -- An intentional stop closed the socket out from under this
            -- accept. That is not a loss and gets no diagnostic.
            unless stopping $ handleAcceptFailure cfg listener cmdQueue
                                                  failures e

handleAcceptFailure ∷ DebugServerConfig → DebugListener → TQueue DebugCommand
                    → Int → SomeException → IO ()
handleAcceptFailure cfg listener cmdQueue failures e =
    case dscClassify cfg e of
        AcceptFatal → reportLoss cfg listener cause
        AcceptRetry
            | remaining ≤ 0 → reportLoss cfg listener $
                cause <> " (exhausted " <> tshow budget
                      <> " consecutive accept retries)"
            | otherwise → do
                dscOnRetry cfg remaining cause
                threadDelay (acceptRetryDelayFor (dscLimits cfg) failures)
                acceptLoop cfg listener cmdQueue (failures + 1)
  where
    cause     = tshow e
    budget    = dslAcceptRetryBudget (dscLimits cfg)
    remaining = budget - failures

-- | Announce a listening socket lost for good, exactly once. Returning
--   ends the accept loop, and its @finally@ closes the socket.
reportLoss ∷ DebugServerConfig → DebugListener → Text → IO ()
reportLoss cfg listener cause = do
    firstReport ← atomically $ not ⊚ swapTVarBool (dlLossReported listener) True
    when firstReport $ dscOnLoss cfg cause

-- | Admit an accepted connection, or refuse it because the console is
--   full.
--
--   The slot is claimed in ONE transaction with the cap check, and the
--   handle is registered BEFORE the handler is forked, so the count the
--   cap is compared against covers admission through handler cleanup
--   with no window at either end. A refused connection is answered with
--   'refuseClient' and closed here: it never reaches the built-in
--   table, the command queue, or a slot.
admit ∷ DebugServerConfig → DebugListener → TQueue DebugCommand → Socket
      → IO ()
admit cfg listener cmdQueue conn = do
    threadVar ← newEmptyMVar
    doneVar ← newEmptyMVar
    let handle = ClientHandle conn threadVar doneVar
    mSlot ← atomically $ do
        clients ← readTVar (dlClients listener)
        stopping ← readTVar (dlStopping listener)
        if stopping ∨ Map.size clients ≥ dslMaxConnections (dscLimits cfg)
            then return Nothing
            else do
                slot ← readTVar (dlNextClientId listener)
                writeTVar (dlNextClientId listener) (slot + 1)
                writeTVar (dlClients listener) (Map.insert slot handle clients)
                return (Just slot)
    case mSlot of
        Nothing → do
            refuseClient cfg conn
            void ∘ tryAny $ close conn
        Just slot → do
            tid ← forkIO $
                (do self ← myThreadId
                    void $ tryPutMVar threadVar self
                    void ∘ tryAny $ serveClient cfg cmdQueue conn)
                `finally` releaseSlot listener slot handle
            void $ tryPutMVar threadVar tid

-- | Give the slot back and signal the handler's exit. Runs in the
--   handler's own @finally@, so it covers a clean disconnect, a
--   refused-line disconnect, an idle close, and an async kill alike.
releaseSlot ∷ DebugListener → Int → ClientHandle → IO ()
releaseSlot listener slot handle = do
    atomically $ modifyTVar' (dlClients listener) (Map.delete slot)
    void ∘ tryAny ∘ close $ chSocket handle
    void $ tryPutMVar (chDone handle) ()

swapTVarBool ∷ TVar Bool → Bool → STM Bool
swapTVarBool var new = do
    old ← readTVar var
    writeTVar var new
    return old

tryAny ∷ IO α → IO (Either SomeException α)
tryAny = try

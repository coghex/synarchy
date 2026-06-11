{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.DebugServer
    ( DebugCommand(..)
    , startDebugServer
    , pollDebugCommand
    ) where

import UPrelude
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
            -- When port=0 (dump mode), write to stderr to keep stdout
            -- clean for JSON output.
            let readyHandle = if port ≡ 0 then stderr else stdout
            hPutStrLn readyHandle ("READY port=" <> show port)
            hFlush readyHandle
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
                cmdText = T.strip $ TE.decodeUtf8 line
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

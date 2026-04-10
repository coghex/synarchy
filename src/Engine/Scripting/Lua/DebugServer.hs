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
import Control.Exception (SomeException, catch, bracket, finally)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

data DebugCommand = DebugCommand
    { dcCommand  ∷ !Text      -- ^ Lua code to evaluate
    , dcResponse ∷ !(MVar Text)  -- ^ Response channel
    }

-- | Start the debug TCP server on the given port.
--   Returns a TQueue that the Lua thread polls for commands.
startDebugServer ∷ Int → IO (TQueue DebugCommand)
startDebugServer port = do
    cmdQueue ← atomically newTQueue
    _ ← forkIO $ runServer port cmdQueue
    return cmdQueue

pollDebugCommand ∷ TQueue DebugCommand → IO (Maybe DebugCommand)
pollDebugCommand = atomically . tryReadTQueue

runServer ∷ Int → TQueue DebugCommand → IO ()
runServer port cmdQueue = do
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    addrs ← getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
    addr ← case addrs of
        (a:_) → return a
        []    → error "getAddrInfo returned no addresses"
    bracket (openSocket addr) close $ \sock → do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 4
        -- Ready signal on stdout — agents can wait for this line
        -- to know the debug console is accepting connections.
        -- When port=0 (dump mode), write to stderr to keep stdout
        -- clean for JSON output.
        let readyHandle = if port ≡ 0 then stderr else stdout
        hPutStrLn readyHandle ("READY port=" <> show port)
        hFlush readyHandle
        acceptLoop sock cmdQueue

acceptLoop ∷ Socket → TQueue DebugCommand → IO ()
acceptLoop sock cmdQueue = do
    (conn, _) ← accept sock
    _ ← forkIO $ handleClient conn cmdQueue
    acceptLoop sock cmdQueue

handleClient ∷ Socket → TQueue DebugCommand → IO ()
handleClient conn cmdQueue =
    (do sendAll conn "synarchy debug console\n> "
        clientLoop conn cmdQueue BS.empty
    ) `finally` close conn

clientLoop ∷ Socket → TQueue DebugCommand → BS.ByteString → IO ()
clientLoop conn cmdQueue leftover = do
    chunk ← recv conn 4096
    if BS.null chunk
        then return ()  -- client disconnected
        else do
            let buf = leftover <> chunk
            processLines conn cmdQueue buf

processLines ∷ Socket → TQueue DebugCommand → BS.ByteString → IO ()
processLines conn cmdQueue buf =
    case BS8.elemIndex '\n' buf of
        Nothing → clientLoop conn cmdQueue buf  -- no complete line yet
        Just idx →
            let (line, rest) = BS.splitAt idx buf
                remaining = BS.drop 1 rest  -- skip the \n
                cmdText = T.strip $ TE.decodeUtf8 line
            in if T.null cmdText
               then do
                   sendAll conn "> "
                   processLines conn cmdQueue remaining
               else do
                   responseMVar ← newEmptyMVar
                   atomically $ writeTQueue cmdQueue (DebugCommand cmdText responseMVar)
                   -- Wait for Lua thread to process and respond.
                   -- Timeout guards against deadlock: if the Lua thread
                   -- crashes after dequeuing the command but before filling
                   -- the MVar, an unbounded takeMVar would block forever
                   -- (the crash handler only drains the TQueue, not
                   -- already-dequeued commands).
                   mResult ← timeout 30000000 (takeMVar responseMVar)
                   let result = fromMaybe
                         "ERROR: command timed out (Lua thread may have crashed)"
                         mResult
                       resultBytes = TE.encodeUtf8 result
                   sendAll conn resultBytes
                   sendAll conn "\n> "
                   processLines conn cmdQueue remaining

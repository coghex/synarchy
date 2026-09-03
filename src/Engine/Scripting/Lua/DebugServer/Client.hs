{-# LANGUAGE Strict #-}
-- | One debug-console connection, from its banner to its close.
--
--   The loop itself is what it always was — read bytes, split on the
--   first newline, try the built-in table, otherwise queue the line for
--   the Lua thread and wait for an answer. What #2170 added is that
--   every wait and every buffer is now FINITE:
--
--   * the read is bounded by 'dslIdleTimeoutMicros', measured only
--     while no command is in flight;
--   * the assembled line is bounded by 'dslMaxLineBytes', in raw
--     received bytes excluding the newline, checked before any decoding
--     so an over-cap buffer is never retained — in BOTH forms, the
--     terminated line that is one byte too long and the unterminated
--     buffer that has already run past the cap;
--   * the request handed to @recv@ is itself sized from the room left
--     under that cap, so the peak per-connection buffer is the cap plus
--     two bytes rather than the cap plus a whole read.
--
--   The 'dslCommandResponseMicros' wait for the Lua thread is
--   deliberately OUTSIDE the idle bound and its production value is
--   deliberately unchanged: a queued command is a command in flight,
--   and so is a built-in that blocks for minutes
--   ('world.waitForInit').
--
--   What #2282 changed is what the EXPIRY of that wait means. It used
--   to mean nothing at all — the command stayed queued and a later
--   drain ran it against a session whose client had already been told
--   it had not. Now the expiry races the Lua thread's claim through
--   'cancelDebugCommand', and the client reports which side won.
module Engine.Scripting.Lua.DebugServer.Client
    ( serveClient
    , refuseClient
    ) where

import UPrelude
import Engine.Scripting.Lua.DebugServer.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Control.Exception (SomeException, try)
import System.Timeout (timeout)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

-- | Tell a connection the console is full and say nothing else.
--
--   No banner, no prompt, and — the part the cap exists for — no path
--   from here to 'dscBuiltin' or to the command queue. The caller
--   closes the socket; a refused connection never occupies a slot.
refuseClient ∷ DebugServerConfig → Socket → IO ()
refuseClient cfg conn = void ∘ tryIO ∘ sendAll conn ∘ TE.encodeUtf8 $
    connectionRefusedMessage (dslMaxConnections (dscLimits cfg)) <> "\n"

-- | Serve one ADMITTED connection until it disconnects, goes idle, or
--   overruns the line cap. Never closes the socket: the listener owns
--   that, so a shutdown can close it out from under this loop.
serveClient ∷ DebugServerConfig → TQueue DebugCommand → Socket → IO ()
serveClient cfg cmdQueue conn = do
    sendAll conn "synarchy debug console\n> "
    clientLoop cfg cmdQueue conn BS.empty

-- | Wait for more bytes with a command NOT in flight, so this — and
--   only this — is the wait the idle timeout bounds.
clientLoop ∷ DebugServerConfig → TQueue DebugCommand → Socket
           → BS.ByteString → IO ()
clientLoop cfg cmdQueue conn leftover = do
    -- Ask for only the room left under the cap PLUS the two bytes that
    -- make an overrun distinguishable: one byte past the cap, and the
    -- newline that would have terminated it. Without that headroom a
    -- terminated over-cap line could never be SEEN as terminated — the
    -- read would stop one byte short of its newline every time — and
    -- the two rejections below would collapse into one. Away from the
    -- cap this is the historical 4096.
    --
    -- 'leftover' is a newline-free partial line and is never longer
    -- than the cap (the rejection below is what guarantees that), so
    -- the request is always at least two bytes and the buffer peaks at
    -- the cap plus two.
    let cap  = dslMaxLineBytes (dscLimits cfg)
        room = min 4096 (cap + 2 - BS.length leftover)
    mChunk ← timeout (dslIdleTimeoutMicros (dscLimits cfg)) (recv conn room)
    case mChunk of
        Nothing → disconnectWith conn $
            idleTimeoutMessage (dslIdleTimeoutMicros (dscLimits cfg))
        Just chunk
            | BS.null chunk → return ()  -- client disconnected
            | otherwise     → processLines cfg cmdQueue conn (leftover <> chunk)

-- | Consume every COMPLETE line in the buffer, then go back to waiting.
processLines ∷ DebugServerConfig → TQueue DebugCommand → Socket
             → BS.ByteString → IO ()
processLines cfg cmdQueue conn buf =
    case BS8.elemIndex '\n' buf of
        -- No complete line yet. An unterminated buffer past the cap is
        -- refused HERE, without waiting for a newline that may never
        -- come, and the buffer is dropped rather than carried.
        Nothing
            | BS.length buf > cap → tooLong
            | otherwise           → clientLoop cfg cmdQueue conn buf
        -- A complete line. Its length is the index of the newline, so
        -- the cap is on the line's own bytes and the terminator is not
        -- charged against it. Exactly at the cap is valid.
        Just idx
            | idx > cap → tooLong
            | otherwise →
                let (line, rest) = BS.splitAt idx buf
                    remaining = BS.drop 1 rest  -- skip the \n
                    cmdText = T.strip $ TE.decodeUtf8Lenient line
                in if T.null cmdText
                   then do
                       sendAll conn "> "
                       processLines cfg cmdQueue conn remaining
                   else do
                       result ← runCommand cfg cmdQueue cmdText
                       sendAll conn (TE.encodeUtf8 result)
                       sendAll conn "\n> "
                       processLines cfg cmdQueue conn remaining
  where
    cap     = dslMaxLineBytes (dscLimits cfg)
    tooLong = disconnectWith conn (lineTooLongMessage cap)

-- | Answer one command line. Built-ins (the long-blocking waits) run
--   HERE, on the client thread, so they never freeze the Lua thread;
--   everything else goes on the queue and waits
--   'dslCommandResponseMicros' for the Lua thread's reply.
--
--   Built-ins never reach the queue at all, so they have no lifecycle
--   and no cancellation: 'dscBuiltin' answering @Just@ returns before a
--   'DebugCommand' is ever constructed, which is why @engine.quit@,
--   @world.waitForInit@ and @world.waitForChunks@ are untouched by all
--   of this.
--
--   On expiry the command is not simply abandoned (#2282). The wait
--   races the Lua thread's 'claimDebugCommand' through the command's
--   one lifecycle cell, and 'cancelDebugCommand' reports which of three
--   things happened:
--
--   * the cancellation WON — the command was still queued, is now
--     permanently unclaimable, and will be discarded unrun by whichever
--     drain dequeues it. The session is untouched, so the client is
--     told 'commandCancelledMessage' and may safely re-send.
--   * the command was ALREADY CANCELLED by someone else — a load
--     handoff (#763) or a teardown drain got there while this wait was
--     running out. It did not execute either, so the honest reply is
--     that canceller's own, which is what 'cancelDebugCommand' hands
--     back. Reporting an unknown outcome here would be wrong twice
--     over: it would claim the session might have been touched when it
--     was not, and it would discard a rejection the client is entitled
--     to see.
--   * the cancellation LOST to a claim — the Lua thread is running the
--     command. Its answer will be published into a response channel
--     nobody is reading any more, so the client is told
--     'commandUnknownOutcomeMessage' and must NOT re-send.
--
--   In every case the reply is this command's own and the connection
--   moves on: a late answer cannot surface as a stray line on it,
--   because the response channel is per-command and this loop never
--   looks at that one again.
runCommand ∷ DebugServerConfig → TQueue DebugCommand → Text → IO Text
runCommand cfg cmdQueue cmdText = do
    mBuiltin ← dscBuiltin cfg cmdText
    case mBuiltin of
        Just r  → return r
        Nothing → do
            cmd ← newDebugCommand cmdText
            atomically $ writeTQueue cmdQueue cmd
            mResult ← timeout (dslCommandResponseMicros (dscLimits cfg))
                              (takeMVar (dcResponse cmd))
            case mResult of
                Just r  → return r
                Nothing → do
                    cancelled ← cancelDebugCommand cmd commandCancelledMessage
                    return (fromMaybe commandUnknownOutcomeMessage cancelled)

-- | Say why, then stop serving. The send is best-effort — the reason
--   the connection is being dropped is frequently the reason it can no
--   longer be written to.
disconnectWith ∷ Socket → Text → IO ()
disconnectWith conn reason =
    void ∘ tryIO ∘ sendAll conn ∘ TE.encodeUtf8 $ reason <> "\n"

tryIO ∷ IO α → IO (Either SomeException α)
tryIO = try

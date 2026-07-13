{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Input-queue draining and per-event routing (#787): drains
--   'InputEvent's off the input thread's queue, publishes
--   'InputState' after each one, and routes each event by kind to its
--   domain module — 'Engine.Input.Thread.Keyboard',
--   'Engine.Input.Thread.Char', 'Engine.Input.Thread.Mouse',
--   'Engine.Input.Thread.Scroll'. Cursor-move, the synthetic-sequence
--   followup/barrier tokens, and window events are small enough to
--   stay inline here. Split out of 'Engine.Input.Thread' to keep that
--   facade a thin thread-loop entrypoint; both entrypoints below are
--   re-exported there so the public API is unchanged.
module Engine.Input.Thread.Dispatch
  ( processInputs
  , processInput
  ) where

import UPrelude
import qualified Data.Text as T
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Log (logDebug, LogCategory(..))
import Engine.Core.State
import Engine.Input.State (releaseHeldButtons, updateWindowState)
import Engine.Input.Types
import Engine.Scripting.Lua.Types
import qualified Engine.Core.Queue as Q
import Engine.Input.Thread.Keyboard (dispatchKeyEvent)
import Engine.Input.Thread.Char (dispatchCharEvent, flushPendingCharBatch)
import Engine.Input.Thread.Mouse (dispatchMouseEvent)
import Engine.Input.Thread.Scroll (dispatchScrollEvent)

processInputs ∷ EngineEnv → InputState → IO InputState
processInputs env inpSt = do
    mEvent ← Q.tryReadQueue (inputQueue env)
    case mEvent of
        Just event → do
            newState ← processInput env inpSt event
            -- Publish after EVERY event, not once per drained batch: a
            -- synthetic modifier press (#697) must be visible to Lua
            -- pollers (engine.isKeyDown) before the following click/key
            -- event's Lua broadcast is dispatched — this write
            -- happens-before that broadcast's STM enqueue below, so a
            -- callback can never observe a state older than the events
            -- that preceded its own.
            writeIORef (inputStateRef env) newState
            processInputs env newState
        Nothing → do
            -- F4 (#730): flush a still-pending char-aggregate batch at
            -- the tail of every drain too — not just when a following
            -- non-char event interrupts it — so a lone trailing real
            -- character (nothing left in the queue to interrupt it
            -- this tick) still records promptly instead of waiting
            -- indefinitely for the next unrelated event. Must persist
            -- via inputStateRef like the Just branch above, or the
            -- next drain re-reads the unflushed batch and double-pushes it.
            flushed ← flushPendingCharBatch env inpSt
            writeIORef (inputStateRef env) flushed
            return flushed

-- | F4 (#730): flush any pending char-aggregate outcome (see
--   'Engine.Input.Thread.Char.flushPendingCharBatch') before
--   dispatching a NON-char event — the interleaving event (a real
--   key's matching InputKeyEvent, a mouse move, ...) is what ends the
--   run of characters it interrupted. A char event never flushes here
--   (it's the thing BEING accumulated); 'dispatchInput's own
--   InputCharEvent case folds it into the batch instead.
processInput ∷ EngineEnv → InputState → InputEvent → IO InputState
processInput env inpSt0 event = do
    inpSt ← case event of
        InputCharEvent _ → pure inpSt0
        _                → flushPendingCharBatch env inpSt0
    dispatchInput env inpSt event

-- | The top-level per-event router — split out of the pre-#787
--   'Engine.Input.Thread' facade, which owned every one of these
--   branches inline. Each domain's dispatch lives in its own module
--   now; only the small, no-domain-of-their-own event kinds
--   (cursor-move, synthetic followup/barrier, window) stay here.
dispatchInput ∷ EngineEnv → InputState → InputEvent → IO InputState
dispatchInput env inpSt event = case event of
    InputKeyEvent glfwKey keyState mods →
        dispatchKeyEvent env inpSt glfwKey keyState mods
    InputCharEvent c →
        dispatchCharEvent env inpSt c
    InputMouseEvent btn pos state →
        dispatchMouseEvent env inpSt btn pos state
    InputCursorMove x y →
        return $ inpSt { inpMousePos = (x, y) }
    -- Synthetic-sequence fence (#697): hand the carried events (the
    -- tap's modifier releases) to the Lua thread. This message enters
    -- the Lua queue AFTER every broadcast the preceding events of the
    -- same sequence queued above, so the Lua thread re-injects the
    -- releases only once those callbacks have run — a shift-click's
    -- handler polls shift as held, and the release still lands
    -- afterwards (no stuck keys).
    InputFollowup evs → do
        Q.writeQueue (luaQueue env) (LuaInjectFollowup evs)
        return inpSt
    -- Completion marker for synthetic injection (#727): everything
    -- queued ahead of this barrier (FIFO, this is the only consumer)
    -- has already been fully processed, side effects included, by the
    -- time this token lands — see 'inputBarrierRef's haddock. 'max'
    -- (not overwrite) so an out-of-order arrival can't ever move the
    -- watermark backwards, even though allocation+push order already
    -- guarantees monotonic processing order here.
    InputBarrier tok → do
        atomically $ modifyTVar' (inputBarrierRef env) (max tok)
        return inpSt
    InputScrollEvent x y →
        dispatchScrollEvent env inpSt x y
    InputWindowEvent winEv → do
        logger ← readIORef (loggerRef env)
        case winEv of
          WindowResize w h → do
            logDebug logger CatInput $ "Window resize event: width=" <> T.pack (show w) <> ", height=" <> T.pack (show h)
            writeIORef (windowSizeRef env) (w, h)
            Q.writeQueue (luaQueue env) (LuaWindowResize w h)
          FramebufferResize w h → do
            logDebug logger CatInput $ "Framebuffer resize event: width=" <> T.pack (show w) <> ", height=" <> T.pack (show h)
            writeIORef (framebufferSizeRef env) (w, h)
            Q.writeQueue (luaQueue env) (LuaFramebufferResize w h)
          WindowFocus focused → do
            logDebug logger CatInput $ "Window focus event: focused=" <> T.pack (show focused)
            unless focused $ releaseHeldButtons env inpSt
          WindowMinimize minimized → do
            logDebug logger CatInput $ "Window minimize event: minimized=" <> T.pack (show minimized)
            when minimized $ releaseHeldButtons env inpSt
          -- Currently never emitted (the GLFW close request is polled
          -- via shouldClose in the main loop, not routed as an input
          -- event); handled here so the match stays total if it is.
          WindowClose →
            logDebug logger CatInput "Window close event"
        return $ updateWindowState inpSt winEv

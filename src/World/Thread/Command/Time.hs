module World.Thread.Command.Time
    ( handleWorldSetTimeCommand
    , handleWorldSetDateCommand
    , handleWorldSetTimeScaleCommand
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import World.Types
import World.Thread.Helpers (unWorldPageId)

handleWorldSetTimeCommand ∷ WorldSimCapability → LoggerState → WorldPageId → Int → Int → IO ()
handleWorldSetTimeCommand wsc logger pageId hour minute = do
    logDebug logger CatWorld $
        "Setting time for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show hour) <> ":" <> T.pack (show minute)
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            let clampedH = max 0 (min 23 hour)
                clampedM = max 0 (min 59 minute)
            atomicModifyIORef' (wsTimeRef worldState) $ \_ →
                (WorldTime clampedH clampedM, ())
        Nothing →
            logDebug logger CatWorld $
                "World not found for time update: " <> unWorldPageId pageId


handleWorldSetDateCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → Int → Int → Int → IO ()
handleWorldSetDateCommand wsc logger pageId year month day = do
    logDebug logger CatWorld $
        "Setting date for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show year) <> "-"
        <> T.pack (show month) <> "-" <> T.pack (show day)
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            let newDate = WorldDate year month day
            oldDate ← atomicModifyIORef' (wsDateRef worldState) $ \old →
                (newDate, old)
            -- Flora textures derive from the date (#332: annual stage +
            -- derived age), so a date poke must invalidate cached quads
            -- the same way the midnight rollover in tickWorldTime does —
            -- otherwise world.setDate leaves stale flora visuals until
            -- some unrelated invalidation.
            when (oldDate ≢ newDate) $ bumpQuadCacheGen worldState
        Nothing →
            logDebug logger CatWorld $
                "World not found for date update: " <> unWorldPageId pageId

handleWorldSetTimeScaleCommand ∷ WorldSimCapability → LoggerState → WorldPageId → Float → IO ()
handleWorldSetTimeScaleCommand wsc logger pageId scale = do
    logDebug logger CatWorld $
        "Setting time scale for world: " <> unWorldPageId pageId
        <> " to " <> T.pack (show scale) <> " game-min/real-sec"
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- Never store a running scale while the engine is paused. Pause
            -- and time scale are set through different mechanisms (a
            -- synchronous wsEnginePausedRef flip vs this queued command), so a
            -- nonzero scale can be enqueued and then processed AFTER a pause
            -- has taken effect — e.g. a WorldSetTimeScale landing after a
            -- WorldSave, or a stale speed control. Applying it would leave
            -- isPaused() true alongside a nonzero stored scale, the exact
            -- state #42 is about. The player's chosen speed is held by
            -- scripts/pause.lua (prevTimeScale) and re-applied on resume,
            -- where wsEnginePausedRef is already false and this clamp no-ops.
            paused ← readIORef (wsEnginePausedRef wsc)
            writeIORef (wsTimeScaleRef worldState) (if paused then 0 else scale)
        Nothing →
            logDebug logger CatWorld $
                "World not found for time scale update: " <> unWorldPageId pageId

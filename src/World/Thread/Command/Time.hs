module World.Thread.Command.Time
    ( handleWorldSetTimeCommand
    , handleWorldSetDateCommand
    , handleWorldSetTimeScaleCommand
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), withPlayerIntentHeld)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import World.Pause (setPauseResumeScale)
import World.Types

handleWorldSetTimeCommand ∷ WorldSimCapability → LoggerState → WorldPageId → Int → Int → IO ()
handleWorldSetTimeCommand wsc logger pageId hour minute = do
    logDebug logger CatWorld $
        "Setting time for world: " <> unWorldPageId pageId
        <> " to " <> tshow hour <> ":" <> tshow minute
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
        <> " to " <> tshow year <> "-"
        <> tshow month <> "-" <> tshow day
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
        <> " to " <> tshow scale <> " game-min/real-sec"
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
            -- state #42 is about.
            --
            -- #1599: the request is REMEMBERED rather than dropped. It goes
            -- to the page's pause epoch ('World.Pause'), which is what a
            -- resume reinstates — so a speed chosen while paused, or one
            -- whose queued command lost the race with a pause, still takes
            -- effect when the clock starts again instead of silently
            -- becoming 1x. The live clock still reads 0 for the whole pause,
            -- so the invariant above is unchanged.
            -- Under the epoch mutex, so the pause READ and whichever
            -- write it selects are one step: without it a pause epoch
            -- opening or closing concurrently could pair this read with
            -- the other branch's write.
            withPlayerIntentHeld wsc $ \_ → do
                paused ← readIORef (wsEnginePausedRef wsc)
                if paused
                    then do
                        writeIORef (wsTimeScaleRef worldState) 0
                        setPauseResumeScale worldState scale
                    else writeIORef (wsTimeScaleRef worldState) scale
        Nothing →
            logDebug logger CatWorld $
                "World not found for time scale update: " <> unWorldPageId pageId

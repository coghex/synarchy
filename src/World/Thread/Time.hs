{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Time
    ( tickWorldTime
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import World.Types

-- | Advance time for all visible worlds, write sun angle to the shared ref.
tickWorldTime ∷ EngineEnv → Float → IO ()
tickWorldTime env dt = do
    manager ← readIORef (worldManagerRef env)
    -- enginePausedRef is the single source of truth for "is the world clock
    -- advancing". wsTimeScaleRef (the player's chosen speed) is updated
    -- through a separate, asynchronous path (a queued WorldSetTimeScale)
    -- than the synchronous pause flag, so the two can momentarily disagree
    -- — e.g. a WorldSetTimeScale enqueued after a WorldSave can restore a
    -- nonzero scale onto an auto-paused world before this tick reads it.
    -- Gating advancement on the pause flag keeps a paused world's clock
    -- frozen regardless of how wsTimeScaleRef got set (#42). It also covers
    -- every other pause source (notification ccPause, etc.) for free.
    paused ← readIORef (enginePausedRef env)

    forM_ (wmVisible manager) $ \pageId →
        case lookup pageId (wmWorlds manager) of
            Nothing → return ()
            Just worldState → do
                timeScale ← readIORef (wsTimeScaleRef worldState)
                let effScale = if paused then 0 else timeScale
                atomicModifyIORef' (wsTimeRef worldState) $ \wt →
                    (advanceWorldTime effScale dt wt, ())

    case wmVisible manager of
        (pageId:_) → case lookup pageId (wmWorlds manager) of
            Just worldState → do
                wt ← readIORef (wsTimeRef worldState)
                let sunAngle = worldTimeToSunAngle wt
                atomicModifyIORef' (sunAngleRef env) $ \_ → (sunAngle, ())
            Nothing → return ()
        [] → return ()

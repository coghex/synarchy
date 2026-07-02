{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Time
    ( tickWorldTime
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Flora.Harvest (tickFloraHarvests)
import World.Thread.ItemTemp (tickItemTemperatures)

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
                -- Advance time of day AND the calendar date (#332):
                -- midnights crossed carry into wsDateRef, so day-of-year
                -- driven state (flora annual cycle, derived flora age)
                -- actually moves. Both refs are only written from this
                -- thread (tick + queued set-time/date commands), so the
                -- two-ref update can't race another writer.
                wt ← readIORef (wsTimeRef worldState)
                date ← readIORef (wsDateRef worldState)
                paramsM ← readIORef (wsGenParamsRef worldState)
                let calendar = maybe defaultCalendarConfig wgpCalender paramsM
                    (wt', date', daysRolled) =
                        advanceWorldClock calendar effScale dt wt date
                writeIORef (wsTimeRef worldState) wt'
                writeIORef (wsDateRef worldState) date'
                -- Flora regrowth (#94) follows the same clock: timers
                -- count GAME-seconds (timeScale = game-minutes per
                -- real-second, so dtGame = dt·scale·60) and freeze with
                -- the pause flag like everything else on this page.
                -- When a tile finishes regrowing its plant needs its
                -- normal texture back → invalidate the quad cache. A
                -- rolled day does too: flora textures derive from the
                -- date (annual stage + derived age, #332).
                let dtGame = dt * effScale * 60
                when (dtGame > 0) $ do
                    regrew ← atomicModifyIORef' (wsFloraHarvestsRef worldState) $
                        tickFloraHarvests dtGame
                    when (regrew ∨ daysRolled > 0) $
                        bumpQuadCacheGen worldState
                    -- Item temperatures (#344) follow the same
                    -- game-second clock: tracked (hot/cold) items on
                    -- this page relax toward their tile's ambient.
                    tickItemTemperatures env pageId worldState dtGame

    case wmVisible manager of
        (pageId:_) → case lookup pageId (wmWorlds manager) of
            Just worldState → do
                wt ← readIORef (wsTimeRef worldState)
                let sunAngle = worldTimeToSunAngle wt
                atomicModifyIORef' (sunAngleRef env) $ \_ → (sunAngle, ())
            Nothing → return ()
        [] → return ()

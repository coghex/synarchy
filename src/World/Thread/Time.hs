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

    forM_ (wmVisible manager) $ \pageId →
        case lookup pageId (wmWorlds manager) of
            Nothing → return ()
            Just worldState → do
                timeScale ← readIORef (wsTimeScaleRef worldState)
                atomicModifyIORef' (wsTimeRef worldState) $ \wt →
                    (advanceWorldTime timeScale dt wt, ())

    case wmVisible manager of
        (pageId:_) → case lookup pageId (wmWorlds manager) of
            Just worldState → do
                wt ← readIORef (wsTimeRef worldState)
                let sunAngle = worldTimeToSunAngle wt
                atomicModifyIORef' (sunAngleRef env) $ \_ → (sunAngle, ())
            Nothing → return ()
        [] → return ()

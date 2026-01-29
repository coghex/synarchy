module Engine.Loop.Timing
  ( updateFrameTiming
  , getCurTime
  ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Engine.Core.Monad
import Engine.Core.State

-- | Update frame timing and FPS tracking
updateFrameTiming ∷ EngineM ε σ ()
updateFrameTiming = do
    currentTime ← liftIO getCurTime
    tstate ← gets timingState
    
    let lastTime  = lastFrameTime tstate
        accum     = frameTimeAccum tstate
        tFps      = targetFPS tstate
        frameTime = currentTime - lastTime
        targetFrameTime = if tFps > 0.0 then 1.0 / tFps else 1.0 / 60.0
        systemOverhead = 0.0002

    -- Sleep if ahead of schedule
    when (frameTime < targetFrameTime) $ do
        let sleepTime = targetFrameTime - frameTime - systemOverhead
        when (sleepTime > 0) $
            liftIO $ threadDelay $ floor (sleepTime * 1000000)

    -- Update timing state
    actualCurrentTime ← liftIO getCurTime
    let actualFrameTime = actualCurrentTime - lastTime
        newAccum = accum + actualFrameTime

    modify $ \s → s { timingState = (timingState s)
        { lastFrameTime = actualCurrentTime
        , frameTimeAccum = newAccum
        , frameCount = frameCount (timingState s) + 1
        , deltaTime = actualFrameTime
        , currentTime = actualCurrentTime 
        } }

    -- FPS logging and adjustment (every second)
    when (newAccum ≥ 1.0) $ do
        tstate' ← gets timingState
        let currentCount = frameCount tstate'
            fps = if newAccum > 0 then fromIntegral currentCount / newAccum else 0.0
        
        -- Adjust target FPS if needed
        when (fps < 59.0) $
            modify $ \s → s { timingState = (timingState s) {
                                targetFPS = targetFPS (timingState s) + 0.1 } }
        when (fps > 61.0) $
            modify $ \s → s { timingState = (timingState s) {
                                targetFPS = targetFPS (timingState s) - 0.1 } }
        
        -- Reset accumulators
        modify $ \s → s { timingState = (timingState s)
            { frameTimeAccum = 0.0
            , frameCount = 0 
            } }

-- | Get current time as Double
getCurTime ∷ IO Double
getCurTime = realToFrac . utctDayTime <$> getCurrentTime

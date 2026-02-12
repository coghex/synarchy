module Engine.Loop.Timing
  ( updateFrameTiming
  , getCurTime
  ) where

import UPrelude
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.State (get, put)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Text as T
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugSM, logWarnSM, logInfoSM, logInfoM)

-----------------------------------------------------------
-- Frame Timing
-----------------------------------------------------------

updateFrameTiming ∷ EngineM ε σ ()
updateFrameTiming = do
  state ← get
  let timing = timingState state
  env ← ask
  videoConfig ← liftIO $ readIORef (videoConfigRef env)
  let mbTargetFps = if vcVSync videoConfig
                      then Just 60
                      else vcFrameLimit videoConfig
  case mbTargetFps of
    Just targetFps → do
      now ← liftIO getCurTime
      let frameDt = now - lastFrameTime timing
          targetFrameTime = 1.0 / fromIntegral targetFps ∷ Double
          compensatedTarget = targetFrameTime - 0.0012
          sleepTime = compensatedTarget - frameDt
          sleepMicros = floor (sleepTime * 1000000) ∷ Int
      when (sleepMicros > 100) $
        liftIO $ threadDelay sleepMicros
    Nothing → pure ()
  actualNow ← liftIO getCurTime
  let actualDt = actualNow - lastFrameTime timing
      newFrameCount = frameCount timing + 1
      newAccum = frameTimeAccum timing + actualDt
  when (newAccum ≥ 1.0) $ do
    let fps = fromIntegral newFrameCount / newAccum ∷ Double
        avgFrameTime = (newAccum * 1000.0) / fromIntegral newFrameCount
    liftIO $ writeIORef (fpsRef env) fps
    logDebugSM CatGraphics "Performance"
      [("fps", T.pack $ printf "%.1f" fps)
      ,("avg_frame_ms", T.pack $ printf "%.2f" avgFrameTime)]
  put $! state { timingState = timing 
    { currentTime = actualNow
    , deltaTime = actualDt
    , lastFrameTime = actualNow
    , frameCount = if newAccum ≥ 1.0 then 0 else newFrameCount
    , frameTimeAccum = if newAccum ≥ 1.0 then 0.0 else newAccum
    }}

-----------------------------------------------------------
-- Utility Functions
-----------------------------------------------------------

getCurTime ∷ IO Double
getCurTime = realToFrac . utctDayTime ⊚ getCurrentTime

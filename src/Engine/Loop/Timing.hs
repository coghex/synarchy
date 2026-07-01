module Engine.Loop.Timing
  ( updateFrameTiming
  , getCurTime
  ) where

import UPrelude
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Monad.State (put)
import Text.Printf (printf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugSM)

updateFrameTiming ∷ EngineM ε σ ()
updateFrameTiming = do
  state ← get
  let timing = timingState state
  
  -- Get video config (cache the read)
  env ← ask
  videoConfig ← liftIO $ readIORef (videoConfigRef env)
  
  -- Determine target frame time.
  --
  -- VSync ON: do NOT software-cap here. The swapchain is in FIFO present
  -- mode (see Swapchain.chooseSwapPresentMode) — vkQueuePresentKHR / the
  -- next acquire block until the display's vertical blank, so the loop is
  -- already paced to the real refresh rate by the GPU/driver/compositor.
  -- FIFO is the one present mode the Vulkan spec guarantees on every
  -- implementation (incl. MoltenVK/Metal on Apple SoC, where it maps to
  -- Metal's display-synced presentation), so this is fully cross-platform
  -- and auto-adapts to 60 / 120 ProMotion / 144 Hz with no refresh-rate
  -- query. A CPU threadDelay on top would only mis-cap it (the old
  -- hardcoded 60 throttled high-refresh displays).
  --
  -- VSync OFF: MAILBOX/IMMEDIATE do NOT block, so a software cap
  -- (vcFrameLimit) is the legitimate way to bound an otherwise-unlimited
  -- frame rate. Nothing = run unlimited.
  let mbTargetFps = if vcVSync videoConfig
                      then Nothing
                      else vcFrameLimit videoConfig
  
  case mbTargetFps of
    Just targetFps → do
      -- Measure time since last frame
      now ← liftIO getCurTime
      let frameDt = now - lastFrameTime timing
          targetFrameTime = 1.0 / fromIntegral targetFps ∷ Double
          -- COMPENSATION: subtract 1.2ms overhead from target
          compensatedTarget = targetFrameTime - 0.0012
          sleepTime = compensatedTarget - frameDt
          sleepMicros = floor (sleepTime * 1000000) ∷ Int
      
      -- Only sleep if we have meaningful time left
      when (sleepMicros > 100) $  -- Skip tiny sleeps < 0.1ms
        liftIO $ threadDelay sleepMicros
    
    Nothing → pure ()  -- Unlimited FPS
  
  -- Measure actual time after sleep (single call)
  actualNow ← liftIO getCurTime
  let actualDt = actualNow - lastFrameTime timing
      newFrameCount = frameCount timing + 1
      newAccum = frameTimeAccum timing + actualDt
  
  -- Log FPS every second
  when (newAccum ≥ 1.0) $ do
    let fps = fromIntegral newFrameCount / newAccum ∷ Double
        avgFrameTime = (newAccum * 1000.0) / fromIntegral newFrameCount
    liftIO $ writeIORef (fpsRef env) fps
    logDebugSM CatGraphics "Performance"
      [("fps", T.pack $ printf "%.1f" fps)
      ,("avg_frame_ms", T.pack $ printf "%.2f" avgFrameTime)]
  
  -- Update timing state (single write)
  put $! state { timingState = timing
    { currentTime = actualNow
    , deltaTime = actualDt
    , lastFrameTime = actualNow
    , frameCount = if newAccum ≥ 1.0 then 0 else newFrameCount
    , frameTimeAccum = if newAccum ≥ 1.0 then 0.0 else newAccum
    }}

-- | Get current time as Double
getCurTime ∷ IO Double
getCurTime = realToFrac ⊚ getPOSIXTime

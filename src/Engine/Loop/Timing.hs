-- | Render-loop frame timing: the software frame cap and the stored
--   per-frame delta both rendering modes consume.
--
--   Since #2204 every elapsed difference here is a MONOTONIC one passed
--   through the shared boundary in "Engine.Core.Clock": the pre-sleep
--   @frameDt@ that decides how long to sleep and the post-sleep
--   @actualDt@ that becomes 'deltaTime' are sanitised SEPARATELY, and
--   the retained 'lastFrameTime' is always the current RAW sample. The
--   old wall-clock version had two failure shapes worth remembering: a
--   backward clock step made @frameDt@ negative, so the cap slept for
--   the whole step (an hour's correction froze the loop for an hour),
--   and 'lastFrameTime' started at @0.0@, so the very first frame stored
--   epoch-seconds as its delta for the camera and tooltip to integrate.
--   'primeFrameTiming' closes the second: the startup handshake takes a
--   real sample before any frame is measured.
module Engine.Loop.Timing
  ( updateFrameTiming
  , updateFrameTimingWith
  , FrameTimingSeams(..)
  , productionFrameTimingSeams
  , primeFrameTiming
  ) where

import UPrelude
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent (threadDelay)
import Control.Monad.State (put)
import Text.Printf (printf)
import qualified Data.Text as T
import Engine.Core.Clock (monotonicSeconds, sanitiseElapsed)
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugSM)

-- | The two effects frame timing performs, injectable so the headless
--   gate can script a clock jump and observe the sleep it produces
--   instead of waiting through it. Production uses
--   'productionFrameTimingSeams'.
data FrameTimingSeams = FrameTimingSeams
  { frameClock ∷ IO Double
    -- ^ The elapsed-time source, in seconds. 'monotonicSeconds' in
    --   production; a scripted sequence under test.
  , frameSleep ∷ Int → IO ()
    -- ^ The frame-cap sleep, in microseconds. 'threadDelay' in
    --   production; a recorder under test.
  }

productionFrameTimingSeams ∷ FrameTimingSeams
productionFrameTimingSeams = FrameTimingSeams
  { frameClock = monotonicSeconds
  , frameSleep = threadDelay
  }

-- | Take the initial monotonic sample, so the first frame's delta is
--   measured from a real instant rather than from the @0.0@
--   'Engine.Core.Defaults.defaultEngineState' starts with. Run once by
--   'Engine.Loop.Mode.runStartupHandshake' before the first
--   'EngineRunning' tick in every mode (harmless in headless, which
--   never measures a frame).
primeFrameTiming ∷ EngineM σ ()
primeFrameTiming = do
  now ← liftIO monotonicSeconds
  modify $ \s → s { timingState = (timingState s) { lastFrameTime = now } }

updateFrameTiming ∷ EngineM σ ()
updateFrameTiming = updateFrameTimingWith productionFrameTimingSeams

updateFrameTimingWith ∷ FrameTimingSeams → EngineM σ ()
updateFrameTimingWith seams = do
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
      -- Measure time since last frame. Sanitised on its own: a
      -- negative or NaN difference would otherwise become an
      -- arbitrarily long (or arbitrary-Int) sleep, and a bounded one
      -- keeps the sleep at most the compensated target.
      now ← liftIO (frameClock seams)
      let frameDt = sanitiseElapsed (now - lastFrameTime timing)
          targetFrameTime = 1.0 / fromIntegral targetFps ∷ Double
          -- COMPENSATION: subtract 1.2ms overhead from target
          compensatedTarget = targetFrameTime - 0.0012
          sleepTime = compensatedTarget - frameDt
          sleepMicros = floor (sleepTime * 1000000) ∷ Int

      -- Only sleep if we have meaningful time left
      when (sleepMicros > 100) $  -- Skip tiny sleeps < 0.1ms
        liftIO $ frameSleep seams sleepMicros

    Nothing → pure ()  -- Unlimited FPS

  -- Measure actual time after sleep (single call). Sanitised
  -- separately from frameDt above: this is the delta that is stored
  -- and that the FPS window accumulates.
  actualNow ← liftIO (frameClock seams)
  let actualDt = sanitiseElapsed (actualNow - lastFrameTime timing)
      newWindowFrames = fpsWindowFrames timing + 1
      newWindowElapsed = fpsWindowElapsed timing + actualDt

  -- Log FPS every second
  when (newWindowElapsed ≥ 1.0) $ do
    let fps = fromIntegral newWindowFrames / newWindowElapsed ∷ Double
        avgFrameTime = (newWindowElapsed * 1000.0) / fromIntegral newWindowFrames
    liftIO $ writeIORef (fpsRef env) fps
    logDebugSM CatGraphics "Performance"
      [("fps", T.pack $ printf "%.1f" fps)
      ,("avg_frame_ms", T.pack $ printf "%.2f" avgFrameTime)]

  -- Update timing state (single write). The FPS sampling window resets
  -- once it reaches a second; deltaTime/lastFrameTime carry every frame.
  -- lastFrameTime is the RAW sample, whatever actualDt sanitised to:
  -- the excess above the cap is dropped here, not carried into the
  -- next frame's difference.
  put $! state { timingState = timing
    { deltaTime = actualDt
    , lastFrameTime = actualNow
    , fpsWindowFrames = if newWindowElapsed ≥ 1.0 then 0 else newWindowFrames
    , fpsWindowElapsed = if newWindowElapsed ≥ 1.0 then 0.0 else newWindowElapsed
    }}

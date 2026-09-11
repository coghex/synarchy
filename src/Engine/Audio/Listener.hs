-- | Main-loop publication after camera integration. The epoch guard refuses a
-- snapshot whose IO reads straddled whole-session replacement.
module Engine.Audio.Listener (publishCameraListener) where

import UPrelude
import Data.IORef (readIORef)
import Control.Concurrent.STM (atomically)
import Engine.Audio.Transport
import Engine.Audio.Types
import Engine.Core.Capability.Audio
import Engine.Core.Capability.RenderView
import Engine.Core.Capability.WorldSim
import Engine.Core.State (activeWorldPageFrom)
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Loop.Camera (zoomMin)
import World.Grid (worldToGridF, zoomFadeStart, zoomFadeEnd)
import World.Types (WorldState(..), WorldGenParams(..), chunkSize)

publishCameraListener ∷ AudioCapability → RenderViewCapability → WorldSimCapability → IO ()
publishCameraListener audio render world = do
  let transport = acTransport audio
  epoch ← atomically $ readAudioEpoch transport
  active ← activeWorldPageFrom (wsWorldManagerRef world)
  snapshot ← case active of
    Nothing → pure Nothing
    Just (page, state) → do
      camera ← readIORef (rvCameraRef render)
      params ← readIORef (wsGenParamsRef state)
      let (sx, sy) = camPosition camera
          (x, y) = worldToGridF (camFacing camera) sx sy
          circumference = maybe 0 (fromIntegral ∘ (* chunkSize) ∘ wgpWorldSize) params
      pure $ Just $ ListenerSnapshot page x y (fromIntegral $ camZSlice camera)
        (camFacing camera) (camZoom camera) circumference 0 zoomMin zoomFadeStart zoomFadeEnd
  void $ atomically $ publishAudioListenerForEpoch transport epoch snapshot

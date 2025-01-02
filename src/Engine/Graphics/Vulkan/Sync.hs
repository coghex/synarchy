module Engine.Graphics.Vulkan.Sync
  ( createSyncObjects
  , destroySyncObjects
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Vulkan as Vk
import Engine.Core.Monad
import Engine.Core.Resource (allocResource)
import Engine.Core.Error.Exception
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types

-- | Create semaphores and fences for frame synchronization
createSyncObjects ∷ Vk.Device → GraphicsConfig → EngineM ε σ SyncObjects
createSyncObjects device config = do
  let semaphoreInfo = Vk.SemaphoreCreateInfo
        { next = ()
        , flags = Vk.SemaphoreCreateFlags 0
        }
      fenceInfo = Vk.FenceCreateInfo
        { next = ()
        , flags = Vk.FENCE_CREATE_SIGNALED_BIT
        }
      maxFramesInFlight = gcMaxFrames config
  
  imageAvailable ← V.replicateM maxFramesInFlight $
    allocResource (\s0 → Vk.destroySemaphore device s0 Nothing)
      $ Vk.createSemaphore device semaphoreInfo Nothing
  renderFinished ← V.replicateM maxFramesInFlight $
    allocResource (\s0 → Vk.destroySemaphore device s0 Nothing)
      $ Vk.createSemaphore device semaphoreInfo Nothing
  inFlight ← V.replicateM maxFramesInFlight $
    allocResource (\f0 → Vk.destroyFence device f0 Nothing)
      $ Vk.createFence device fenceInfo Nothing
  
  pure SyncObjects
    { imageAvailableSemaphores = imageAvailable
    , renderFinishedSemaphores = renderFinished
    , inFlightFences = inFlight
    }

-- | Cleanup synchronization objects
destroySyncObjects ∷ Vk.Device → SyncObjects → EngineM ε σ ()
destroySyncObjects device syncObjs = do
  V.mapM_ (flip (Vk.destroySemaphore device) Nothing) 
    (imageAvailableSemaphores syncObjs)
  V.mapM_ (flip (Vk.destroySemaphore device) Nothing) 
    (renderFinishedSemaphores syncObjs)
  V.mapM_ (flip (Vk.destroyFence device) Nothing) 
    (inFlightFences syncObjs)

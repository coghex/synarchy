module Engine.Graphics.Vulkan.Sync
  ( createRenderFinishedSemaphores
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Vulkan as Vk
import Engine.Core.Monad
import Engine.Graphics.Vulkan.Types.Cleanup
import Engine.Core.State (GraphicsState(..))

-- | Create one render-finished semaphore per swapchain IMAGE and store
--   the vector in 'renderFinishedSems'. The semaphore that
--   vkQueuePresentKHR waits on must belong to the swapchain image, not
--   the frame-in-flight slot (the image count can differ from
--   gcMaxFrames), so these are (re)created alongside the swapchain.
--   Destruction is registered into the 'vulkanCleanup' record — NOT via
--   allocResource — so the old set is destroyed (under deviceWaitIdle)
--   on swapchain recreation and at shutdown instead of accumulating
--   until program exit.
createRenderFinishedSemaphores ∷ Vk.Device → Int → EngineM σ (V.Vector Vk.Semaphore)
createRenderFinishedSemaphores device count = do
  let semaphoreInfo = Vk.SemaphoreCreateInfo
        { next = ()
        , flags = Vk.SemaphoreCreateFlags 0
        }
  sems ← V.replicateM count $ Vk.createSemaphore device semaphoreInfo Nothing
  modifyGraphicsState $ \gs → gs
    { renderFinishedSems = sems
    , vulkanCleanup = (vulkanCleanup gs)
        { cleanupRenderFinishedSems =
            V.mapM_ (\sem → Vk.destroySemaphore device sem Nothing) sems
        } }
  pure sems

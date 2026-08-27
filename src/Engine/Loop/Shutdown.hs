module Engine.Loop.Shutdown
  ( ShutdownTargets(..)
  , shutdownEngine
  , shutdownEngineWith
  , AtlasRelease(..)
  , atlasReleaseDecision
  , releaseLoadedAtlases
  , releaseLoadedAtlasesWith
  , checkStatus
  ) where

import UPrelude
import Control.Exception (SomeAsyncException, SomeException, catch
                         , displayException, fromException, throwIO)
import Data.IORef (writeIORef, readIORef)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Exit (ExitCode)
import Engine.Asset.Manager (cleanupAssetManager)
import Engine.Core.Log (shutdownLogger, LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Workers (EngineWorkers, preRenderWorkers
                           , postRenderWorkers, stopWorkers)
import Engine.Core.Error.Exception (EngineException(..))
import Engine.Graphics.Window.Types (Window(..))
import Engine.Graphics.Vulkan.Types.Cleanup (runAllCleanups)
import Engine.Graphics.Vulkan.Sampler.Cache (destroySamplerCache)
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Input.Callback (clearGLFWCallbacks)
import Engine.Scene.Types (createBatchManager, SceneManager(..)
                          , SceneDynamicBuffer(..), TextInstanceBuffer(..))
import Vulkan.Core10 (deviceWaitIdle, destroyBuffer, freeMemory)

-- | What 'shutdownEngine' tears down. Named fields rather than a
--   positional argument list (#1036): the old signature took two
--   adjacent @Maybe ThreadState@ and two adjacent @ThreadState@, so
--   swapping either pair compiled and tore the engine down in a
--   different order.
data ShutdownTargets = ShutdownTargets
  { stWindow  ∷ Maybe Window
    -- ^ 'Nothing' for the offscreen mode (#650), which has no GLFW
    --   state to tear down.
  , stWorkers ∷ EngineWorkers
    -- ^ The threads this boot mode started, each absent one named
    --   'Nothing' at the call site.
  }

-- | Shutdown the engine: stop the pre-render workers, tear down Vulkan
--   and GLFW, stop the post-render workers, flush the logger.
--
--   That split /is/ the phase boundary the boot modes have always had —
--   the pre-render phase stops ahead of the render teardown, the
--   post-render phase after it. 'Engine.Core.Workers' owns which worker
--   belongs to which phase and in what order, so this and the
--   fatal-error tail cannot drift apart.
shutdownEngine ∷ ShutdownTargets → EngineM σ ()
shutdownEngine = shutdownEngineWith releaseLoadedAtlases

-- | 'shutdownEngine' with its loaded-atlas release step supplied
--   rather than fixed (#1691).
--
--   Exported for the sake of that step's ORDER, which is the part the
--   real one cannot demonstrate without a GPU: the release sits after
--   the device-idle barrier and before 'runAllCleanups', and every
--   phase below it still runs when the release fails. Production has
--   exactly one caller and it passes 'releaseLoadedAtlases'.
shutdownEngineWith ∷ EngineM σ () → ShutdownTargets → EngineM σ ()
shutdownEngineWith releaseAtlases targets = do
    logInfoM CatSystem "Starting engine shutdown..."

    stopWorkers announceStop (preRenderWorkers (stWorkers targets))

    state ← gets graphicsState
    let device = vulkanDevice state

    -- The pre-render phase has already stopped above; the Vulkan
    -- teardown below precedes only the post-render phase. That is safe
    -- only while Vulkan objects are touched exclusively by this (main)
    -- thread — workers hand pixel data over via IORefs/queues and must
    -- never call into Vulkan, which is what lets the post-render phase
    -- outlive the teardown.

    -- Clear batch manager
    logDebugM CatSystem "Clearing batch manager..."
    modify $ \s → s { sceneManager = (sceneManager s) {
                          smBatchManager = createBatchManager } }
   
    -- Wait for Vulkan device
    logDebugM CatSystem "Waiting for Vulkan device idle..."
    forM_ device $ \dev → liftIO $ deviceWaitIdle dev

    -- Destroy the last transient-texture generations (zoom atlas /
    -- world preview). These use explicit cleanups instead of
    -- exit-time allocResource (they're replaced per world init/load,
    -- see Engine.Scripting.Lua.Message), so the final generation is
    -- ours to free — after the waitIdle above, before the device goes.
    logDebugM CatSystem "Destroying transient textures..."
    forM_ (previewTexture state)   $ \tt → liftIO (ttCleanup tt)
    forM_ (zoomAtlasTexture state) $ \tt → liftIO (ttCleanup tt)

    -- Release every atlas loaded from disk (#1691). These are the
    -- OTHER explicitly-cleaned GPU images: unlike the transient pair
    -- above they are owned by 'apTextureAtlases', and until this call
    -- existed nothing ever ran their stored 'taCleanup' closures, so
    -- their VkImage/VkImageView/VkDeviceMemory were still alive when
    -- the device was destroyed. Here, and not later: the release
    -- invalidates bindless handles through the descriptor set, so it
    -- has to precede the sweep below, and it needs the device idle,
    -- which the wait above has already made it.
    releaseAtlases

    -- run manual cleanup actions
    logDebugM CatSystem "Running Vulkan cleanup actions..."
    liftIO $ runAllCleanups (vulkanCleanup state)

    -- Destroy every cached sampler (texture + font). Device is already
    -- idle (waitIdle above); this frees the handful of shared VkSamplers
    -- the refcounted cache kept alive.
    logDebugM CatSystem "Destroying sampler cache..."
    cacheEnv ← ask
    forM_ device $ \dev →
      liftIO $ destroySamplerCache dev (samplerCacheRef cacheEnv)

    -- Destroy cached text instance buffer
    case device of
        Just dev → do
            V.forM_ (textInstanceBuffers state) $ \case
                Just tib → liftIO $ do
                    destroyBuffer dev (tibBuffer tib) Nothing
                    freeMemory dev (tibMemory tib) Nothing
                Nothing → pure ()

            -- Destroy cached dynamic vertex buffers (one per frame in flight)
            V.forM_ (dynamicVertexBuffers state) $ \case
                Just sdb → liftIO $ do
                    destroyBuffer dev (sdbBuffer sdb) Nothing
                    freeMemory dev (sdbMemory sdb) Nothing
                Nothing → pure ()
        Nothing → logDebugM CatSystem "No Vulkan device found, skipping buffer cleanup"
    
    -- GLFW cleanup (windowed modes only)
    forM_ (stWindow targets) $ \(Window win) → do
        logDebugM CatSystem "Cleaning up GLFW..."
        liftIO $ GLFW.postEmptyEvent
        GLFW.setWindowShouldClose win True
        liftIO $ clearGLFWCallbacks win

    -- Shutdown threads
    stopWorkers announceStop (postRenderWorkers (stWorkers targets))

    -- shut down logger, then mark the engine stopped -- both are
    -- pure core-init capability (issue #889): nothing here needs
    -- graphics/window/thread state, unlike everything above it.
    logDebugM CatSystem "Shutting down logger..."
    env ← ask
    finalizeCoreShutdown (toCoreCapability env)

    logDebugM CatSystem "Engine shutdown complete"
  where
    announceStop name =
      logDebugM CatSystem $ "Shutting down " <> name <> " thread..."

-- | Whether shutdown may release the loaded texture atlases (#1691).
data AtlasRelease
  = ReleaseAtlases
    -- ^ A device and its queues are both live, which is what the
    --   teardown requires before it will touch an atlas.
  | SkipAtlasRelease !Text
    -- ^ One of them is absent, so this boot mode never uploaded an
    --   atlas either. The text names which, for the debug line.
  deriving (Eq, Show)

-- | Decide from presence alone.
--
--   'Engine.Asset.Manager.cleanupAssetManager' fails loudly when
--   either @vulkanDevice@ or @deviceQueues@ is 'Nothing', and it
--   checks both BEFORE looking at whether the pool holds anything — so
--   a mode without them must not reach it at all, or a shutdown that
--   has nothing to release becomes a shutdown that reports an error
--   (requirement 4).
--
--   Generic in both payloads because only the 'Maybe' is consulted:
--   nothing here inspects a device or a queue, and that is what makes
--   the decision drivable in a test with no Vulkan at all.
atlasReleaseDecision ∷ Maybe device → Maybe queues → AtlasRelease
atlasReleaseDecision device queues = case (device, queues) of
  (Just _ , Just _ ) → ReleaseAtlases
  (Nothing, Just _ ) → SkipAtlasRelease "no Vulkan device"
  (Just _ , Nothing) → SkipAtlasRelease "no device queues"
  (Nothing, Nothing) → SkipAtlasRelease "no Vulkan device or queues"

-- | Release every atlas in @apTextureAtlases@ through the alias-safe
--   teardown, or skip it in a boot mode that has no device.
--
--   The release goes through 'cleanupAssetManager' rather than a fresh
--   loop over the stored closures: that entry point invalidates every
--   stable handle naming a slot being freed — canonical owner and
--   cached-atlas alias alike — before any image is destroyed, which is
--   exactly what #1281 fixed and what a private loop would undo.
releaseLoadedAtlases ∷ EngineM σ ()
releaseLoadedAtlases = do
    state ← gets graphicsState
    releaseLoadedAtlasesWith
      (atlasReleaseDecision (vulkanDevice state) (deviceQueues state))
      cleanupAssetManager

-- | 'releaseLoadedAtlases' with its decision and its release action
--   supplied, so both halves are drivable without a Vulkan device.
--
--   Containment is the point (#1691). The release has two independent
--   failure channels and neither may skip the phases after it — the
--   generic cleanup sweep, the sampler cache and cached buffers, the
--   GLFW teardown, the post-render workers, the logger flush, the
--   'EngineStopped' transition. 'cleanupAssetManager' reports through
--   the CPS error channel (@logAndThrowM@), which surfaces here as a
--   'Left'; the Vulkan bindings beneath it throw native IO exceptions,
--   which bypass that channel entirely. Both are reported and
--   contained, so a release that cannot complete costs the atlases and
--   nothing else.
--
--   The two that still propagate are not release failures: an
--   asynchronous exception is the caller killing this thread, and an
--   'ExitCode' is an explicit exit — swallowing either would be the
--   new way to fail to exit that requirement 6 forbids.
releaseLoadedAtlasesWith ∷ AtlasRelease → EngineM' () → EngineM σ ()
releaseLoadedAtlasesWith decision release = case decision of
    SkipAtlasRelease why →
      logDebugM CatSystem $ "Skipping loaded-atlas release: " <> why
    ReleaseAtlases → do
      logDebugM CatSystem "Releasing loaded texture atlases..."
      env ← ask
      outcome ← liftIO $ guardRelease (runEngineM release env pure)
      case outcome of
        Right (Right ()) →
          logDebugM CatSystem "Loaded texture atlases released"
        Right (Left ex)  → report (tshow ex)
        Left  ex         → report (T.pack (displayException ex))
  where
    report why = logWarnM CatSystem $
      "Failed to release the loaded texture atlases, continuing shutdown: "
        <> why
    guardRelease act = (Right ⊚ act) `catch` \(e ∷ SomeException) →
      case (fromException e ∷ Maybe ExitCode) of
        Just _  → throwIO e
        Nothing → case (fromException e ∷ Maybe SomeAsyncException) of
          Just _  → throwIO e
          Nothing → pure (Left e)

-- | Flush the logger and mark the engine lifecycle stopped -- the
--   'core-init'-only tail of 'shutdownEngine', narrowed to
--   'CoreCapability' rather than the full 'EngineEnv' (issue #889).
finalizeCoreShutdown ∷ MonadIO m ⇒ CoreCapability → m ()
finalizeCoreShutdown cap = liftIO $ do
  logger ← readIORef (ccLoggerRef cap)
  shutdownLogger logger
  writeIORef (ccLifecycleRef cap) EngineStopped

-- | Final continuation for 'runEngineM': pass the result through
--   unchanged. Error handling (thread shutdown, logger flush, failure
--   exit code) lives in each main's @Left@ branch — exiting here,
--   inside the CPS continuation, would make those branches
--   unreachable (they were dead code while this called exitFailure,
--   which also lost any buffered log lines on engine errors).
checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus = pure

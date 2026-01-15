{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException, throwIO)
import Control.Concurrent (threadDelay, forkIO)
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (newIORef, readIORef, IORef, writeIORef)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Linear (M44, V3(..), identity, (!*!), perspective, lookAt, translation, ortho)
import System.Environment (setEnv)
import System.Exit ( exitFailure )
import System.FilePath ((</>))
import Engine.Asset.Types
import Engine.Asset.Manager
import Engine.Core.Base
import Engine.Core.Defaults
import Engine.Core.Monad (runEngineM, EngineM', MonadIO(..))
import Engine.Core.Types
import Engine.Core.Thread
import Engine.Core.State
import Engine.Core.Resource
import qualified Engine.Core.Queue as Q
import Engine.Core.Error.Exception
import Engine.Core.Var
import Engine.Graphics.Base
import Engine.Graphics.Types
import Engine.Input.Types
import Engine.Input.Thread
import Engine.Input.Event (handleInputEvents)
import Engine.Input.Callback (setupCallbacks, clearGLFWCallbacks)
import Engine.Graphics.Camera
import Engine.Graphics.Window.GLFW (initializeGLFW, terminateGLFW
                                   , createWindow, destroyWindow, createWindowSurface)
import Engine.Graphics.Window.Types (WindowConfig(..), Window(..))
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture (TexturePoolState(..))
import Engine.Graphics.Vulkan.Instance (createVulkanInstance)
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Device (createVulkanDevice, pickPhysicalDevice)
import Engine.Graphics.Vulkan.Framebuffer (createVulkanFramebuffers)
import Engine.Graphics.Vulkan.Pipeline
import Engine.Graphics.Vulkan.Swapchain (createVulkanSwapchain, querySwapchainSupport
                                        , createSwapchainImageViews)
import Engine.Graphics.Vulkan.Sync (createSyncObjects)
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Vertex
import Engine.Graphics.Vulkan.Types.Vertex (Vertex, Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Scene.Manager
import Engine.Scene.Types
import Engine.Scene.Render
import Engine.Scene.Graph
import Engine.Scene.Base
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.Extensions.VK_KHR_swapchain
import GHC.Stack (HasCallStack)

main ∷ IO ()
main = do
  setEnv "NSLog_Disabled" "YES"
#ifdef DEVELOPMENT
  setEnv "VK_LOADER_DEBUG" "none"
  setEnv "VK_LOADER_MESSAGE_LEVEL" "error"
  setEnv "VK_LOADER_LOG_LEVEL" "0"
#else
#endif

  -- Initialize queues first
  eventQueue ← Q.newQueue
  inputQueue ← Q.newQueue
  logQueue   ← Q.newQueue
  -- initialize engine lifecycle io ref to block threads on engine status
  lifecycleRef ← newIORef EngineStarting
  -- Initialize engine environment and state
  let defaultEngineEnv = EngineEnv
        { engineConfig = defaultEngineConfig
        , eventQueue   = eventQueue
        , inputQueue   = inputQueue
        , logQueue     = logQueue
        , lifecycleRef = lifecycleRef }
  envVar ←   atomically $ newVar defaultEngineEnv
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  stateVar ← atomically $ newVar $ defaultEngineState lf

  -- fork input thread
  inputThreadState ← startInputThread defaultEngineEnv
  
  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        -- initialize GLFW first
        window ← GLFW.createWindow defaultWindowConfig
        modify $ \s → s { graphicsState = (graphicsState s) {
                            glfwWindow = Just window } }

        -- setup input callbacks
        env ← ask
        let glfwWin = case window of Window w → w
        liftIO $ setupCallbacks glfwWin lifecycleRef inputQueue
        
        -- create Vulkan instance
        (vkInstance, _debugMessenger) ← createVulkanInstance defaultGraphicsConfig
        
        -- create surface
        surface ← createWindowSurface window vkInstance
        
        -- select physical device and create logical device
        physicalDevice ← pickPhysicalDevice vkInstance surface
        (device, queues) ← createVulkanDevice vkInstance physicalDevice surface
        modify $ \s → s { graphicsState = (graphicsState s)
                          { vulkanInstance = Just vkInstance
                          , vulkanPDevice = Just physicalDevice
                          , vulkanDevice = Just device
                          , deviceQueues = Just queues
                          } }
        
        -- print some info about the device
        props ← liftIO $ getPhysicalDeviceProperties physicalDevice
        logDebug $ "Selected device: " ⧺ show (deviceName props)
        
        -- test swapchain creation
        swapInfo ← createVulkanSwapchain physicalDevice device
                                         queues surface
        logDebug $ "Swapchain Format: " ⧺ show (siSwapImgFormat swapInfo)
        modify $ \s → s { graphicsState = (graphicsState s) {
                            swapchainInfo = Just swapInfo } }
        let numImages = length $ siSwapImgs swapInfo

        -- test swapchain support query
        support ← querySwapchainSupport physicalDevice surface
        logDebug $ "Available Formats: " ⧺ show (length $ formats support)
        logDebug $ "Available Present Modes: " ⧺ show (presentModes support)

        -- create sync objects
        syncObjects ← createSyncObjects device defaultGraphicsConfig
        modify $ \s → s { graphicsState = (graphicsState s) {
                            syncObjects = Just syncObjects } }

        -- create command pool and buffers
        frameRes ← V.generateM (fromIntegral $ gcMaxFrames defaultGraphicsConfig) $ \_ →
            createFrameResources device queues
        let cmdPool = frCommandPool $ frameRes V.! 0
        modify $ \s → s { graphicsState = (graphicsState s) {
                            frameResources = frameRes
                          , vulkanCmdPool  = Just cmdPool } }

        -- create Descriptor Pool
        let descConfig = DescriptorManagerConfig
              { dmcMaxSets      = fromIntegral $ gcMaxFrames defaultGraphicsConfig * 2
              , dmcUniformCount = fromIntegral $ gcMaxFrames defaultGraphicsConfig
              , dmcSamplerCount = fromIntegral $ gcMaxFrames defaultGraphicsConfig
              }
        descManager ← createVulkanDescriptorManager device descConfig
        logDebug $ "Descriptor Pool Created: " ⧺ show (dmPool descManager)
        modify $ \s → s { graphicsState = (graphicsState s) {
                            descriptorState = Just descManager } }

        -- allocate initial descriptor sets
        descSets ← allocateVulkanDescriptorSets device descManager
                     (fromIntegral $ gcMaxFrames defaultGraphicsConfig)
        let updatedManager = descManager { dmActiveSets = descSets }
        modify $ \s → s { graphicsState = (graphicsState s) {
                            descriptorState = Just updatedManager } }
        logDebug $ "Descriptor Sets Allocated: " ⧺ show (V.length descSets)

        -- creating vertex buffer
        (vBuffer, vBufferMemory) ← createVertexBuffer device physicalDevice
                                     (graphicsQueue queues) cmdPool
        logDebug $ "VertexBuffer: " ⧺ show vBuffer
        modify $ \s → s { graphicsState = (graphicsState s) {
                            vertexBuffer = Just (vBuffer, vBufferMemory) } }
        -- Create descriptor pool and layout first
        descriptorPool ← createTextureDescriptorPool device
        (uniformLayout, texLayout) ← createVulkanDescriptorSetLayout device
        
        logDebug "Created descriptor pool and layout"
        -- initialize textures
        initializeTextures device physicalDevice cmdPool
                           (graphicsQueue queues) descriptorPool texLayout

        -- create uniform buffers
        let modelMatrix = identity
            viewMatrix = lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)
            projMatrix = ortho (-2) 2 (-2) 2 0.1 10
            uboData = UBO modelMatrix viewMatrix projMatrix
            uboSize = fromIntegral $ sizeOf uboData
            numFrames = gcMaxFrames defaultGraphicsConfig
        
        uniformBuffers ← V.generateM (fromIntegral numFrames) $ \_ → do
            (buffer, memory) ← createUniformBuffer device physicalDevice uboSize
            -- Initialize with identity matrices
            let uboData = UBO identity identity identity
            updateUniformBuffer device memory uboData
            pure (buffer, memory)
        
        logDebug $ "UniformBuffers created: " ⧺ show (V.length uniformBuffers)
        modify $ \s → s { graphicsState = (graphicsState s) {
                            uniformBuffers = Just uniformBuffers } }
        
        -- Update descriptor sets for all uniform buffers
        forM_ (zip [0..] (V.toList uniformBuffers)) $ \(i, (buffer, _)) → do
              let bufferInfo = (zero ∷ DescriptorBufferInfo)
                    { buffer = buffer
                    , offset = 0
                    , range  = uboSize
                    }
                  write = zero
                    { dstSet          = descSets V.! i
                    , dstBinding      = 0
                    , dstArrayElement = 0
                    , descriptorCount = 1
                    , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
                    , bufferInfo      = V.singleton bufferInfo
                    }
              updateDescriptorSets device (V.singleton $ SomeStruct write) V.empty

        -- create render pass
        renderPass ← createVulkanRenderPass device (siSwapImgFormat swapInfo)
        logDebug $ "RenderPass: " ⧺ show renderPass
        modify $ \s → s { graphicsState = (graphicsState s) {
                            vulkanRenderPass = Just renderPass } }

        -- create pipeline
        (pipeline, pipelineLayout) ← createVulkanRenderPipeline device renderPass
                                       (siSwapExtent swapInfo) uniformLayout
        logDebug $ "Pipeline: " ⧺ show pipeline
        logDebug $ "PipelineLayout: " ⧺ show pipelineLayout
        let pstate = PipelineState pipeline pipelineLayout renderPass
        modify $ \s → s { graphicsState = (graphicsState s) {
                            pipelineState = Just (pstate) } }

        -- create swapchain image views
        imageViews ← createSwapchainImageViews device swapInfo
        logDebug $ "ImageViews: " ⧺ show (length imageViews)
        
        -- create framebuffers
        framebuffers ← createVulkanFramebuffers device renderPass swapInfo imageViews
        logDebug $ "Framebuffers: " ⧺ show (length framebuffers)
        modify $ \s → s { graphicsState = (graphicsState s) {
                            framebuffers = Just framebuffers } }

        -- Verify all counts match before recording
        state ← gets graphicsState
        let cmdBufferCount = V.length $ frCommandBuffer $ V.head frameRes
            fbCount = V.length framebuffers
            dsCount = maybe 0 (V.length . dmActiveSets) $ descriptorState state
        when (cmdBufferCount /= fbCount || fbCount /= dsCount) $
            throwResourceError (ResourceCountMismatch "engine init: ") $ T.pack $
                "Resource count mismatch: cmdBuffers=" ⧺ show cmdBufferCount ⧺
                " framebuffers=" ⧺ show fbCount ⧺
                " descSets=" ⧺ show dsCount

        -- initialize the scene
        initializeTestScene

        -- record initial command buffers
        state ← gets graphicsState
        let numImages = maybe 0 V.length (vulkanCmdBuffers state)
        forM_ [0..numImages-1] $ \i → do
            recordRenderCommandBuffer 
                (fromJust (vulkanCmdBuffers state) V.! i) 
                (fromIntegral i)
            logDebug $ "Recorded command buffer " ⧺ show i

        mainLoop
        shutdownEngine window inputThreadState
 
  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        liftIO $ shutdownInputThread defaultEngineEnv inputThreadState
    Right _  → pure ()

-- the ever important shutdown function
shutdownEngine ∷ Window → ThreadState → EngineM' EngineEnv ()
shutdownEngine (Window win) ts = do
    logDebug "Engine cleaning up..."

    -- Wait for Vulkan device to idle before resource cleanup
    state ← gets graphicsState
    forM_ (vulkanDevice state) $ \device → liftIO $ deviceWaitIdle device

    -- Wait for input thread to finish
    env ← ask
    inputThreadState ← liftIO $ readIORef (tsRunning ts)
    liftIO $ shutdownInputThread env ts

    -- glfw cleanup
    liftIO $ clearGLFWCallbacks win

    -- cleanup asset manager
    logDebug "cleaning up asset manager..."
    assets ← gets assetPool
    cleanupAssetManager assets

    -- Transition to stopped state
    liftIO $ writeIORef (lifecycleRef env) EngineStopped
    logDebug "Engine shutdown complete."

checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus (Right ()) = pure (Right ())
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure

initializeTextures ∷ Device → PhysicalDevice → CommandPool → Queue
  → DescriptorPool → DescriptorSetLayout
  → EngineM' EngineEnv ()
initializeTextures device physicalDevice cmdPool queue
                   descriptorPool textureLayout = do
 
  -- Update engine state with pool and layout
  let poolState = TexturePoolState descriptorPool textureLayout
  modify $ \s → s { graphicsState = (graphicsState s) {
                      textureState = (poolState, V.empty) } }

  -- Initialize asset manager
  assetPool <- initAssetManager (AssetConfig 100 100 True True)
  modify $ \s → s { assetPool = assetPool }

  -- Create descriptor set for texture array
  let allocInfo = zero 
        { descriptorPool = descriptorPool
        , setLayouts = V.singleton textureLayout
        }
  textureSets ← liftIO $ allocateDescriptorSets device allocInfo
  let textureArrayState = TextureArrayState
        { tasDescriptorPool = descriptorPool
        , tasDescriptorSetLayout = textureLayout
        , tasActiveTextures = V.empty
        , tasDescriptorSet = Just (V.head textureSets)
        }
  -- Update state with texture array
  modify $ \s → s { graphicsState = (graphicsState s) {
      textureArrayStates = Map.singleton "default" textureArrayState
  } }

  -- Load texture using asset manager
  let texturePath1 = "dat/tile01.png"
  let texturePath2 = "dat/tile02.png"
  textureId1 ← loadTextureAtlas (T.pack "tile01") texturePath1 (T.pack "default")
  logDebug $ "Texture 1 loaded: " ⧺ show textureId1
  textureId2 ← loadTextureAtlas (T.pack "tile02") texturePath2 (T.pack "default")
  logDebug $ "Texture 2 loaded: " ⧺ show textureId2
  
  -- Get the loaded texture atlas
  atlas1 ← getTextureAtlas textureId1
  atlas2 ← getTextureAtlas textureId2
  logDebug $ "Atlas 1 status: " ⧺ show (taStatus atlas1)
  logDebug $ "Atlas 2 status: " ⧺ show (taStatus atlas2)
 -- Create descriptor sets for both textures
 
  -- Update descriptor sets for both textures
  case (taInfo atlas1, taInfo atlas2) of
    (Just info1, Just info2) → do
      -- Update first descriptor set
      let imageInfos = V.fromList
            [ zero
              { imageView = tiView info1
              , sampler = tiSampler info1
              , imageLayout = tiLayout info1
              }
            , zero
              { imageView = tiView info2
              , sampler = tiSampler info2
              , imageLayout = tiLayout info2
              }
            ]
          write = zero
            { dstSet = V.head textureSets
            , dstBinding = 0
            , dstArrayElement = 0
            , descriptorCount = 2
            , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            , imageInfo = imageInfos
            }
      
      liftIO $ updateDescriptorSets device 
        (V.singleton $ SomeStruct write)
        V.empty

      -- Create TextureData for both textures
      let textureData1 = TextureData
            { tdImageView = tiView info1
            , tdSampler = tiSampler info1
            , tdMipLevels = amMipLevels (taMetadata atlas1)
            , tdDescriptorSet = textureSets V.! 0
            }
          textureData2 = TextureData
            { tdImageView = tiView info2
            , tdSampler = tiSampler info2
            , tdMipLevels = amMipLevels (taMetadata atlas2)
            , tdDescriptorSet = textureSets V.! 1
            }
      
      -- Update engine state with both textures
      modify $ \s → s { graphicsState = (graphicsState s) {
        textureState = 
          let (poolState', _) = textureState (graphicsState s)
          in (poolState', V.fromList [textureData1, textureData2])
      } }
      
      logDebug "Both textures loaded and descriptor sets updated"
    _ → throwGraphicsError TextureLoadFailed "Texture info not found"

drawFrame ∷ EngineM' EngineEnv ()
drawFrame = do
    state ← gets graphicsState
    -- get window size
    Window win ← case extractWindow state of
        Left err → throwSystemError (GLFWError "drawFrame: ") $ T.pack $
            "No window found: " ⧺ displayException err
        Right w → pure w
    let frameIdx = currentFrame state
    -- update scene for this frame
    updateSceneForRender
    batches ← getCurrentRenderBatches
    -- update uniform buffer
    case (vulkanDevice state, uniformBuffers state) of
        (Just device, Just buffers) → do
            let (_, memory) = buffers V.! fromIntegral frameIdx
            (width, height) ← GLFW.getFramebufferSize win
            
            -- Create matrices using camera
            let camera = camera2D state
                modelMatrix = identity
                viewMatrix = createViewMatrix camera
                projMatrix = createProjectionMatrix camera 
                             (fromIntegral width) 
                             (fromIntegral height)
                uboData = UBO modelMatrix viewMatrix projMatrix
            
            -- Update the uniform buffer
            updateUniformBuffer device memory uboData
        _ → throwGraphicsError VulkanDeviceLost "No device or uniform buffer"
    -- prepare dynamic vertex buffer with scene data
    let totalVertices = V.sum $ V.map (fromIntegral . V.length . rbVertices) batches
    dynamicBuffer ← if totalVertices > 0
        then do
            buffer ← ensureDynamicVertexBuffer totalVertices
            uploadBatchesToBuffer batches buffer
        else do
            ensureDynamicVertexBuffer 6

    -- Validate descriptor sets
    case descriptorState state of
        Nothing → throwGraphicsError DescriptorError "No descriptor manager available"
        Just descManager → when (V.null $ dmActiveSets descManager) $
            throwGraphicsError DescriptorError "No active descriptor sets available"
    -- Validate textures
    let (_, textures) = textureState state
    when (V.length textures < minRequiredTextures) $
      throwGraphicsError TextureLoadFailed $
        "Not enough textures loaded: " <> T.pack (show (V.length textures)) <>
        ", expected at least " <> T.pack (show minRequiredTextures)
    
    let resources = frameResources state V.! fromIntegral frameIdx
        cmdBuffer = V.head $ frCommandBuffer resources
    cmdBuffer ← case safeVectorHead (frCommandBuffer resources) of
        Nothing → throwGraphicsError CommandBufferError
            "No command buffer available for this frame"
        Just cb → pure cb
    resources ← case safeVectorIndex (frameResources state) (fromIntegral frameIdx) of
        Nothing → throwGraphicsError CommandBufferError $
            "Frame index out of bounds: " <> T.pack (show frameIdx)
        Just res → pure res
    -- Wait for previous frame
    device ← case vulkanDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device"
        Just d → pure d
    
    liftIO $ do
      waitForFences device (V.singleton (frInFlight resources))
                           True maxTimeout
      resetFences device (V.singleton (frInFlight resources))
    
    -- Acquire next image
    swapchain ← case swapchainInfo state of
        Nothing → throwGraphicsError SwapchainError "No swapchain"
        Just si → pure $ siSwapchain si
    (acquireResult, imageIndex) ← liftIO $ acquireNextImageKHR device swapchain
                                    maxTimeout (frImageAvailable resources) zero
--    -- reset fence only after acquiring image
--    liftIO $ resetFences device (V.singleton (frInFlight resources))
    
    -- Check if we need to recreate swapchain
    when (acquireResult ≠ SUCCESS && acquireResult ≠ SUBOPTIMAL_KHR) $
        throwGraphicsError SwapchainError $
            T.pack $ "Failed to acquire next image: " ⧺ show acquireResult

    -- reset and record command buffer
    liftIO $ resetCommandBuffer cmdBuffer zero
    --recordRenderCommandBuffer cmdBuffer $ fromIntegral imageIndex
    recordSceneCommandBuffer cmdBuffer (fromIntegral imageIndex)
                             dynamicBuffer batches

    -- submit work
    let waitStages = V.singleton PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        submitInfo = zero 
            { waitSemaphores = V.singleton $ frImageAvailable resources
            , waitDstStageMask = waitStages
            , commandBuffers = V.singleton $ commandBufferHandle cmdBuffer
            , signalSemaphores = V.singleton $ frRenderFinished resources
            }
    
    queues ← case deviceQueues state of
        Nothing → throwGraphicsError VulkanDeviceLost "No queues"
        Just q → pure q
    
    liftIO $ queueSubmit (graphicsQueue queues)
               (V.singleton $ SomeStruct submitInfo)
               $ frInFlight resources

    -- Present
    let presentInfo = zero
            { waitSemaphores = V.singleton $ frRenderFinished resources
            , swapchains = V.singleton swapchain
            , imageIndices = V.singleton imageIndex
            }
    
    presentResult ← liftIO $ queuePresentKHR (presentQueue queues) presentInfo
    case presentResult of
        SUCCESS → pure ()
        SUBOPTIMAL_KHR → pure ()
        err → throwGraphicsError SwapchainError $
                T.pack $ "Failed to present image: " ⧺ show err
    
    -- Update frame index
    modify $ \s → s { graphicsState = (graphicsState s) {
                        currentFrame = (currentFrame (graphicsState s) + 1)
                                         `mod` fromIntegral
                                         (gcMaxFrames defaultGraphicsConfig) } }

getCurTime ∷ IO Double
getCurTime = do
    now ← getCurrentTime
    return $ realToFrac $ utctDayTime now

mainLoop ∷ EngineM' EngineEnv ()
mainLoop = do
    env ← ask
    state  ← gets graphicsState
    tstate ← gets timingState
    lifecycle ← liftIO $ readIORef (lifecycleRef env)

    case lifecycle of
        -- block premature exits
        EngineStarting → do
          logDebug "Engine starting..."
          liftIO $ threadDelay 100000
          -- clear the input queue before hitting the main loop again
          liftIO $ Q.flushQueue (inputQueue env)
          liftIO $ writeIORef (lifecycleRef env) EngineRunning
          mainLoop
        -- regular operation
        EngineRunning → do
          window ← case glfwWindow state of
              Nothing → throwSystemError (GLFWError "main loop: ") "No window"
              Just w → pure w
              
          let glfwWindow = case window of
                  Window w → w

          currentTime ← liftIO getCurTime
          tstate ← gets timingState
          let lastTime  = lastFrameTime tstate
              accum     = frameTimeAccum tstate
              tFps      = targetFPS tstate

          let frameTime = currentTime - lastTime
              targetFrameTime = if tFps > 0.0 then 1.0 / tFps else 1.0 / 60.0
              -- Add a small adjustment for system overhead
              systemOverhead = 0.0002  -- 0.2ms adjustment

          -- Sleep if we're ahead of schedule, accounting for overhead
          when (frameTime < targetFrameTime) $ do
              let sleepTime = targetFrameTime - frameTime - systemOverhead
              when (sleepTime > 0) $
                  liftIO $ threadDelay $ floor (sleepTime * 1000000)

          -- Get time after potential sleep
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

          when (newAccum ≥ 1.0) $ do
              tstate ← gets timingState
              let currentCount = frameCount tstate
                  fps = if newAccum > 0 then fromIntegral currentCount / newAccum else 0.0
              logDebug $ "FPS: " ⧺ show fps
              -- Adjust timing if FPS is consistently off
              when (fps < 59.0) $
                  modify $ \s → s { timingState = (timingState s) {
                                      targetFPS = targetFPS (timingState s) + 0.1 } }
              when (fps > 61.0) $
                  modify $ \s → s { timingState = (timingState s) {
                                      targetFPS = targetFPS (timingState s) - 0.1 } }
              modify $ \s → s { timingState = (timingState s)
                  { frameTimeAccum = 0.0
                  , frameCount = 0 
                  } }
          GLFW.pollEvents
          handleInputEvents
          shouldClose ← GLFW.windowShouldClose glfwWindow
          env ← ask
          lifecycle ← liftIO $ readIORef (lifecycleRef env)
          if shouldClose || lifecycle ≢ EngineRunning
              then do
                  liftIO $ Q.writeQueue (logQueue env) "Engine shutting down..."
                  -- Just wait for device to idle
                  forM_ (vulkanDevice state) $ \device → 
                      unless (lifecycle ≡ EngineStopped) $
                        liftIO $ deviceWaitIdle device
              else unless (lifecycle ≡ CleaningUp) $ do
                  drawFrame
                  mainLoop
        CleaningUp → logDebug "Engine is cleaning up"
        EngineStopped → logDebug "Engine has stopped"

-- Add after initializeTextures function
initializeTestScene ∷ EngineM' EngineEnv ()
initializeTestScene = do
    -- Get loaded texture asset IDs
    assetPool ← gets assetPool
    let textureIds = Map.keys (apTextureAtlases assetPool)
    
    case textureIds of
        [] → logDebug "No textures loaded for test scene"
        (tex1:tex2:_) → do
            -- Create test scene
            let camera = defaultCamera
                testSceneId = "test"
            
            sceneMgr ← gets sceneManager
            let sceneWithTest = createScene testSceneId camera sceneMgr
                activeScene = setActiveScene testSceneId sceneWithTest
            
            -- Create first test object
            let node1 = (createSceneNode SpriteObject)
                    { nodeTransform = defaultTransform { position = (-1.0, 0.0) }
                    , nodeTexture = Just tex1
                    , nodeSize = (1.0, 1.0)
                    , nodeColor = Vec4 1.0 1.0 1.0 1.0
                    }
            
            -- Create second test object  
            let node2 = (createSceneNode SpriteObject)
                    { nodeTransform = defaultTransform { position = (1.0, 0.0) }
                    , nodeTexture = Just tex2
                    , nodeSize = (1.0, 1.0)
                    , nodeColor = Vec4 1.0 1.0 1.0 1.0
                    }
            
            -- Add objects to scene
            case addObjectToScene testSceneId node1 activeScene of
                Nothing → logDebug "Failed to add first object to scene"
                Just (obj1Id, mgr1) → case addObjectToScene testSceneId node2 mgr1 of
                    Nothing → logDebug "Failed to add second object to scene"
                    Just (obj2Id, finalMgr) → do
                        modify $ \s → s { sceneManager = finalMgr }
                        logDebug $ "Test scene created with objects: " ⧺ show obj1Id ⧺ ", " ⧺ show obj2Id
        (tex1:_) → do
            -- Single texture case
            let camera = defaultCamera
                testSceneId = "test"
            
            sceneMgr ← gets sceneManager
            let sceneWithTest = createScene testSceneId camera sceneMgr
                activeScene = setActiveScene testSceneId sceneWithTest
            
            let node1 = (createSceneNode SpriteObject)
                    { nodeTransform = defaultTransform { position = (0.0, 0.0) }
                    , nodeTexture = Just tex1
                    , nodeSize = (1.0, 1.0)
                    , nodeColor = Vec4 1.0 1.0 1.0 1.0
                    }
            
            case addObjectToScene testSceneId node1 activeScene of
                Nothing → logDebug "Failed to add object to scene"
                Just (objId, finalMgr) → do
                    modify $ \s → s { sceneManager = finalMgr }
                    logDebug $ "Test scene created with object: " ⧺ show objId

extractWindow ∷ HasCallStack ⇒ GraphicsState → Either EngineException Window
extractWindow state =
    case glfwWindow state of
        Nothing → Left $ EngineException
                    (ExSystem (GLFWError "drawFrame"))
                    "No GLFW window initialized"
                    mkErrorContext
        Just win → Right win

-- | Safe vector access that checks bounds
safeVectorIndex ∷ V.Vector a → Int → Maybe a
safeVectorIndex vec idx
  | idx >= 0 && idx < V.length vec = Just (vec V.! idx)
  | otherwise = Nothing

-- | Safe vector head that returns Maybe
safeVectorHead ∷ V.Vector a → Maybe a
safeVectorHead vec
  | V.null vec = Nothing
  | otherwise = Just (V.head vec)


minRequiredTextures ∷ Int
minRequiredTextures = 2

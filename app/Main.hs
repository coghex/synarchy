{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException, throwIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, when, forM_, unless)
import qualified Control.Monad.Logger.CallStack as Logger
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, get, gets)
import Data.Bits ((.|.))
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Foreign.Storable (sizeOf, poke)
import Foreign.Ptr (castPtr)
import Linear (M44, V3(..), identity, (!*!), perspective, lookAt, translation, ortho)
import System.Environment (setEnv)
import System.Exit ( exitFailure )
import System.FilePath ((</>))
import Engine.Core.Monad (runEngineM, EngineM')
import Engine.Core.Types
import Engine.Core.Resource
import qualified Engine.Core.Queue as Q
import Engine.Core.Error.Exception
import Engine.Concurrent.Var
import Engine.Graphics.Types
import Engine.Input.Keyboard
import Engine.Input.Types
import Engine.Input.Thread (shutdownInputThread, startInputThread)
import Engine.Input.Event (handleInputEvents)
import Engine.Input.Callback (setupCallbacks)
import Engine.Graphics.Window.GLFW (initializeGLFW, terminateGLFW
                                   , createWindow, destroyWindow, createWindowSurface)
import Engine.Graphics.Window.Types (WindowConfig(..), Window(..))
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
import Engine.Graphics.Vulkan.Types.Texture
import qualified Engine.Graphics.Window.GLFW as GLFW
import Vulkan.CStruct.Extends
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.Extensions.VK_KHR_swapchain
import Control.Monad.IO.Class (liftIO)

defaultEngineConfig ∷ EngineConfig
defaultEngineConfig = EngineConfig 
  { windowWidth  = 800
  , windowHeight = 600
  , enableVSync  = True
#ifdef DEVELOPMENT
  , enableDebug  = True
#else
  , enableDebug  = False
#endif
  }

defaultGraphicsConfig ∷ GraphicsConfig
defaultGraphicsConfig = GraphicsConfig 
  { gcAppName   = T.pack "Vulkan Device Test"
#ifdef DEVELOPMENT
  , gcDebugMode = True
#else
  , gcDebugMode = False
#endif
  , gcWidth     = 800
  , gcHeight    = 600
  , gcMaxFrames = 2
  }

defaultWindowConfig ∷ WindowConfig
defaultWindowConfig = WindowConfig 
  { wcWidth     = 800
  , wcHeight    = 600
  , wcTitle     = T.pack "Vulkan Test"
  , wcResizable = True
  }

defaultEngineState ∷ LoggingFunc → EngineState
defaultEngineState lf = EngineState
  { frameCount       = 0
  , engineRunning    = True
  , currentTime      = 0.0
  , deltaTime        = 0.0
  , frameTimeAccum   = 0.0
  , lastFrameTime    = 0.0
  , targetFPS        = 60.0
  , inputState       = defaultInputState
  , logFunc          = lf
  , vulkanInstance   = Nothing
  , vulkanDevice     = Nothing
  , vulkanCmdPool    = Nothing
  , vulkanCmdBuffers = Nothing
  , vulkanRenderPass = Nothing
  , textureState     = (TexturePoolState zero zero, V.empty)
  , descriptorState  = Nothing
  , pipelineState    = Nothing
  , currentFrame     = 0
  , framebuffers     = Nothing
  , swapchainInfo    = Nothing
  , syncObjects      = Nothing
  , vertexBuffer     = Nothing
  , uniformBuffers   = Nothing
  }

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
  -- Initialize engine environment and state
  let defaultEngineEnv = EngineEnv
        { engineConfig = defaultEngineConfig
        , eventQueue   = eventQueue
        , inputQueue   = inputQueue
        , logQueue     = logQueue }
  envVar ←   atomically $ newVar defaultEngineEnv
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  stateVar ← atomically $ newVar $ defaultEngineState lf

  -- fork input thread
  inputThreadState ← startInputThread defaultEngineEnv
  
  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        -- initialize GLFW first
        window ← GLFW.createWindow defaultWindowConfig
        modify $ \s → s { glfwWindow = Just window }

        -- setup input callbacks
        env ← ask
        let glfwWin = case window of Window w → w
        liftIO $ setupCallbacks glfwWin inputQueue
        
        -- create Vulkan instance
        (vkInstance, _debugMessenger) ← createVulkanInstance defaultGraphicsConfig
        
        -- create surface
        surface ← createWindowSurface window vkInstance
        
        -- select physical device and create logical device
        physicalDevice ← pickPhysicalDevice vkInstance surface
        (device, queues) ← createVulkanDevice vkInstance physicalDevice surface
        modify $ \s → s { vulkanInstance = Just vkInstance
                        , vulkanDevice = Just device
                        , deviceQueues = Just queues
                        }
        
        -- print some info about the device
        props ← liftIO $ getPhysicalDeviceProperties physicalDevice
        logDebug $ "Selected device: " ⧺ show (deviceName props)
        
        -- test swapchain creation
        swapInfo ← createVulkanSwapchain physicalDevice device
                                         queues surface
        logDebug $ "Swapchain Format: " ⧺ show (siSwapImgFormat swapInfo)
        modify $ \s → s { swapchainInfo = Just swapInfo }
        let numImages = length $ siSwapImgs swapInfo

        -- test swapchain support query
        support ← querySwapchainSupport physicalDevice surface
        logDebug $ "Available Formats: " ⧺ show (length $ formats support)
        logDebug $ "Available Present Modes: " ⧺ show (presentModes support)

        -- create sync objects
        syncObjects ← createSyncObjects device defaultGraphicsConfig
        modify $ \s → s { syncObjects = Just syncObjects }

        -- create command pool and buffers
        cmdCollection ← createVulkanCommandCollection device queues
                          (fromIntegral $ length $ imageAvailableSemaphores syncObjects)
        logDebug $ "CommandPool: " ⧺ show (length $ vccCommandBuffers cmdCollection)
        modify $ \s → s { vulkanCmdPool = Just (vccCommandPool cmdCollection)
                        , vulkanCmdBuffers = Just (vccCommandBuffers cmdCollection)
                        }

        -- create Descriptor Pool
        let descConfig = DescriptorManagerConfig
              { dmcMaxSets      = fromIntegral $ gcMaxFrames defaultGraphicsConfig * 2
              , dmcUniformCount = fromIntegral $ gcMaxFrames defaultGraphicsConfig
              , dmcSamplerCount = fromIntegral $ gcMaxFrames defaultGraphicsConfig
              }
        descManager ← createVulkanDescriptorManager device descConfig
        logDebug $ "Descriptor Pool Created: " ⧺ show (dmPool descManager)
        modify $ \s → s { descriptorState = Just descManager }

        -- allocate initial descriptor sets
        descSets ← allocateVulkanDescriptorSets device descManager
                     (fromIntegral $ gcMaxFrames defaultGraphicsConfig)
        let updatedManager = descManager { dmActiveSets = descSets }
        modify $ \s → s { descriptorState = Just updatedManager }
        logDebug $ "Descriptor Sets Allocated: " ⧺ show (V.length descSets)

        -- creating vertex buffer
        (vBuffer, vBufferMemory) ← createVertexBuffer device physicalDevice
                                     (graphicsQueue queues)
                                     (vccCommandPool cmdCollection)
        logDebug $ "VertexBuffer: " ⧺ show vBuffer
        modify $ \s → s { vertexBuffer = Just (vBuffer, vBufferMemory) }

        -- create descriptor set layout
        descSetLayout ← createVulkanDescriptorSetLayout device
        logDebug $ "DescriptorSetLayout: " ⧺ show descSetLayout

        -- initialize textures
        initializeTextures device physicalDevice
                           (vccCommandPool cmdCollection)
                           (graphicsQueue queues)

        -- create uniform buffers
        let modelMatrix = identity
            viewMatrix = lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)
            projMatrix = ortho (-2) 2 (-2) 2 0.1 10
            uboData = UBO modelMatrix viewMatrix projMatrix
            uboSize = fromIntegral $ sizeOf uboData
        (uboBuffer, uboMemory) ← createUniformBuffer device physicalDevice uboSize
        logDebug $ "UniformBuffer: " ⧺ show uboBuffer
        updateUniformBuffer device uboMemory uboData
        modify $ \s → s { uniformBuffers = Just (uboBuffer, uboMemory) }
        let bufferInfo = (zero ∷ DescriptorBufferInfo)
                { buffer = uboBuffer
                , offset = 0
                , range  = uboSize
                }
            write = zero
                { dstSet          = V.head descSets
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
        modify $ \s → s { vulkanRenderPass = Just renderPass }

        -- create pipeline
        (pipeline, pipelineLayout) ← createVulkanRenderPipeline device renderPass
                                       (siSwapExtent swapInfo) descSetLayout
        logDebug $ "Pipeline: " ⧺ show pipeline
        logDebug $ "PipelineLayout: " ⧺ show pipelineLayout
        let pstate = PipelineState pipeline pipelineLayout renderPass
        modify $ \s → s { pipelineState = Just (pstate) }

        -- create swapchain image views
        imageViews ← createSwapchainImageViews device swapInfo
        logDebug $ "ImageViews: " ⧺ show (length imageViews)
        
        -- create framebuffers
        framebuffers ← createVulkanFramebuffers device renderPass swapInfo imageViews
        logDebug $ "Framebuffers: " ⧺ show (length framebuffers)
        modify $ \s → s { framebuffers = Just framebuffers }

        -- verify all counts match before recording
        when (V.length (vccCommandBuffers cmdCollection) /= V.length framebuffers ||
              V.length framebuffers /= V.length descSets) $
          throwEngineException $ EngineException ExGraphics
                                   "Resource count mismatch"

        -- record initial command buffers
        state ← get
        let numImages = maybe 0 V.length (vulkanCmdBuffers state)
        forM_ [0..numImages-1] $ \i → do
            recordRenderCommandBuffer 
                (fromJust (vulkanCmdBuffers state) V.! i) 
                (fromIntegral i)
            logDebug $ "Recorded command buffer " ⧺ show i
        mainLoop
        liftIO $ Q.writeQueue logQueue "Engine shutting down..."
        liftIO $ shutdownInputThread env inputThreadState
  
  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        liftIO $ shutdownInputThread defaultEngineEnv inputThreadState
    Right _  → pure ()

checkStatus ∷ Either EngineException () → IO (Either EngineException ())
checkStatus (Right ()) = pure (Right ())
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure

initializeTextures ∷ Device → PhysicalDevice → CommandPool → Queue → EngineM' EngineEnv ()
initializeTextures device physicalDevice cmdPool queue = do
  -- Create descriptor pool and layout first
  descriptorPool ← createTextureDescriptorPool device
  descriptorSetLayout ← createTextureDescriptorSetLayout device
  
  logDebug "Created descriptor pool and layout"
  
  -- Update engine state with pool and layout
  let poolState = TexturePoolState descriptorPool descriptorSetLayout
  modify $ \s → s { textureState = (poolState, V.empty) }
  
  -- Load texture with proper error handling
  let texturePath = "dat/tile01.png"
  textureData ← createTextureWithDescriptor device physicalDevice cmdPool queue texturePath
  
  logDebug "Created texture with descriptor"
  
  -- Update engine state with the new texture
  modify $ \s → s { textureState = 
    let (poolState', _) = textureState s
    in (poolState', V.singleton textureData)
  }
  
  logDebug $ "Texture loaded: " ⧺ show textureData

drawFrame ∷ EngineM' EngineEnv ()
drawFrame = do
    state ← get
    
    -- Get current frame index
    let currentFrameIdx = currentFrame state
    
    -- Get sync objects for current frame
    syncObjs ← case syncObjects state of
        Nothing → throwEngineException $ EngineException ExGraphics "No sync objects"
        Just s → pure s
    
    let inFlightFence = inFlightFences syncObjs V.! fromIntegral currentFrameIdx
        imageAvailableSemaphore = imageAvailableSemaphores syncObjs V.! fromIntegral currentFrameIdx
        renderFinishedSemaphore = renderFinishedSemaphores syncObjs V.! fromIntegral currentFrameIdx

    -- Wait for previous frame
    device ← case vulkanDevice state of
        Nothing → throwEngineException $ EngineException ExGraphics "No device"
        Just d → pure d
    
    liftIO $ waitForFences device (V.singleton inFlightFence) True maxTimeout
    liftIO $ resetFences device (V.singleton inFlightFence)
    
    -- Acquire next image
    swapchain ← case swapchainInfo state of
        Nothing → throwEngineException $ EngineException ExGraphics "No swapchain"
        Just si → pure $ siSwapchain si
        
    (acquireResult, imageIndex) ← liftIO $ acquireNextImageKHR 
        device 
        swapchain
        maxTimeout 
        imageAvailableSemaphore
        zero
    
    when (acquireResult ≠ SUCCESS && acquireResult ≠ SUBOPTIMAL_KHR) $
        throwEngineException $ EngineException ExGraphics $
            T.pack $ "Failed to acquire next image: " ⧺ show acquireResult

    -- Get command buffer
    cmdBuffers ← case vulkanCmdBuffers state of
        Nothing → throwEngineException $ EngineException ExGraphics "No command buffers"
        Just cb → pure cb
        
    let cmdBuffer = cmdBuffers V.! fromIntegral imageIndex
        waitStages = V.singleton PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        submitInfo = zero 
            { waitSemaphores = V.singleton imageAvailableSemaphore
            , waitDstStageMask = waitStages
            , commandBuffers = V.singleton (commandBufferHandle cmdBuffer)
            , signalSemaphores = V.singleton renderFinishedSemaphore
            }
    liftIO $ resetCommandBuffer cmdBuffer zero
    recordRenderCommandBuffer cmdBuffer $ fromIntegral imageIndex
    
    queues ← case deviceQueues state of
        Nothing → throwEngineException $ EngineException ExGraphics "No queues"
        Just q → pure q
    
    liftIO $ queueSubmit (graphicsQueue queues) (V.singleton $ SomeStruct submitInfo) inFlightFence

    -- Present
    let presentInfo = zero
            { waitSemaphores = V.singleton renderFinishedSemaphore
            , swapchains = V.singleton swapchain
            , imageIndices = V.singleton imageIndex
            }
    
    presentResult ← liftIO $ queuePresentKHR (presentQueue queues) presentInfo
    case presentResult of
        SUCCESS → pure ()
        SUBOPTIMAL_KHR → pure ()
        err → throwEngineException $ EngineException ExGraphics $
                T.pack $ "Failed to present image: " ⧺ show err
    
    -- Update frame index
    modify $ \s → s { currentFrame = (currentFrame s + 1) `mod` 2 }

getCurTime ∷ IO Double
getCurTime = do
    now ← getCurrentTime
    return $ realToFrac $ utctDayTime now

mainLoop ∷ EngineM' EngineEnv ()
mainLoop = do
    state ← get
    running ← gets engineRunning
    
    unless (not running) $ do
        window ← case glfwWindow state of
            Nothing → throwEngineException $ EngineException ExGraphics "No window"
            Just w → pure w
            
        let glfwWindow = case window of
                Window w → w

        currentTime ← liftIO getCurTime
        lastTime ← gets lastFrameTime
        accum ← gets frameTimeAccum
        targetFps ← gets targetFPS

        let frameTime = currentTime - lastTime
            targetFrameTime = 1.0 / targetFps
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

        modify $ \s → s 
            { lastFrameTime = actualCurrentTime
            , frameTimeAccum = newAccum
            , frameCount = frameCount s + 1
            , deltaTime = actualFrameTime
            , currentTime = actualCurrentTime 
            }

        when (newAccum ≥ 1.0) $ do
            currentCount ← gets frameCount
            let fps = fromIntegral currentCount / newAccum
            logDebug $ "FPS: " ⧺ show fps
            -- Adjust timing if FPS is consistently off
            when (fps < 59.0) $
                modify $ \s → s { targetFPS = targetFPS s + 0.1 }
            when (fps > 61.0) $
                modify $ \s → s { targetFPS = targetFPS s - 0.1 }
            modify $ \s → s 
                { frameTimeAccum = 0.0
                , frameCount = 0 
                }
        GLFW.pollEvents
        handleInputEvents
        shouldClose ← GLFW.windowShouldClose glfwWindow
        if shouldClose || not running
            then do
                env ← ask
                liftIO $ Q.writeQueue (logQueue env) "Engine shutting down..."
                -- Just wait for device to idle
                forM_ (vulkanDevice state) $ \device → 
                    liftIO $ deviceWaitIdle device
            else do
                drawFrame
                mainLoop



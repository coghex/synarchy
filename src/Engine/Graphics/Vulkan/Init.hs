{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
module Engine.Graphics.Vulkan.Init
  ( initializeVulkan
  , initializeTextures
  , createUniformBuffersForFrames
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFWRaw
import Foreign.Storable (sizeOf)
import Linear (M44, identity)
import Engine.Asset.Handle
import Engine.Core.Defaults
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Graphics.Base
import Engine.Graphics.Camera
import Engine.Graphics.Types
import Engine.Graphics.Font.Draw (createFontPipeline, createFontQuadBuffer)
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Graphics.Window.GLFW as GLFW
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Buffer
import Engine.Graphics.Vulkan.Command
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Device (createVulkanDevice, pickPhysicalDevice)
import Engine.Graphics.Vulkan.Framebuffer (createVulkanFramebuffers)
import Engine.Graphics.Vulkan.Instance (createVulkanInstance)
import Engine.Graphics.Vulkan.Pipeline
import Engine.Graphics.Vulkan.Swapchain
import Engine.Graphics.Vulkan.Sync (createSyncObjects)
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Vertex
import Engine.Scene.Manager (createScene, setActiveScene)
import Engine.Scene.Types
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends (SomeStruct(..))

-- | Initialize all Vulkan resources
-- Returns the command pool for use by other systems
-- Using general EngineM ε σ so helper functions with different return types work
initializeVulkan ∷ Window → EngineM ε σ CommandPool
initializeVulkan window = do
  let Window glfwWin = window
  
  -- Create Vulkan instance
  (vkInstance, _debugMessenger) ← createVulkanInstance defaultGraphicsConfig
  
  -- Create surface
  surface ← GLFW.createWindowSurface window vkInstance
  
  -- Select physical device and create logical device
  physicalDevice ← pickPhysicalDevice vkInstance surface
  (device, queues) ← createVulkanDevice vkInstance physicalDevice surface
  
  modify $ \s → s { graphicsState = (graphicsState s)
                    { vulkanInstance = Just vkInstance
                    , vulkanPDevice  = Just physicalDevice
                    , vulkanDevice   = Just device
                    , deviceQueues   = Just queues
                    } }
  
  -- Log device info
  props ← liftIO $ getPhysicalDeviceProperties physicalDevice
  logDebug $ "Selected device: " ⧺ show (deviceName props)
  
  -- Create swapchain
  swapInfo ← createVulkanSwapchain physicalDevice device queues surface
  logDebug $ "Swapchain Format: " ⧺ show (siSwapImgFormat swapInfo)
  modify $ \s → s { graphicsState = (graphicsState s) {
                      swapchainInfo = Just swapInfo } }
  
  -- Log swapchain support
  support ← querySwapchainSupport physicalDevice surface
  logDebug $ "Available Formats: " ⧺ show (length $ formats support)
  logDebug $ "Available Present Modes: " ⧺ show (presentModes support)
  
  -- Create sync objects
  syncObjects ← createSyncObjects device defaultGraphicsConfig
  modify $ \s → s { graphicsState = (graphicsState s) {
                      syncObjects = Just syncObjects } }
  
  -- Create command pool and frame resources
  let numFrames = gcMaxFrames defaultGraphicsConfig
  frameRes ← V.generateM (fromIntegral numFrames) $ \_ →
      createFrameResources device queues
  let cmdPool = frCommandPool $ frameRes V.! 0
  modify $ \s → s { graphicsState = (graphicsState s) {
                      frameResources = frameRes
                    , vulkanCmdPool  = Just cmdPool } }
  
  -- Create descriptor manager
  let descConfig = DescriptorManagerConfig
        { dmcMaxSets      = fromIntegral $ numFrames * 2
        , dmcUniformCount = fromIntegral numFrames
        , dmcSamplerCount = fromIntegral numFrames
        }
  descManager ← createVulkanDescriptorManager device descConfig
  logDebug $ "Descriptor Pool Created: " ⧺ show (dmPool descManager)
  modify $ \s → s { graphicsState = (graphicsState s) {
                      descriptorState = Just descManager } }
  
  -- Allocate descriptor sets
  descSets ← allocateVulkanDescriptorSets device descManager (fromIntegral numFrames)
  let updatedManager = descManager { dmActiveSets = descSets }
  modify $ \s → s { graphicsState = (graphicsState s) {
                      descriptorState = Just updatedManager } }
  logDebug $ "Descriptor Sets Allocated: " ⧺ show (V.length descSets)
  
  -- Create vertex buffer
  (vBuffer, vBufferMemory) ← createVertexBuffer device physicalDevice
                               (graphicsQueue queues) cmdPool
  logDebug $ "VertexBuffer: " ⧺ show vBuffer
  modify $ \s → s { graphicsState = (graphicsState s) {
                      vertexBuffer = Just (vBuffer, vBufferMemory) } }
  
  -- Create texture descriptor pool and layout
  descriptorPool ← createTextureDescriptorPool device
  (uniformLayout, texLayout) ← createVulkanDescriptorSetLayout device
  logDebug "Created descriptor pool and layout"
  
  -- Initialize textures
  initializeTextures device physicalDevice cmdPool (graphicsQueue queues) 
                     descriptorPool texLayout
  
  -- Create default scene
  let defaultSceneId = "default"
  sceneMgr ← gets sceneManager
  let sceneWithDefault = createScene defaultSceneId defaultCamera sceneMgr
      activeScene = setActiveScene defaultSceneId sceneWithDefault
  modify $ \s → s { sceneManager = activeScene }
  logDebug $ "Created default scene with id: " ⧺ defaultSceneId
  
  -- Create uniform buffers
  createUniformBuffersForFrames device physicalDevice glfwWin descSets
  
  -- Create render pass
  renderPass ← createVulkanRenderPass device (siSwapImgFormat swapInfo)
  logDebug $ "RenderPass: " ⧺ show renderPass
  modify $ \s → s { graphicsState = (graphicsState s) {
                      vulkanRenderPass = Just renderPass } }
  
  -- Create sprite pipeline
  (pipeline, pipelineLayout) ← createVulkanRenderPipeline device renderPass
                                 (siSwapExtent swapInfo) uniformLayout
  logDebug $ "Pipeline: " ⧺ show pipeline
  let pstate = PipelineState pipeline pipelineLayout renderPass
  modify $ \s → s { graphicsState = (graphicsState s) {
                      pipelineState = Just pstate } }
  
  -- Create font pipeline
  (fontPipe, fontPipeLayout, fontDescLayout) ←
    createFontPipeline device renderPass (siSwapExtent swapInfo) uniformLayout
  logDebug $ "Font Pipeline: " ⧺ show fontPipe
  modify $ \s → s { graphicsState = (graphicsState s) {
                      fontPipeline = Just (fontPipe, fontPipeLayout)
                    , fontDescriptorLayout = Just fontDescLayout } }
  
  -- Create font quad buffer
  quadBuf ← createFontQuadBuffer device physicalDevice (graphicsQueue queues) cmdPool
  logDebug $ "Font Quad Buffer: " ⧺ show quadBuf
  modify $ \s → s { graphicsState = (graphicsState s) {
                      fontQuadBuffer = Just quadBuf } }
  logDebug "Font system initialized"
  
  -- Create swapchain image views
  imageViews ← createSwapchainImageViews device swapInfo
  logDebug $ "ImageViews: " ⧺ show (length imageViews)
  
  -- Create framebuffers
  framebuffers ← createVulkanFramebuffers device renderPass swapInfo imageViews
  logDebug $ "Framebuffers: " ⧺ show (length framebuffers)
  modify $ \s → s { graphicsState = (graphicsState s) {
                      framebuffers = Just framebuffers } }
  
  pure cmdPool

-- | Initialize texture subsystem
-- Using general EngineM ε σ to be callable from any context
initializeTextures ∷ Device → PhysicalDevice → CommandPool → Queue
  → DescriptorPool → DescriptorSetLayout
  → EngineM ε σ ()
initializeTextures device _physicalDevice _cmdPool _queue descriptorPool textureLayout = do
  let poolState = TexturePoolState descriptorPool textureLayout
  modify $ \s → s { graphicsState = (graphicsState s) {
                      textureState = (poolState, V.empty) } }
  
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
        , tasCurrentCapacity = 0
        , tasCurrentCount = 0
        , tasHandleToIndex = Map.empty
        }
  
  modify $ \s → s { graphicsState = (graphicsState s) {
      textureArrayStates = Map.singleton "default" textureArrayState
  } }

-- | Create uniform buffers for all frames
-- Using general EngineM ε σ to be callable from any context
createUniformBuffersForFrames ∷ Device → PhysicalDevice 
  → GLFWRaw.Window → V.Vector DescriptorSet → EngineM ε σ ()
createUniformBuffersForFrames device physicalDevice glfwWin descSets = do
  (width, height) ← GLFW.getFramebufferSize glfwWin
  
  state ← gets graphicsState
  let camera = camera2D state
      uboData = UBO identity (createViewMatrix camera) 
                    (createProjectionMatrix camera (fromIntegral width) (fromIntegral height))
      uboSize = fromIntegral $ sizeOf uboData
      numFrames = gcMaxFrames defaultGraphicsConfig
  
  uniformBuffers ← V.generateM (fromIntegral numFrames) $ \_ → do
      (buffer, memory) ← createUniformBuffer device physicalDevice uboSize
      updateUniformBuffer device memory (UBO identity identity identity)
      pure (buffer, memory)
  
  logDebug $ "UniformBuffers created: " ⧺ show (V.length uniformBuffers)
  modify $ \s → s { graphicsState = (graphicsState s) {
                      uniformBuffers = Just uniformBuffers } }
  
  -- Update descriptor sets
  forM_ (zip [0..] (V.toList uniformBuffers)) $ \(i, (buffer, _)) → do
    let bufferInfo = (zero ∷ DescriptorBufferInfo)
          { buffer = buffer
          , offset = 0
          , range  = uboSize
          }
        write = (zero ∷ WriteDescriptorSet '[])
          { dstSet          = descSets V.! i
          , dstBinding      = 0
          , dstArrayElement = 0
          , descriptorCount = 1
          , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , bufferInfo      = V.singleton bufferInfo
          }
    updateDescriptorSets device (V.singleton $ SomeStruct write) V.empty

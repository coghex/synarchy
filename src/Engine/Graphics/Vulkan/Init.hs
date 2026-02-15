{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Init
  ( initializeVulkan
  , createUniformBuffersForFrames
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFWRaw
import Data.IORef (readIORef, writeIORef)
import Foreign.Storable (sizeOf)
import Linear (M44, identity)
import Engine.Asset.Handle
import Engine.Core.Defaults
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logDebugSM, logInfoSM)
import Engine.Graphics.Base
import Engine.Graphics.Config
import Engine.Graphics.Camera
import Engine.Graphics.Types
import Engine.Graphics.Font.Load (createFontDescriptorPool)
import Engine.Graphics.Font.Draw (createFontPipeline
                                 , createFontUIPipeline, createFontQuadBuffer)
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
import Engine.Graphics.Vulkan.Pipeline.Bindless (createBindlessPipeline
                                                , createBindlessUIPipeline)
import Engine.Graphics.Vulkan.MSAA (createMSAAColorImage)
import Engine.Graphics.Vulkan.Swapchain
import Engine.Graphics.Vulkan.Sync (createSyncObjects)
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Texture.System
import Engine.Graphics.Vulkan.Texture.Types
import Engine.Graphics.Vulkan.Texture.DefaultFaceMap (createDefaultFaceMap
                                                     , DefaultFaceMap(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Vertex
import Engine.Loop.Frame (computeAmbientLight)
import Engine.Scene.Manager (createScene, setActiveScene)
import Engine.Scene.Types
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends (SomeStruct(..))

-- | Initialize all Vulkan resources
initializeVulkan ∷ Window → EngineM ε σ CommandPool
initializeVulkan window = do
  let Window glfwWin = window
  
  -- Create Vulkan instance
  logDebugM CatVulkan "Creating Vulkan instance"
  (vkInstance, _debugMessenger) ← createVulkanInstance defaultGraphicsConfig
  logDebugM CatVulkan "Vulkan instance created successfully"
  
  -- Create surface
  logDebugM CatVulkan "Creating window surface"
  surface ← GLFW.createWindowSurface window vkInstance
  logDebugM CatVulkan "Window surface created successfully"
  
  -- Select physical device and create logical device
  logDebugM CatVulkan "Selecting physical device"
  physicalDevice ← pickPhysicalDevice vkInstance surface
  
  -- Log device info
  props ← liftIO $ getPhysicalDeviceProperties physicalDevice
 
  logDebugM CatVulkan "Creating logical device"
  (device, queues) ← createVulkanDevice vkInstance physicalDevice surface
  logDebugM CatVulkan "Logical device created successfully"
  
  logDebugSM CatVulkan "Queue family indices selected"
    [("graphics_family", T.pack $ show $ graphicsFamIdx queues)
    ,("present_family", T.pack $ show $ presentFamIdx queues)]
  
  modify $ \s → s { graphicsState = (graphicsState s)
                    { vulkanInstance = Just vkInstance
                    , vulkanPDevice  = Just physicalDevice
                    , vulkanDevice   = Just device
                    , vulkanSurface  = Just surface
                    , deviceQueues   = Just queues
                    } }
  
  -- Create swapchain
  env ← ask
  videoConfig ← liftIO $ readIORef (videoConfigRef env)
  let vsyncEnabled = vcVSync videoConfig
  
  logDebugSM CatVulkan "Creating swapchain"
    [("vsync", if vsyncEnabled then "enabled" else "disabled")]
  logDebugM CatGraphics "Creating swapchain"
  swapInfo ← createVulkanSwapchain physicalDevice device queues surface vsyncEnabled
  modify $ \s → s { graphicsState = (graphicsState s) {
                      swapchainInfo = Just swapInfo } }
  
  -- Log swapchain support
  support ← querySwapchainSupport physicalDevice surface

  -- read msaa from config, determine appropriate sample count
  let msaaInt = vcMSAA videoConfig
      requestedSamples = msaaToSampleCount msaaInt
  -- query device limits to clamp sample count
  let supportedSamples = framebufferColorSampleCounts (limits props)
      sampleCount = clampSampleCount supportedSamples requestedSamples
  logDebugSM CatVulkan "MSAA configuration"
    [("requested", T.pack $ show msaaInt)
    ,("actual_samples", T.pack $ show sampleCount)]
  
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
  logDebugSM CatDescriptor "Creating descriptor manager"
    [("max_sets", T.pack $ show $ dmcMaxSets descConfig)
    ,("uniform_count", T.pack $ show $ dmcUniformCount descConfig)]
  descManager ← createVulkanDescriptorManager device descConfig
  logDebugM CatDescriptor "Descriptor manager created successfully"
  modify $ \s → s { graphicsState = (graphicsState s) {
                      descriptorState = Just descManager } }
  -- create a font descriptor pool (supports up to 64 fonts)
  logDebugM CatDescriptor "Creating font descriptor pool"
  fontDescPool ← createFontDescriptorPool device 64
  modify $ \s → s { graphicsState = (graphicsState s) {
                      fontDescriptorPool = Just fontDescPool } }
  
  -- Allocate descriptor sets
  descSets ← allocateVulkanDescriptorSets device descManager (fromIntegral numFrames)
  let updatedManager = descManager { dmActiveSets = descSets }
  modify $ \s → s { graphicsState = (graphicsState s) {
                      descriptorState = Just updatedManager } }
  
  -- Create vertex buffer
  (vBuffer, vBufferMemory) ← createVertexBuffer device physicalDevice
                               (graphicsQueue queues) cmdPool
  modify $ \s → s { graphicsState = (graphicsState s) {
                      vertexBuffer = Just (vBuffer, vBufferMemory) } }
  
  -- Create uniform descriptor set layout
  let texSystemConfig = TextureSystemConfig
        { tscMaxTextures   = 16384
        , tscReservedSlots = 1      -- Slot 0 = undefined texture
        , tscForceBindless = False
        , tscForceLegacy   = False
        }
  texSystem ← createTextureSystem physicalDevice device cmdPool 
                                   (graphicsQueue queues) texSystemConfig
  (defaultFaceMap, texSystemWithFaceMap) ← createDefaultFaceMap
      physicalDevice device cmdPool (graphicsQueue queues) texSystem
  modify $ \s → s { graphicsState = (graphicsState s) {
                        textureSystem = Just texSystemWithFaceMap
                      , defaultFaceMapSlot = dfmSlot defaultFaceMap } }
  liftIO $ writeIORef (textureSystemRef env) (Just texSystemWithFaceMap)
  liftIO $ writeIORef (defaultFaceMapSlotRef env) (dfmSlot defaultFaceMap)
  
  -- Create default scene
  let defaultSceneId = "default"
  sceneMgr ← gets sceneManager
  let sceneWithDefault = createScene defaultSceneId defaultCamera sceneMgr
      activeScene = setActiveScene defaultSceneId sceneWithDefault
  modify $ \s → s { sceneManager = activeScene }
  
  -- Create uniform buffers
  createUniformBuffersForFrames device physicalDevice glfwWin descSets
  
  -- Create render pass
  renderPass ← createVulkanRenderPass device (siSwapImgFormat swapInfo) sampleCount
  modify $ \s → s { graphicsState = (graphicsState s) {
                      vulkanRenderPass = Just renderPass } }
  
  let bindlessTexLayout = btsDescriptorLayout texSystem
  logDebugM CatGraphics "Creating bindless pipeline"
  (bindlessPipe, bindlessPipeLayout) ← 
    createBindlessPipeline device renderPass (siSwapExtent swapInfo) 
                           (dmUniformLayout descManager)
                           bindlessTexLayout sampleCount
  logDebugM CatGraphics "Bindless pipeline created successfully"
  modify $ \s → s { graphicsState = (graphicsState s) {
                          bindlessPipeline = Just (bindlessPipe, bindlessPipeLayout) } }
  logDebugM CatGraphics "Creating bindless UI pipeline"
  (bindlessUIPipe, bindlessUIPipeLayout) ← 
    createBindlessUIPipeline device renderPass (siSwapExtent swapInfo) 
                             (dmUniformLayout descManager)
                             bindlessTexLayout sampleCount
  logDebugM CatGraphics "Bindless UI pipeline created successfully"
  modify $ \s → s { graphicsState = (graphicsState s) {
                          bindlessUIPipeline = Just (bindlessUIPipe, bindlessUIPipeLayout) } }
  
  -- Create font pipeline
  logDebugM CatGraphics "Creating font pipeline"
  (fontPipe, fontPipeLayout, fontDescLayout) ←
    createFontPipeline device renderPass (siSwapExtent swapInfo)
                              (dmUniformLayout descManager) sampleCount
  logDebugM CatGraphics "Font pipeline created successfully"
  modify $ \s → s { graphicsState = (graphicsState s) {
                      fontPipeline = Just (fontPipe, fontPipeLayout)
                    , fontDescriptorLayout = Just fontDescLayout } }
  logDebugM CatGraphics "Creating font UI pipeline"
  (fontUIPipe, fontUIPipeLayout) ←
    createFontUIPipeline device renderPass (siSwapExtent swapInfo)
                         (dmUniformLayout descManager) fontDescLayout sampleCount
  logDebugM CatGraphics "Font UI pipeline created successfully"
  modify $ \s → s { graphicsState = (graphicsState s) {
                      fontUIPipeline = Just (fontUIPipe, fontUIPipeLayout) } }
  
  -- Create font quad buffer
  quadBuf ← createFontQuadBuffer device physicalDevice (graphicsQueue queues) cmdPool
  modify $ \s → s { graphicsState = (graphicsState s) {
                      fontQuadBuffer = Just quadBuf } }
  
  -- Create swapchain image views
  imageViews ← createSwapchainImageViews device swapInfo
  modify $ \s → s { graphicsState = (graphicsState s) {
                      swapchainInfo = case swapchainInfo (graphicsState s) of
                        Just si → Just si { siSwapImgViews = imageViews }
                        Nothing → Nothing } }
  
  -- Create MSAA color image if needed
  mMsaaImageView ← if sampleCount ≢ SAMPLE_COUNT_1_BIT
    then do
      (img, mem, view) ← createMSAAColorImage physicalDevice device
                                              (siSwapImgFormat swapInfo)
                                              (siSwapExtent swapInfo) sampleCount
      modify $ \s → s { graphicsState = (graphicsState s) {
                          msaaColorImage = Just (img, mem, view) } }
      logDebugM CatGraphics "MSAA color image created successfully"
      pure (Just view)
    else do
      modify $ \s → s { graphicsState = (graphicsState s) {
                          msaaColorImage = Nothing } }
      pure Nothing
  
  -- Create framebuffers
  logDebugSM CatGraphics "Creating framebuffers"
    [("count", T.pack $ show $ V.length imageViews)]
  framebuffers ← createVulkanFramebuffers device renderPass swapInfo
                                          imageViews mMsaaImageView
  logDebugM CatGraphics "Framebuffers created successfully"
  modify $ \s → s { graphicsState = (graphicsState s) {
                      framebuffers = Just framebuffers } }
  
  logDebugM CatInit "Vulkan initialization complete"
  pure cmdPool


createUniformBuffersForFrames ∷ Device → PhysicalDevice 
  → GLFWRaw.Window → V.Vector DescriptorSet → EngineM ε σ ()
createUniformBuffersForFrames device physicalDevice glfwWin descSets = do
  (width, height) ← GLFW.getFramebufferSize glfwWin
  env ← ask
  let cRef   = cameraRef env
      uiCRef = uiCameraRef env
      bRef   = brightnessRef env
      psRef  = pixelSnapRef env
  state ← gets graphicsState
  camera ← liftIO $ readIORef cRef
  brightnessInt ← liftIO $ readIORef bRef
  pixelSnap ← liftIO $ readIORef psRef
  sunAngle ← liftIO $ readIORef (sunAngleRef env)
  
  -- UPDATE: Create/update UI camera with actual framebuffer size
  let uiCamera = defaultUICamera (fromIntegral width) (fromIntegral height)
      facing = case camFacing camera of
          FaceSouth → 0.0
          FaceWest → 1.0
          FaceNorth → 2.0
          FaceEast → 3.0
  liftIO $ writeIORef uiCRef uiCamera  -- Update the ref with correct size
  
  let ambientLight = computeAmbientLight sunAngle
      uboData = UBO 
          identity 
          (createViewMatrix camera) 
          (createProjectionMatrix camera (fromIntegral width) (fromIntegral height))
          (createUIViewMatrix uiCamera)
          (createUIProjectionMatrix uiCamera)
          (brightnessToMultiplier brightnessInt)
          (fromIntegral width) (fromIntegral height)
          (if pixelSnap then 1.0 else 0.0)
          sunAngle
          ambientLight
          facing
      uboSize = fromIntegral $ sizeOf uboData
      numFrames = gcMaxFrames defaultGraphicsConfig
  
  uniformBuffers ← V.generateM (fromIntegral numFrames) $ \_ → do
      (buffer, memory) ← createUniformBuffer device physicalDevice uboSize
      updateUniformBuffer device memory
          (UBO identity identity identity identity identity
               (brightnessToMultiplier brightnessInt)
               (fromIntegral width) (fromIntegral height)
               (if pixelSnap then 1.0 else 0.0)
               sunAngle ambientLight facing)
      pure (buffer, memory)
  
  modify $ \s → s { graphicsState = (graphicsState s) {
                      uniformBuffers = Just uniformBuffers } }
  
  -- Update descriptor sets
  forM_ (zip [0..] (V.toList uniformBuffers)) $ \(i, (buffer, _)) → do
    logDebugSM CatDescriptor "Updating descriptor set"
      [("set_index", T.pack $ show (i ∷ Int))
      ,("binding", "0")
      ,("type", "UNIFORM_BUFFER")
      ,("count", "1")]
    
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

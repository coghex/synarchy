{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException, throwIO, catch, SomeException)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (newTVarIO, newTQueueIO)
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (newIORef, readIORef, IORef, writeIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Linear (M44, V3(..), identity, (!*!), perspective, lookAt, translation, ortho)
import System.Environment (setEnv)
import System.Exit ( exitFailure )
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), takeBaseName)
import qualified HsLua as Lua
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
import Engine.Scripting.Lua.Backend
import Engine.Scripting.Lua.Types
import Engine.Graphics.Base
import Engine.Graphics.Config
import Engine.Graphics.Types
import Engine.Graphics.Font.Load (loadFont)
import Engine.Graphics.Font.Draw
import Engine.Graphics.Font.Data
import Engine.Input.Bindings
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
import Engine.Scripting.Loader (createBackend)
import Engine.Scripting.Backend (AnyBackend(..), BackendType(..), initBackend)
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
  lteq       ← Q.newQueue
  etlq       ← Q.newQueue
  -- initialize engine lifecycle io ref to block threads on engine status
  lifecycleRef ← newIORef EngineStarting
  -- initialize logging function
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  -- empty asset pool
  ap ← defaultAssetPool
  apRef ← newIORef ap
  nextObjIdRef ← newIORef 0
  -- initialize input state
  inputStateRef ← newIORef defaultInputState
  keyBindings ← loadKeyBindings "config/keybinds.json"
  keyBindingsRef ← newIORef keyBindings
  -- load video config
  videoConfig ← loadVideoConfig "config/video.yaml"
  -- create camera reference
  cameraRef ← newIORef defaultCamera
  -- Initialize engine environment and state
  let defaultEngineEnv = EngineEnv
        { engineConfig     = defaultEngineConfig
        , eventQueue       = eventQueue
        , inputQueue       = inputQueue
        , logFunc          = lf
        , luaToEngineQueue = lteq
        , luaQueue = etlq
        , lifecycleRef     = lifecycleRef
        , assetPoolRef     = apRef
        , nextObjectIdRef  = nextObjIdRef
        , inputStateRef    = inputStateRef
        , keyBindingsRef   = keyBindingsRef
        , cameraRef        = cameraRef
        }
  envVar ←   atomically $ newVar defaultEngineEnv
  stateVar ← atomically $ newVar $ defaultEngineState ap

  -- fork input thread
  inputThreadState ← startInputThread defaultEngineEnv
  -- fork scripting thread
  luaThreadState ← startLuaThread defaultEngineEnv
  
  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        -- initialize GLFW first
        window ← GLFW.createWindow $ defaultWindowConfig videoConfig
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
        -- create default active scene
        let defaultSceneId = "default"
            camera = defaultCamera
        sceneMgr ← gets sceneManager
        let sceneWithDefault = createScene defaultSceneId camera sceneMgr
            activeScene = setActiveScene defaultSceneId sceneWithDefault
        modify $ \s → s { sceneManager = activeScene }
        logDebug $ "Created default scene with id: " ⧺ defaultSceneId

        -- create uniform buffers
        (width, height) ← GLFW.getFramebufferSize glfwWin
        let modelMatrix = identity
            viewMatrix = createViewMatrix camera
            projMatrix = createProjectionMatrix camera
                         (fromIntegral width)
                         (fromIntegral height)
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

        -- create a pipeline for fonts
        (fontPipe, fontPipeLayout) ← createFontPipeline device renderPass
                                       (siSwapExtent swapInfo) uniformLayout
        logDebug $ "Font Pipeline: " ⧺ show fontPipe
        modify $ \s → s { graphicsState = (graphicsState s) {
                            fontPipeline = Just (fontPipe, fontPipeLayout) } }
        -- create shared quad buffer for text rendering
        quadBuf ← createFontQuadBuffer device physicalDevice
                      (graphicsQueue queues) cmdPool
        logDebug $ "Font Quad Buffer: " ⧺ show quadBuf
        modify $ \s → s { graphicsState = (graphicsState s) {
                            fontQuadBuffer = Just quadBuf } }
        logDebug "font system initialized"

        -- create swapchain image views
        imageViews ← createSwapchainImageViews device swapInfo
        logDebug $ "ImageViews: " ⧺ show (length imageViews)
        
        -- create framebuffers
        framebuffers ← createVulkanFramebuffers device renderPass swapInfo imageViews
        logDebug $ "Framebuffers: " ⧺ show (length framebuffers)
        modify $ \s → s { graphicsState = (graphicsState s) {
                            framebuffers = Just framebuffers } }

        mainLoop
        shutdownEngine window inputThreadState luaThreadState
        logDebug "Engine shutdown complete."
 
  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → do
        putStrLn $ displayException err
        liftIO $ shutdownThread inputThreadState
        liftIO $ shutdownThread luaThreadState
    Right _  → pure ()

-- the ever important shutdown function
shutdownEngine ∷ Window → ThreadState → ThreadState → EngineM' EngineEnv ()
shutdownEngine (Window win) its lts = do
    logDebug "Engine cleaning up..."
    state ← gets graphicsState
    modify $ \s → s { graphicsState = (graphicsState s) {
                          textBatchQueue = V.empty }
                    , sceneManager = (sceneManager s) {
                          smBatchManager = createBatchManager } }
    -- Wait for Vulkan device to idle before resource cleanup
    forM_ (vulkanDevice state) $ \device → liftIO $ deviceWaitIdle device
    -- glfw cleanup, stopping polling first
    liftIO $ GLFW.postEmptyEvent
    GLFW.setWindowShouldClose win True
    liftIO $ clearGLFWCallbacks win
    -- Wait for input thread to finish
    env ← ask
    -- Signal thread to stop
    logDebug "Shutting down input thread..."
    liftIO $ shutdownThread its
    -- cleanup lua thread
    logDebug "Shutting down Lua thread..."
    liftIO $ shutdownThread lts
    -- Transition to stopped state
    liftIO $ writeIORef (lifecycleRef env) EngineStopped

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


processLuaMessages ∷ EngineM' EngineEnv ()
processLuaMessages = do
    env ← ask
    let lteq = luaToEngineQueue env
    -- check for messages
    maybeMsg ← liftIO $ Q.tryReadQueue lteq
    case maybeMsg of
      Nothing → return () -- no messages
      Just msg → case msg of

        LuaLoadTextureRequest handle path → do
          logDebug $ "Loading texture: " ⧺ (show path)
          assetId ← loadTextureAtlas (T.pack $ takeBaseName path) path "default"
          -- update shared asset pool
          apRef ← asks assetPoolRef
          liftIO $ do
            pool ← readIORef apRef
            updateAssetState @TextureHandle handle
                (AssetReady assetId []) pool
          -- update local state too
          pool ← liftIO $ readIORef apRef
          modify $ \s → s { assetPool = pool }
          logDebug $ "Texture loaded: handle=" ⧺ (show handle) ⧺
                     ", assetId=" ⧺ (show assetId)

        LuaLoadFontRequest handle path size → do
          logDebug $ "Loading font: " ⧺ (show path) ⧺ " size=" ⧺ show size
          -- Load font (if it fails, the exception will propagate up)
          actualHandle ← loadFont handle path size
          logDebug $ "Font loaded successfully:  handle=" ⧺ show actualHandle
          -- Send success message back to Lua thread
          let etlq = luaQueue env
          liftIO $ Q.writeQueue etlq (LuaFontLoaded actualHandle)

        LuaSpawnTextRequest oid x y fontHandle text → do
          sceneMgr ← gets sceneManager
          case smActiveScene sceneMgr of
            Just sceneId → do
              let node = (createSceneNode TextObject)
                    { nodeId = oid
                    , nodeTransform = defaultTransform { position = (x,y) }
                    , nodeFont = Just fontHandle
                    , nodeText = Just text
                    , nodeColor = Vec4 1 1 1 1
                    , nodeVisible = True
                    }
              case addObjectToScene sceneId node sceneMgr of
                Just (addedObjId, newSceneMgr) → do
                  modify $ \s → s { sceneManager = newSceneMgr }
                  logDebug $ "Text object succesfully spawned with object id: "
                           ⧺ show addedObjId
                Nothing → logDebug $ "Failed to add text object "
                                   ⧺ show oid ⧺ " to scene"
            Nothing → logDebug "cannot draw text: no active scene"

        LuaSpawnSpriteRequest objId x y width height texHandle → do
          logDebug $ "Spawning sprite id=" ⧺ show objId ⧺
                     " pos=(" ⧺ show x ⧺ ", " ⧺ show y ⧺ ")" ⧺
                     " size=(" ⧺ show width ⧺ ", " ⧺ show height ⧺ ")" ⧺
                     " tex=" ⧺ show texHandle
          -- get active scene
          sceneMgr ← gets sceneManager
          case smActiveScene sceneMgr of
            Just sceneId → do
              let node = (createSceneNode SpriteObject)
                    { nodeId = objId
                    , nodeTransform = defaultTransform { position = (x,y) }
                    , nodeTexture = Just texHandle
                    , nodeSize = (width, height)
                    , nodeColor = Vec4 1 1 1 1
                    , nodeVisible = True
                    }
              -- add to scene graph
              case addObjectToScene sceneId node sceneMgr of
                Just (addedObjId, newSceneMgr) → do
                  modify $ \s → s { sceneManager = newSceneMgr }
                  logDebug $ "Sprite succesfully spawned with object id: "
                           ⧺ show addedObjId
                Nothing → logDebug $ "Failed to add sprite "
                                   ⧺ show objId ⧺ " to scene"
            Nothing → logDebug "cannot spawn sprite: no active scene"

        LuaMoveSpriteRequest objId x y → do
          modifySceneNode objId $ \node →
            node { nodeTransform = (nodeTransform node) {
                      position = (x, y) } }

        LuaSetSpriteColorRequest objId color → do
          modifySceneNode objId $ \node →
            node { nodeColor = color }

        LuaSetSpriteVisibleRequest objId visible → do
          modifySceneNode objId $ \node →
            node { nodeVisible = visible }

        LuaDestroySpriteRequest objId → do
          sceneMgr ← gets sceneManager
          case smActiveScene sceneMgr of
            Just sceneId → do
              case Map.lookup sceneId (smSceneGraphs sceneMgr) of
                Just graph → do
                  let updatedGraph = graph { sgNodes      = Map.delete objId
                                                              (sgNodes graph)
                                           , sgWorldTrans = Map.delete objId
                                                              (sgWorldTrans graph)
                                           , sgDirtyNodes = Set.delete objId
                                                              (sgDirtyNodes graph) }
                      updatedGraphs = Map.insert sceneId updatedGraph
                                                  (smSceneGraphs sceneMgr)
                  modify $ \s → s { sceneManager = sceneMgr {
                                      smSceneGraphs = updatedGraphs } }
                  logDebug $ "Destroyed sprite with object id: " ⧺ show objId
                Nothing → logInfo $ "no scene graph"
            Nothing → logInfo $ "no active scene"

        _ → return () -- unhandled message

-- helper function
modifySceneNode ∷ ObjectId → (SceneNode → SceneNode) → EngineM' EngineEnv ()
modifySceneNode objId f = do
  sceneMgr ← gets sceneManager
  case smActiveScene sceneMgr of
    Just sceneId → do
      case Map.lookup sceneId (smSceneGraphs sceneMgr) of
        Just graph → do
          case Map.lookup objId (sgNodes graph) of
            Just node → do
              let updatedNode = f node
                  updatedGraph = graph { sgNodes = Map.insert objId updatedNode
                                                             (sgNodes graph)
                                       , sgDirtyNodes = Set.insert objId
                                                             (sgDirtyNodes graph) }
                  updatedGraphs = Map.insert sceneId updatedGraph
                                              (smSceneGraphs sceneMgr)
              modify $ \s → s { sceneManager = sceneMgr {
                                  smSceneGraphs = updatedGraphs } }
            Nothing → logInfo $ "Sprite not found: " ⧺ (show objId)
        Nothing → logInfo $ "no scene graph"
    Nothing → logInfo $ "no active scene"

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
    state' ← gets graphicsState
    let textBatches = textBatchQueue state'
    batches ← getCurrentRenderBatches
    -- update uniform buffer
    case (vulkanDevice state, uniformBuffers state) of
        (Just device, Just buffers) → do
            let (_, memory) = buffers V.! fromIntegral frameIdx
            (fbWidth, fbHeight) ← GLFW.getFramebufferSize win
            (winWidth, winHeight) ← GLFW.getWindowSize win
            -- Create matrices using camera
            let camera = camera2D state
                modelMatrix = identity
                viewMatrix = createViewMatrix camera
                projMatrix = createProjectionMatrix camera 
                             (fromIntegral fbWidth) 
                             (fromIntegral fbHeight)
                uboData = UBO modelMatrix viewMatrix projMatrix
            env ← ask
            liftIO $ writeIORef (cameraRef env) camera
            liftIO $ atomicModifyIORef' (inputStateRef env) $ \is →
                (is { inpWindowSize = (winWidth, winHeight)
                    , inpFramebufferSize = (fbWidth, fbHeight) }, ())
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
    cleanupPendingInstanceBuffers
    
    -- Acquire next image
    swapchain ← case swapchainInfo state of
        Nothing → throwGraphicsError SwapchainError "No swapchain"
        Just si → pure $ siSwapchain si
    (acquireResult, imageIndex) ← liftIO $ acquireNextImageKHR device swapchain
                                    maxTimeout (frImageAvailable resources) zero
    
    -- Check if we need to recreate swapchain
    when (acquireResult ≠ SUCCESS && acquireResult ≠ SUBOPTIMAL_KHR) $
        throwGraphicsError SwapchainError $
            T.pack $ "Failed to acquire next image: " ⧺ show acquireResult

    -- reset and record command buffer
    liftIO $ resetCommandBuffer cmdBuffer zero
    recordSceneCommandBuffer cmdBuffer (fromIntegral imageIndex)
                             dynamicBuffer batches textBatches

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
          flushed ← liftIO $ Q.flushQueue (inputQueue env)
          -- double check if its flushed
          when (not $ null flushed) $
              logDebug $ "Unexpected inputs found during engine start: " ⧺ show flushed
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
              --logDebug $ "FPS: " ⧺ show fps
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
          processLuaMessages
          shouldClose ← GLFW.windowShouldClose glfwWindow
          env ← ask
          lifecycle ← liftIO $ readIORef (lifecycleRef env)
          if shouldClose || lifecycle ≢ EngineRunning
              then do
                  logInfo "Engine shutting down..."
                  liftIO $ writeIORef (lifecycleRef env) CleaningUp
              else unless (lifecycle ≡ CleaningUp) $ do
                  drawFrame
                  mainLoop
        CleaningUp → logDebug "Engine is cleaning up"
        EngineStopped → logDebug "Engine has stopped"

-- | Extract the GLFW window from the graphics state
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

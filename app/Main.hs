{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import Control.Exception (displayException)
import Control.Monad (void)
import qualified Control.Monad.Logger.CallStack as Logger
import Control.Monad.State (modify)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (setEnv)
import System.Exit ( exitFailure )
import System.FilePath ((</>))
import Engine.Core.Monad (runEngineM, EngineM')
import Engine.Core.Types
import Engine.Core.Resource
import Engine.Core.Error.Exception
import Engine.Concurrent.Var
import Engine.Graphics.Types
import Engine.Graphics.Window.GLFW (initializeGLFW, terminateGLFW
                                   , createWindow, destroyWindow, createWindowSurface)
import Engine.Graphics.Window.Types (WindowConfig(..))
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture (TexturePoolState(..))
import Engine.Graphics.Vulkan.Instance (createVulkanInstance)
import Engine.Graphics.Vulkan.Command (createVulkanCommandCollection
                                      , VulkanCommandCollection(..))
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Device (createVulkanDevice, pickPhysicalDevice)
import Engine.Graphics.Vulkan.Pipeline
import Engine.Graphics.Vulkan.Swapchain (createVulkanSwapchain, querySwapchainSupport)
import Engine.Graphics.Vulkan.Sync (createSyncObjects)
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Types.Texture
import qualified Engine.Graphics.Window.GLFW as GLFW
import Vulkan.Core10
import Vulkan.Zero
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
  { frameCount      = 0
  , engineRunning   = True
  , currentTime     = 0.0
  , deltaTime       = 0.0
  , logFunc         = lf
  , textureState    = (TexturePoolState zero zero, V.empty)
  , descriptorState = Nothing
  , pipelineState   = Nothing
  }

main ∷ IO ()
main = do
#ifdef DEVELOPMENT
  setEnv "VK_LOADER_DEBUG" "none"
  setEnv "VK_LOADER_MESSAGE_LEVEL" "error"
  setEnv "VK_LOADER_LOG_LEVEL" "0"
#else
#endif
  -- Initialize engine environment and state
  envVar ←   atomically $ newVar (undefined ∷ EngineEnv)
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  stateVar ← atomically $ newVar $ defaultEngineState lf
  
  let engineAction ∷ EngineM' EngineEnv ()
      engineAction = do
        -- Create window using the correct WindowConfig structure
        window ← GLFW.createWindow defaultWindowConfig
        
        -- Create Vulkan instance
        (vkInstance, _debugMessenger) ← createVulkanInstance defaultGraphicsConfig
        
        -- Create surface
        surface ← createWindowSurface window vkInstance
        
        -- Select physical device and create logical device
        physicalDevice ← pickPhysicalDevice vkInstance surface
        (device, queues) ← createVulkanDevice vkInstance physicalDevice surface
        
        -- Print some info about the device
        props ← liftIO $ getPhysicalDeviceProperties physicalDevice
        logDebug $ "Selected device: " ⧺ show (deviceName props)
        
        -- Test swapchain creation
        swapInfo ← createVulkanSwapchain physicalDevice device
                                         queues surface
        logDebug $ "Swapchain Format: " ⧺ show (siSwapImgFormat swapInfo)

        -- Test swapchain support query
        support ← querySwapchainSupport physicalDevice surface
        logDebug $ "Available Formats: " ⧺ show (length $ formats support)
        logDebug $ "Available Present Modes: " ⧺ show (presentModes support)

        -- Create sync objects
        syncObjects ← createSyncObjects device defaultGraphicsConfig

        -- Create command pool and buffers
        cmdCollection ← createVulkanCommandCollection device queues
                          (fromIntegral $ length $ imageAvailableSemaphores syncObjects)
        logDebug $ "CommandPool: " ⧺ show (length $ vccCommandBuffers cmdCollection)

        -- Create descriptor set layout
        descSetLayout ← createVulkanDescriptorSetLayout device
        logDebug $ "DescriptorSetLayout: " ⧺ show descSetLayout

        -- Initialize textures
        initializeTextures device physicalDevice (vccCommandPool cmdCollection) (graphicsQueue queues)

        -- Create Descriptor Pool
        let descConfig = DescriptorManagerConfig
              { dmcMaxSets      = fromIntegral $ gcMaxFrames defaultGraphicsConfig * 2
              , dmcUniformCount = fromIntegral $ gcMaxFrames defaultGraphicsConfig
              , dmcSamplerCount = fromIntegral $ gcMaxFrames defaultGraphicsConfig
              }
        descManager ← createVulkanDescriptorManager device descConfig
        logDebug $ "Descriptor Pool Created: " ⧺ show (dmPool descManager)

        -- Allocate initial descriptor sets
        descSets ← allocateVulkanDescriptorSets device descManager
                     (fromIntegral $ gcMaxFrames defaultGraphicsConfig)
        logDebug $ "Descriptor Sets Allocated: " ⧺ show (V.length descSets)

        -- create render pass
        renderPass ← createVulkanRenderPass device (siSwapImgFormat swapInfo)
        logDebug $ "RenderPass: " ⧺ show renderPass

        -- create pipeline
        (pipeline, pipelineLayout) ← createVulkanRenderPipeline device renderPass
                                       (siSwapExtent swapInfo) descSetLayout
        logDebug $ "Pipeline: " ⧺ show pipeline
        logDebug $ "PipelineLayout: " ⧺ show pipelineLayout

  
  result ← runEngineM engineAction envVar stateVar checkStatus
  case result of
    Left err → putStrLn $ displayException err
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
  
  logDebug "Textures initialized successfully"

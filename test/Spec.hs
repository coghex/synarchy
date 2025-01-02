-- test/Spec.hs
module Main where

import UPrelude
import Test.Hspec
import qualified Test.Engine.Graphics.Vulkan.Command as Command
import qualified Test.Engine.Graphics.Vulkan.Descriptor as Descriptor
import qualified Test.Engine.Graphics.Vulkan.Device as VulkanDevice
import qualified Test.Engine.Graphics.Vulkan.Instance as VulkanInstance
import qualified Test.Engine.Core.Monad as CoreMonad
import qualified Test.Engine.Graphics.Vulkan.Swapchain as Swapchain
import qualified Test.Engine.Graphics.Vulkan.Sync as Sync
import qualified Test.Engine.Graphics.Vulkan.Texture as Texture
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Graphics.Window.Types (Window(..))
import System.IO (stderr, hPutStrLn)
import Control.Monad (unless)

-- Borrowed from your madrigal project
errorCallback :: String -> String -> IO ()
errorCallback description error = 
    hPutStrLn stderr $ "GLFW Error: " ++ description ++ " " ++ error

initGLFW :: IO GLFW.Window
initGLFW = do
    -- Set error callback first
    GLFW.setErrorCallback (Just (\e d -> errorCallback (show e) (show d)))
    
    -- Initialize GLFW
    success <- GLFW.init
    unless success $ error "Failed to init GLFW"
    
    -- Check Vulkan support
    supported <- GLFW.vulkanSupported
    unless supported $ error "GLFW does not support Vulkan"
    
    -- Set window hints before creation
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    -- Create minimal window for testing
    mWindow <- GLFW.createWindow 1 1 "Test Window" Nothing Nothing
    case mWindow of
        Nothing -> error "Failed to create GLFW window"
        Just window -> pure window

main ∷ IO ()
main = do
  bracket
    initGLFW           -- setup
    GLFW.destroyWindow -- cleanup
    (\window -> hspec $ do
        describe "Core Monad" CoreMonad.spec
        describe "Vulkan Instance" VulkanInstance.spec
        describe "Vulkan Device" VulkanDevice.spec
        describe "Swapchain" (Swapchain.spec (Window window))
        describe "Sync" (Sync.spec (Window window))
        describe "Command" (Command.spec (Window window))
        describe "Descriptor" (Descriptor.spec (Window window))
        describe "Texture" (Texture.spec (Window window))
    )

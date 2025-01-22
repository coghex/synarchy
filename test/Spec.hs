-- test/Spec.hs
module Main where

import UPrelude
import Test.Hspec
import qualified Test.UPrelude as UPrelude
import qualified Test.Engine.Core.Error.Exception as CoreException
import qualified Test.Engine.Core.Monad as CoreMonad
import qualified Test.Engine.Core.Resource as CoreResource
import qualified Test.Engine.Core.Queue as CoreQueue
import qualified Test.Engine.Core.Var as CoreVar
import qualified Test.Engine.Graphics.Vulkan.Instance as VulkanInstance
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
        describe "UPrelude" UPrelude.spec
        describe "Engine.Core.Monad" CoreMonad.spec
        describe "Engine.Core.Resource" CoreResource.spec
        describe "Engine.Core.Queue" CoreQueue.spec
        describe "Engine.Core.Var" CoreVar.spec
        describe "Engine.Core.Error.Exception" CoreException.spec
        describe "Engine.Graphics.Vulkan.Instance" VulkanInstance.spec
    )

module Test.Engine.Graphics.Window.GLFW where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.GLFW
import Engine.Graphics.Window.Types
import Engine.Core.Monad
import Engine.Core.Types
import Engine.Concurrent.Var
import qualified Data.Text as T

spec ∷ Spec
spec = describe "GLFW Window" $ do
  it "should initialize GLFW" $ do
    let initialState = EngineState
          { frameCount    = 0
          , engineRunning = True
          , currentTime   = 0.0
          , deltaTime     = 0.0
          }
        initialEnv = EngineEnv
          { engineConfig   = EngineConfig
              { windowWidth  = 800
              , windowHeight = 600
              , enableVSync  = True
              , enableDebug  = True
              }
          , vulkanInstance = error "Instance not yet created"
          , vulkanDevice   = error "Device not yet created"
          }
    
    envVar ← atomically $ newVar initialEnv
    stateVar ← atomically $ newVar initialState
    
    result ← runEngineM initializeGLFW envVar stateVar pure
    case result of
      Right _ → pure ()
      Left err → fail $ show err

  it "should create and destroy window" $ do
    let initialState = EngineState
          { frameCount    = 0
          , engineRunning = True
          , currentTime   = 0.0
          , deltaTime     = 0.0
          }
        initialEnv = EngineEnv
          { engineConfig   = EngineConfig
              { windowWidth  = 800
              , windowHeight = 600
              , enableVSync  = True
              , enableDebug  = True
              }
          , vulkanInstance = error "Instance not yet created"
          , vulkanDevice   = error "Device not yet created"
          }
        config = WindowConfig
          { wcWidth     = 800
          , wcHeight    = 600
          , wcTitle     = T.pack "Test Window"
          , wcResizable = True
          }
    
    envVar ← atomically $ newVar initialEnv
    stateVar ← atomically $ newVar initialState
    
    result ← runEngineM
      (do
        initializeGLFW
        window ← createWindow config
        destroyWindow window
        terminateGLFW
      ) envVar stateVar pure
    
    case result of
      Right _ → pure ()
      Left err → fail $ show err

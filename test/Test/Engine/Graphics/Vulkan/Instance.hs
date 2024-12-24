module Test.Engine.Graphics.Vulkan.Instance where

import UPrelude
import Test.Hspec
import Engine.Graphics.Vulkan.Instance
import Engine.Graphics.Types
import Engine.Core.Monad
import Engine.Core.Types
import qualified Data.Text as T

spec ∷ Spec
spec = describe "Vulkan Instance" $ do
  it "should create instance with validation layers when debug enabled" $ do
    let config = GraphicsConfig
          { gcAppName   = T.pack "Test App"
          , gcDebugMode = True
          , gcWidth     = 800
          , gcHeight    = 600
          }
        envVar   = undefined  -- you'll need to mock these
        stateVar = undefined
    result ← runEngineM (createVulkanInstance config) envVar stateVar pure
    case result of
      Right (inst, Just _) → pure ()  -- success with debug messenger
      _                    → fail "Failed to create Vulkan instance with debug"

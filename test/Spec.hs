-- test/Spec.hs
module Main where

import UPrelude
import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Test.UPrelude as UPrelude
import qualified Test.Engine.Core.Monad as CoreMonad
import qualified Test.Engine.Core.Resource as CoreResource
import qualified Test.Engine.Core.Queue as CoreQueue
import qualified Test.Engine.Graphics.Window.GLFW as GLFWTest
import Control.Concurrent (threadDelay)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Graphics.Window.Types (Window(..))
import Engine.Core.State
import Engine.Core.Defaults
import Engine.Core.Base
import Engine.Core.Queue as Q
import Engine.Core.Error.Exception
import Engine.Input.Types
import qualified Control.Monad.Logger.CallStack as Logger

-- | Initialize a minimal EngineState for testing
initTestState ∷ IO (EngineEnv, EngineState)
initTestState = do
    GLFW.setErrorCallback (Just (\e d → print $ "GLFW Error: " <> show e <> " " <> show d))
    success ← GLFW.init
    unless success $ error "GLFW initialization failed"

    -- Create queues
    eventQ ← Q.newQueue
    inputQ ← Q.newQueue
    logQ ← Q.newQueue
    
    -- Create logging function
    logFunc ← Logger.runStdoutLoggingT $ Logger.LoggingT pure

    let env = EngineEnv
            { engineConfig = defaultEngineConfig
            , eventQueue = eventQ
            , inputQueue = inputQ
            , logQueue = logQ }
    
    -- Return initial state
    pure $ (env, defaultEngineState logFunc)

main ∷ IO ()
main = do
    (env, initialState) ← initTestState
    hspec $ afterAll_ GLFW.terminate $ do
        -- Core tests (no graphics dependencies)
        describe "Core Tests" $ do
            describe "UPrelude" UPrelude.spec
            describe "Engine.Core.Monad" CoreMonad.spec
            describe "Engine.Core.Resource" CoreResource.spec
            describe "Engine.Core.Queue" CoreQueue.spec

        -- Graphics tests
        describe "Graphics Tests" $ do
            describe "GLFW Window Tests" $ 
                GLFWTest.spec env initialState

--            describe "Vulkan Tests" $ 
--                VulkanTest.spec initialState

        -- Input tests
--        describe "Input Tests" $ do
--            describe "Input Thread Tests" $ 
--                InputTest.spec initialState

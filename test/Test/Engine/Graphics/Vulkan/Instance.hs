-- test/Test/Engine/Graphics/Vulkan/Instance.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Vulkan.Instance (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State
import Engine.Core.Monad
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Engine.Graphics.Base
import Engine.Graphics.Vulkan.Instance
import Test.Engine.Graphics.Vulkan.Helpers (withTestInstance)
import Vulkan.Core10
import Vulkan.Extensions.VK_EXT_debug_utils
  (pattern EXT_DEBUG_UTILS_EXTENSION_NAME)
import qualified Control.Concurrent.STM as STM

-- | Main test specification for Vulkan Instance functionality
spec ∷ EngineEnv → EngineState → Spec
spec env state = do
    describe "Vulkan Instance" $ do
        it "can enumerate available extensions" $ do
            runEngineTest env state $ do
                exts <- getAvailableExtensions
                liftIO $ exts `shouldSatisfy` not . null

        it "can create instance with debug mode disabled" $ do
            runEngineTest env state $ do
                let config = defaultGraphicsConfig 
                        { gcDebugMode = False
                        , gcAppName = "VulkanTest" }
                withTestInstance config InstanceForWindow $ \(inst, dbgMessenger) ->
                    liftIO $ do
                        instanceHandle inst `shouldNotBe` nullPtr
                        dbgMessenger `shouldBe` Nothing

        -- Production enables VK_EXT_debug_utils only when the driver
        -- offers it (#1402), so the messenger tracks observed
        -- availability rather than debug mode alone. Which extensions
        -- that decision enables from which availability is pinned
        -- without a driver in Test.Headless.Graphics.InstancePlan.
        it "creates a debug messenger in debug mode iff debug utils exist" $ do
            runEngineTest env state $ do
                let config = defaultGraphicsConfig 
                        { gcDebugMode = True
                        , gcAppName = "VulkanTest" }
                exts <- getAvailableExtensions
                withTestInstance config InstanceForWindow $ \(inst, dbgMessenger) ->
                    liftIO $ do
                        instanceHandle inst `shouldNotBe` nullPtr
                        isJust dbgMessenger `shouldBe`
                            (EXT_DEBUG_UTILS_EXTENSION_NAME `elem` exts)

        -- Cycles, not nesting: each 'withTestInstance' owns its own
        -- continuation, so cycle n's instance is destroyed before cycle
        -- n+1 creates one (#1401). The counters assert that rather than
        -- leaving it to be read off the structure -- @liveRef@ is
        -- raised inside the scope and lowered after it, so the peak it
        -- records would be 3 if the brackets nested instead of cycling,
        -- and it is checked without any dependence on a finalizer, on
        -- GC, or on process exit.
        it "can create and destroy instance multiple times" $ do
            let config = defaultGraphicsConfig 
                    { gcDebugMode = False
                    , gcAppName = "VulkanTest" }
            liveRef   <- newIORef (0 :: Int)
            peakRef   <- newIORef (0 :: Int)
            cyclesRef <- newIORef (0 :: Int)
            runEngineTest env state $ forM_ [1 .. 3 :: Int] $ \_ -> do
                withTestInstance config InstanceForWindow $ \(inst, _dbg) ->
                    liftIO $ do
                        live <- atomicModifyIORef' liveRef $ \n -> (n + 1, n + 1)
                        modifyIORef' peakRef $ max live
                        instanceHandle inst `shouldNotBe` nullPtr
                liftIO $ do
                    modifyIORef' liveRef (subtract 1)
                    modifyIORef' cyclesRef (+ 1)
            readIORef cyclesRef `shouldReturn` 3
            readIORef peakRef `shouldReturn` 1
            readIORef liveRef `shouldReturn` 0

    where
        -- Helper functions
        runEngineTest ∷ ∀ α. EngineEnv → EngineState → EngineM EngineState α → IO α
        runEngineTest env state action = do
            stateRef ← newIORef state
            let env' = env { engineStateRef = stateRef }
            mvar ← STM.atomically $ STM.newTVar Nothing

            let cont result = case result of
                    Right v → do
                        STM.atomically $ STM.writeTVar mvar (Just v)
                        pure state
                    Left err → error $ "Engine error: " ⧺ show err

            _ ← unEngineM action env' cont
            result ← STM.atomically $ STM.readTVar mvar
            case result of
                Just v → pure v
                Nothing → error "No result produced"

-- | Default graphics configuration for testing
defaultGraphicsConfig ∷ GraphicsConfig
defaultGraphicsConfig = GraphicsConfig
    { gcDebugMode = False
    , gcAppName = "VulkanTest"
    , gcMaxFrames = 2
    }

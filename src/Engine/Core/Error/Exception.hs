{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Engine.Core.Error.Exception
  ( -- * Types
    EngineException(..)
  , ExceptionType(..)
  , GraphicsError(..)
  , ResourceError(..)
  , SystemError(..)
  , StateError(..)
  , InitError(..)
  , AssetError(..)
  -- * Functions
  , throwEngineException
  , mkErrorContext
  , contextCallStack
  , logInfo
--  , logExcept
  , logDebug
  , throwGraphicsError
  , throwResourceError
  , throwSystemError
  , throwStateError
  , throwInitError
  , throwAssetError
  , catchEngine
  , tryEngine
  ) where

import UPrelude
import Control.Exception (Exception, displayException)
import Control.Monad.Error.Class (MonadError(..), throwError)
import qualified Control.Monad.Logger.CallStack as LoggerCS
import GHC.Stack (HasCallStack, prettyCallStack, callStack, CallStack, getCallStack)
import qualified Data.Text as T
import Type.Reflection
import qualified Vulkan.Core10 as Vk

-- | Main exception type containing all possible engine errors
data ExceptionType 
  = ExGraphics GraphicsError    -- ^ Graphics/Vulkan related errors
  | ExResource ResourceError    -- ^ Resource management errors
  | ExSystem SystemError       -- ^ System-level errors
  | ExState StateError        -- ^ State management errors
  | ExInit InitError         -- ^ Initialization errors
  | ExAsset AssetError     -- ^ Asset loading errors
  deriving (Show, Eq, Typeable)

-- | Graphics-specific errors
data GraphicsError
  = VulkanDeviceLost         -- ^ Device was lost during operation
  | VulkanOutOfDate          -- ^ Swapchain is out of date
  | ShaderCompilationFailed  -- ^ Shader failed to compile
  | TextureLoadFailed        -- ^ Texture failed to load
  | SwapchainError           -- ^ Swapchain creation/management error
  | PipelineError            -- ^ Pipeline creation/management error
  | CommandBufferError       -- ^ Command buffer error
  | DescriptorError          -- ^ Descriptor set/pool error
  | RenderPassError          -- ^ Render pass error
  | FramebufferError         -- ^ Framebuffer error
  | VertexBufferError        -- ^ Vertex buffer error
  | CleanupError             -- ^ Cleanup error
  | VulkanError Vk.Result    -- ^ Raw Vulkan error
  deriving (Show, Eq, Typeable)

-- | Resource management errors
data ResourceError
  = ResourceNotFound FilePath          -- ^ Resource file not found
  | ResourceAlreadyLoaded FilePath     -- ^ Attempting to load already loaded resource
  | ResourceLoadFailed FilePath T.Text -- ^ Resource failed to load with reason
  | InvalidResourceFormat T.Text       -- ^ Resource format is invalid
  | ResourceAllocationFailed T.Text    -- ^ Failed to allocate resource
  | ResourceCountMismatch T.Text       -- ^ Resource count mismatch
  deriving (Show, Eq, Typeable)

-- | System-level errors
data SystemError
  = GLFWError T.Text        -- ^ GLFW-related error
  | ThreadError T.Text      -- ^ Threading-related error
  | MemoryError T.Text     -- ^ Memory allocation/management error
  | IOError T.Text         -- ^ General IO error
  | TimeoutError T.Text    -- ^ Operation timed out
  | TestError T.Text       -- ^ Test error
  deriving (Show, Eq, Typeable)

-- | State management errors
data StateError
  = InvalidStateTransition T.Text  -- ^ Invalid state transition attempted
  | MissingRequiredState T.Text    -- ^ Required state component missing
  | StateValidationFailed T.Text   -- ^ State validation failed
  | InconsistentState T.Text       -- ^ State inconsistency detected
  deriving (Show, Eq, Typeable)

-- | Initialization errors
data InitError
  = WindowCreationFailed    -- ^ Failed to create window
  | VulkanInitFailed       -- ^ Failed to initialize Vulkan
  | DeviceCreationFailed   -- ^ Failed to create logical device
  | ExtensionNotSupported  -- ^ Required extension not supported
  | ValidationLayerNotSupported -- ^ Required validation layer not supported
  deriving (Show, Eq, Typeable)

-- | Asset loading errors
data AssetError
  = AssetNotFound FilePath          -- ^ Asset file not found
  | AssetAlreadyLoaded FilePath     -- ^ Attempting to load already loaded asset
  | AssetLoadFailed FilePath T.Text -- ^ Asset failed to load with reason
  | InvalidAssetFormat T.Text       -- ^ Asset format is invalid
  | AssetAllocationFailed T.Text    -- ^ Failed to allocate asset
  | AssetCountMismatch T.Text       -- ^ Asset count mismatch
  deriving (Show, Eq, Typeable)

-- | Main exception type with enhanced context
data EngineException = EngineException
  { errorType    ∷ ExceptionType  -- ^ Type of error
  , errorMsg     ∷ T.Text         -- ^ Error message
  , errorContext ∷ ErrorContext -- ^ Additional context
  } deriving (Typeable)
instance Eq EngineException where
  (==) a b = errorType a == errorType b && errorMsg a == errorMsg b

-- | Error context, currently just a call stack
data ErrorContext = ErrorContext
  { contextCallStack ∷ CallStack
  } deriving (Typeable)

instance Show EngineException where
  show (EngineException etype msg ctx) = unlines
    [ "EngineException:"
    , "Type: " ⧺ show etype
    , "Message: " ⧺ T.unpack msg
    , "Stack:\n" ⧺ prettyCallStack (contextCallStack ctx)
    ]

instance Exception EngineException where
  displayException ex = show ex

-- | Helper function to throw engine exceptions
throwEngineException :: MonadError EngineException m => EngineException -> m a
throwEngineException = throwError

-- | Log an informational message
logInfo :: (HasCallStack, MonadError EngineException m, LoggerCS.MonadLogger m) 
        => String -> m ()
logInfo = LoggerCS.logInfoCS callStack . fromString

-- | Log an exception
--logExcept :: (HasCallStack, MonadError EngineException m)
--          => ExceptionType -> String -> m a
--logExcept exType msg = throwError $ EngineException exType 
--  $ T.pack (msg ++ "\n" ++ prettyCallStack callStack ++ "\n")

-- | Log a debug message (only in development)
logDebug :: (HasCallStack, MonadError EngineException m, LoggerCS.MonadLogger m) 
         => String -> m ()
#ifdef DEVELOPMENT
logDebug = LoggerCS.logDebugCS callStack . fromString
#else
logDebug = const $ pure ()
#endif

-- Helper functions for specific error types
throwGraphicsError :: MonadError EngineException m 
                  => GraphicsError -> T.Text -> m a
throwGraphicsError err msg = 
  throwError $ EngineException (ExGraphics err) msg mkErrorContext

throwResourceError :: MonadError EngineException m 
                  => ResourceError -> T.Text -> m a
throwResourceError err msg = 
  throwError $ EngineException (ExResource err) msg mkErrorContext

throwSystemError :: MonadError EngineException m 
                => SystemError -> T.Text -> m a
throwSystemError err msg = 
  throwError $ EngineException (ExSystem err) msg mkErrorContext

throwStateError :: MonadError EngineException m 
                => StateError -> T.Text -> m a
throwStateError err msg = 
  throwError $ EngineException (ExState err) msg mkErrorContext

throwInitError :: MonadError EngineException m 
               => InitError -> T.Text -> m a
throwInitError err msg = 
  throwError $ EngineException (ExInit err) msg mkErrorContext

throwAssetError :: MonadError EngineException m 
               => AssetError -> T.Text -> m a
throwAssetError err msg = 
  throwError $ EngineException (ExAsset err) msg mkErrorContext


-- | Create error context from current call stack
mkErrorContext ∷ HasCallStack ⇒ ErrorContext
mkErrorContext = 
  let stack = callStack
      ((_, loc):_) = getCallStack stack
  in ErrorContext
    { contextCallStack = stack
    }

-- | Catch EngineException in EngineM context
catchEngine :: MonadError EngineException m 
           => m a                                  -- ^ Action that might fail
           -> (EngineException -> m a)            -- ^ Handler for exceptions
           -> m a
catchEngine action handler = catchError action handler

-- | Try running an action, returning Either
tryEngine :: MonadError EngineException m 
         => m a 
         -> m (Either EngineException a)
tryEngine action = (Right <$> action) `catchEngine` (pure . Left)

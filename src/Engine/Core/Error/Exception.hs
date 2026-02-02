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
  , LuaError(..)
  -- * Functions
  , throwEngineException
  , mkErrorContext
  , contextCallStack
  , catchEngine
  , tryEngine
  ) where

import UPrelude
import Engine.Asset.Base (AssetId)
import Control.Exception (Exception, displayException)
import Control.Monad.Error.Class (MonadError(..), throwError)
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
  | ExLua LuaError         -- ^ Lua-specific errors
  deriving (Show, Eq, Typeable)

-- | Graphics-specific errors
data GraphicsError
  = VulkanDeviceLost         -- ^ Device was lost during operation
  | VulkanSurfaceLost        -- ^ Vulkan surface was lost
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
  | FontError                -- ^ Font rendering error
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
  = AssetNotFound AssetId           -- ^ Asset file not found
  | AssetAlreadyLoaded FilePath     -- ^ Attempting to load already loaded asset
  | AssetLoadFailed FilePath T.Text -- ^ Asset failed to load with reason
  | InvalidAssetFormat T.Text       -- ^ Asset format is invalid
  | AssetAllocationFailed T.Text    -- ^ Failed to allocate asset
  | AssetCountMismatch T.Text       -- ^ Asset count mismatch
  | AssetFailedCleanup
  deriving (Show, Eq, Typeable)

-- | Lua-specific errors
data LuaError
  = LuaSyntaxError T.Text        -- ^ Syntax error during Lua script parsing
  | LuaRuntimeError T.Text       -- ^ Runtime error during Lua script execution
  | LuaMissingFunction T.Text    -- ^ Attempt to call a nonexistent Lua function
  | LuaTypeError T.Text          -- ^ Invalid type provided for a Lua function argument
  | LuaMemoryError T.Text        -- ^ Lua VM ran out of memory
  | LuaCallbackError T.Text      -- ^ Error occurred in a Lua->Haskell callback
  | LuaExecutionTimeout T.Text   -- ^ Lua script exceeded execution time limit
  | LuaGenericError T.Text       -- ^ General Lua error with a specific message
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

-- | Create error context from current call stack
mkErrorContext ∷ HasCallStack ⇒ ErrorContext
mkErrorContext = ErrorContext { contextCallStack = callStack }

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

{-# LANGUAGE CPP #-}
module Engine.Core.Error.Exception
  ( -- * Types
    EngineException(..)
  , ExceptionType(..)
  , GraphicsError(..)
  , SystemError(..)
  , InitError(..)
  , AssetError(..)
  -- * Functions
  , throwEngineException
  , mkErrorContext
  , contextCallStack
  , catchEngine
  ) where

import UPrelude
import Engine.Asset.Base (AssetId)
import Control.Exception (Exception, displayException)
import Control.Monad.Error.Class (MonadError(..), throwError)
import GHC.Stack (HasCallStack, prettyCallStack, callStack, CallStack)
import qualified Data.Text as T

-- | Sum of every error domain in the engine
data ExceptionType
  = ExGraphics GraphicsError    -- ^ Graphics/Vulkan related errors
  | ExSystem SystemError       -- ^ System-level errors
  | ExInit InitError         -- ^ Initialization errors
  | ExAsset AssetError     -- ^ Asset loading errors
  deriving (Show, Eq)

data GraphicsError
  = VulkanDeviceLost         -- ^ Device was lost during operation
  | VulkanSurfaceLost        -- ^ Vulkan surface was lost
  | TextureLoadFailed        -- ^ Texture failed to load
  | SwapchainError           -- ^ Swapchain creation/management error
  | PipelineError            -- ^ Pipeline creation/management error
  | CommandBufferError       -- ^ Command buffer error
  | DescriptorError          -- ^ Descriptor set/pool error
  | RenderPassError          -- ^ Render pass error
  | FramebufferError         -- ^ Framebuffer error
  | CleanupError             -- ^ Cleanup error
  | FontError                -- ^ Font rendering error
  deriving (Show, Eq)

data SystemError
  = GLFWError T.Text        -- ^ GLFW-related error
  | MemoryError T.Text     -- ^ Memory allocation/management error
  | IOError T.Text         -- ^ General IO error
  | TimeoutError T.Text    -- ^ Operation timed out
  deriving (Show, Eq)

data InitError
  = WindowCreationFailed    -- ^ Failed to create window
  | VulkanInitFailed       -- ^ Failed to initialize Vulkan
  | DeviceCreationFailed   -- ^ Failed to create logical device
  | ExtensionNotSupported  -- ^ Required extension not supported
  deriving (Show, Eq)

data AssetError
  = AssetNotFound AssetId           -- ^ Asset file not found
  deriving (Show, Eq)

data EngineException = EngineException
  { errorType    ∷ ExceptionType  -- ^ Type of error
  , errorMsg     ∷ T.Text         -- ^ Error message
  , errorContext ∷ ErrorContext -- ^ Additional context
  }
instance Eq EngineException where
  (==) a b = errorType a ≡ errorType b ∧ errorMsg a ≡ errorMsg b

data ErrorContext = ErrorContext
  { contextCallStack ∷ CallStack
  }

instance Show EngineException where
  show (EngineException etype msg ctx) = unlines
    [ "EngineException:"
    , "Type: " ⧺ show etype
    , "Message: " ⧺ T.unpack msg
    , "Stack:\n" ⧺ prettyCallStack (contextCallStack ctx)
    ]

instance Exception EngineException where
  displayException ex = show ex

throwEngineException ∷ MonadError EngineException m ⇒ EngineException → m a
throwEngineException = throwError

mkErrorContext ∷ HasCallStack ⇒ ErrorContext
mkErrorContext = ErrorContext { contextCallStack = callStack }

catchEngine ∷ MonadError EngineException m 
           ⇒ m a                                  -- ^ Action that might fail
           → (EngineException → m a)            -- ^ Handler for exceptions
           → m a
catchEngine action handler = catchError action handler

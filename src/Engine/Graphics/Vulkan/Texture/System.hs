-- | Builds the renderer's bindless texture system from the capability the
--   device reports ("Engine.Graphics.Vulkan.Capability").
--
--   Bindless is a hard requirement of this renderer: there is no non-bindless
--   path, so anything short of 'BindlessTextures' — a device without
--   update-after-bind descriptor support, or one with too few usable slots
--   left after the reservations — fails initialization here.
module Engine.Graphics.Vulkan.Texture.System
  ( TextureSystemConfig(..)
  , createTextureSystem
  ) where

import UPrelude
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM)
import Engine.Core.Error.Exception (GraphicsError(..), ExceptionType(..))
import Engine.Graphics.Vulkan.Texture.Bindless
import Engine.Graphics.Vulkan.Texture.Limits (maxBindlessTextures)
import Engine.Graphics.Vulkan.Texture.Types
import Engine.Graphics.Vulkan.Capability
import Vulkan.Core10

-- | Create the texture system based on device capabilities
createTextureSystem ∷ PhysicalDevice
                    → Device
                    → CommandPool
                    → Queue
                    → TextureSystemConfig
                    → EngineM σ BindlessTextureSystem
createTextureSystem pdev dev cmdPool queue config = do
  support ← liftIO $ queryBindlessSupport pdev
  let capability = determineTextureCapability support (tscReservedSlots config)

  case capability of
    BindlessTextures maxSlots → do
      -- 'maxBindlessTextures' is the fixed upper bound the fragment
      -- shaders are compiled against; the device-reported figure may
      -- legitimately be lower, so allocate the minimum of the two.
      let actualMax = min maxBindlessTextures (min maxSlots (tscMaxTextures config))
      let bindlessConfig = defaultBindlessConfig
            { bcMaxTextures = actualMax
            }

      bindless ← createBindlessTextureSystem pdev dev cmdPool queue bindlessConfig
      pure bindless

    -- Selection already rejects a device that cannot do bindless at all
    -- ("Engine.Graphics.Vulkan.Device"), so what usually reaches here is a
    -- capable device with too few slots left after the reservations. Both
    -- sites build the message from 'unsupportedBindlessMessage' so they
    -- describe the same shortfall identically (#1282).
    unsupported →
      logAndThrowM CatTexture (ExGraphics TextureLoadFailed) $
        unsupportedBindlessMessage support unsupported

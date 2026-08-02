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
      let actualMax = min 16384 (min maxSlots (tscMaxTextures config))
      let bindlessConfig = defaultBindlessConfig
            { bcMaxTextures = actualMax
            }

      bindless ← createBindlessTextureSystem pdev dev cmdPool queue bindlessConfig
      pure bindless

    unsupported →
      logAndThrowM CatTexture (ExGraphics TextureLoadFailed) $
        "Bindless textures are required, but this device does not meet the \
        \renderer's required bindless capability: "
        <> describeCapability unsupported

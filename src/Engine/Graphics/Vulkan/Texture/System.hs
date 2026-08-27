-- | Builds the renderer's bindless texture system from the capability the
--   device reports ("Engine.Graphics.Vulkan.Capability").
--
--   Bindless is a hard requirement of this renderer: there is no non-bindless
--   path, so anything short of 'BindlessTextures' — a device without
--   update-after-bind descriptor support, one that cannot supply the whole
--   fixed descriptor binding, or one with too few usable slots left after
--   the reservations — fails initialization here.
module Engine.Graphics.Vulkan.Texture.System
  ( TextureSystemConfig(..)
  , planBindlessDescriptorCount
  , createTextureSystem
  ) where

import UPrelude
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugSM)
import Engine.Core.Error.Exception (GraphicsError(..), ExceptionType(..))
import Engine.Graphics.Vulkan.Texture.Bindless
import Engine.Graphics.Vulkan.Texture.Types
import Engine.Graphics.Vulkan.Capability
import Vulkan.Core10

-- | The whole pure decision 'createTextureSystem' runs: given what the
--   device reported and the configuration it was handed, either the
--   descriptor count to build the bindless binding with, or the message
--   explaining the refusal.
--
--   Exposed so a GPU-free test can pin the ACTUAL derivation rather than a
--   parallel copy of it. The @Right@ answer is always
--   'Engine.Graphics.Vulkan.Texture.Limits.maxBindlessTextures': neither
--   the device report nor the configuration can shrink the binding below
--   the array size both fragment shaders declare, which is what makes the
--   descriptor-set interface valid on every accepted device (#1689).
planBindlessDescriptorCount
  ∷ BindlessSupport → TextureSystemConfig → Either Text Word32
planBindlessDescriptorCount support config =
  case determineTextureCapability support (tscReservedSlots config) of
    BindlessTextures descriptorCount → Right descriptorCount
    -- Selection already rejects a device that cannot do bindless at all
    -- ("Engine.Graphics.Vulkan.Device"), so what usually reaches here is a
    -- capable device whose reservations leave too few application slots.
    -- Both sites build the message from 'unsupportedBindlessMessage' so
    -- they describe the same shortfall identically (#1282).
    unsupported → Left (unsupportedBindlessMessage support unsupported)

-- | Create the texture system based on device capabilities
createTextureSystem ∷ PhysicalDevice
                    → Device
                    → CommandPool
                    → Queue
                    → TextureSystemConfig
                    → EngineM σ BindlessTextureSystem
createTextureSystem pdev dev cmdPool queue config = do
  support ← liftIO $ queryBindlessSupport pdev

  case planBindlessDescriptorCount support config of
    Left failure →
      logAndThrowM CatTexture (ExGraphics TextureLoadFailed) failure
    Right descriptorCount → do
      -- The binding is sized by 'maxBindlessTextures' inside
      -- "Engine.Graphics.Vulkan.Texture.Bindless" itself, so the planned
      -- count cannot disagree with what gets built; it is logged because the
      -- interesting fact is that the device was CHECKED for it.
      logDebugSM CatTexture "Bindless descriptor capacity accepted"
        [("descriptors", tshow descriptorCount)]
      createBindlessTextureSystem pdev dev cmdPool queue defaultBindlessConfig

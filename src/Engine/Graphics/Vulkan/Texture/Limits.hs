-- | The two compile-time bindless limits that the Haskell renderer and its
--   GLSL fragment shaders must agree on, defined exactly once.
--
--   They live in a module of their own because
--   "Engine.Graphics.Vulkan.ShaderCode" interpolates them straight into the
--   shader source (@${maxBindlessTextures}@, @${handleSlotTableSize}@) via
--   the @glsl@ quasiquoter, and Template Haskell's stage restriction means
--   an interpolated binding has to be defined in a DIFFERENT module than
--   the quote that splices it. The Haskell side and the GLSL side therefore
--   read the same definition: a divergence between them cannot be written
--   down, rather than merely being discouraged by a comment (#975).
--
--   These are the fixed upper bounds. The number of texture slots actually
--   allocated is separately clamped down by what the device reports
--   ("Engine.Graphics.Vulkan.Texture.System"), which may legitimately be
--   lower.
module Engine.Graphics.Vulkan.Texture.Limits
  ( maxBindlessTextures
  , handleSlotTableSize
  ) where

import UPrelude

-- | Size of the bindless texture array: the descriptor count of the
--   combined-image-sampler binding, and the @textures[]@ array length in
--   both bindless fragment shaders. An upper bound — 'createTextureSystem'
--   allocates the minimum of this and the device's usable
--   update-after-bind capacity.
maxBindlessTextures ∷ Word32
maxBindlessTextures = 16384

-- | Number of entries in the handle→slot table. The fragment shader
--   indexes it with a STABLE texture-handle id (not a recyclable slot),
--   so this MUST cover the handle-id space. Handle ids are dense and
--   monotonic from 0 ('Engine.Graphics.Vulkan.Texture.Handle.generateTextureHandle');
--   world-tile material / facemap handles are allocated at startup (low
--   ids), so they are always in range. A handle id beyond this cap
--   resolves to slot 0 (undefined) in the shader — a graceful degrade
--   that can only bite a transient texture in an extremely long session,
--   never a cached tile. Sizes the storage buffer and its zero-fill
--   ("Engine.Graphics.Vulkan.Texture.Bindless") and @HANDLE_TABLE_SIZE@
--   in both bindless fragment shaders. #286.
handleSlotTableSize ∷ Int
handleSlotTableSize = 65536

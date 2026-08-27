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
--   monotonic from 1 ('Engine.Asset.Manager.generateTextureHandle');
--   entry 0 belongs to 'Engine.Asset.Handle.missingTextureHandle' and is
--   held at slot 0 for the whole process lifetime (#1696).
--   World-tile material / facemap handles are allocated at startup (low
--   ids), so they are always in range.
--
--   The id space is FINITE and never recycled: the counter is monotonic
--   and nothing in the tree resets it, so a long-running process can
--   spend it. Past this cap the shader would resolve every id to slot 0
--   (the undefined checkerboard) while the engine reported the texture
--   loaded, so a shader-addressable registration for such an id is
--   REFUSED instead —
--   'Engine.Graphics.Vulkan.Texture.Handle.checkRegistrableHandle'
--   answers @TextureHandleUnrepresentable@ and the request settles on
--   the terminal failure #1690 established (#1699). The one exemption
--   is a @SlotOnly@ registration, whose slot never travels through this
--   table at all ("Engine.Graphics.Vulkan.Texture.DefaultFaceMap").
--
--   Sizes the storage buffer and its zero-fill
--   ("Engine.Graphics.Vulkan.Texture.Bindless") and @HANDLE_TABLE_SIZE@
--   in both bindless fragment shaders. #286.
handleSlotTableSize ∷ Int
handleSlotTableSize = 65536

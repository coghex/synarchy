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
--   These are exact sizes, not upper bounds to clamp down from. Nothing
--   allocates a smaller bindless binding than the shaders declare: a device
--   that cannot supply the whole descriptor count is rejected before the
--   texture system is built ("Engine.Graphics.Vulkan.Capability", #1689).
module Engine.Graphics.Vulkan.Texture.Limits
  ( maxBindlessTextures
  , handleSlotTableSize
  ) where

import UPrelude

-- | Size of the bindless texture array: the descriptor count of the
--   combined-image-sampler binding, and the @textures[]@ array length in
--   both bindless fragment shaders. Those two are the SAME number by
--   requirement, not by coincidence — both shaders index the array with
--   @nonuniformEXT@, so it is statically used at its declared size, and
--   without @runtimeDescriptorArray@ (which
--   "Engine.Graphics.Vulkan.Texture.Requirements" deliberately does not
--   require) the descriptor-set interface rule admits no binding smaller
--   than that. 'createTextureSystem' therefore builds this many descriptors
--   on every device it accepts, and a device that cannot supply them is
--   refused (#1689). Reserved slots — index 0, the undefined texture — are
--   indices INSIDE this count, not a subtraction from it.
maxBindlessTextures ∷ Word32
maxBindlessTextures = 16384

-- | Number of entries in the handle→slot table. The fragment shader
--   indexes it with a STABLE texture-handle id (not a recyclable slot),
--   so this MUST cover the handle-id space. Handle ids are dense and
--   monotonic from 1 ('Engine.Asset.Manager.generateTextureHandle');
--   entry 0 belongs to 'Engine.Asset.Handle.missingTextureHandle' and is
--   held at slot 0 for the whole process lifetime (#1696).
--   World-tile material / facemap handles are allocated at startup (low
--   ids), so they are always in range. A handle id beyond this cap
--   resolves to slot 0 (undefined) in the shader — a graceful degrade
--   that can only bite a transient texture in an extremely long session,
--   never a cached tile. Sizes the storage buffer and its zero-fill
--   ("Engine.Graphics.Vulkan.Texture.Bindless") and @HANDLE_TABLE_SIZE@
--   in both bindless fragment shaders. #286.
handleSlotTableSize ∷ Int
handleSlotTableSize = 65536

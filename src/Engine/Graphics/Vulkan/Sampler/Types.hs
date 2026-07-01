{-# LANGUAGE Strict, UnicodeSyntax, DataKinds #-}

-- | Sampler-cache types + pure config (no 'EngineM' — so 'Engine.Core.State'
--   can hold a 'SamplerCache' without an import cycle). The refcounted
--   operations live in "Engine.Graphics.Vulkan.Sampler.Cache".
module Engine.Graphics.Vulkan.Sampler.Types
  ( SamplerKind(..)
  , SamplerCache
  , emptySamplerCache
  , textureSamplerKind
  , samplerInfoFor
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Vulkan.Core10
import Vulkan.Zero

-- | The distinct sampler configurations the engine actually uses. The
--   undefined/fallback texture also samples NEAREST/REPEAT, so it shares
--   'SamplerTextureNearest'.
data SamplerKind
  = SamplerTextureNearest   -- ^ nearest filter, repeat address
  | SamplerTextureLinear    -- ^ linear filter, clamp-to-edge address
  | SamplerFont             -- ^ linear, clamp-to-edge, linear mipmap
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Live sampler + reference count, keyed by kind.
type SamplerCache = Map.Map SamplerKind (Sampler, Word32)

emptySamplerCache ∷ SamplerCache
emptySamplerCache = Map.empty

-- | Map the runtime texture filter to its sampler kind.
textureSamplerKind ∷ Filter → SamplerKind
textureSamplerKind FILTER_LINEAR = SamplerTextureLinear
textureSamplerKind _             = SamplerTextureNearest

-- | The 'SamplerCreateInfo' for a kind. Fixed defaults shared by every
--   kind; only filter/address/mipmap vary (the fields that actually
--   distinguish our samplers).
samplerInfoFor ∷ SamplerKind → SamplerCreateInfo '[]
samplerInfoFor k =
    let (flt, addr, mip) = case k of
          SamplerTextureNearest →
            (FILTER_NEAREST, SAMPLER_ADDRESS_MODE_REPEAT
            , SAMPLER_MIPMAP_MODE_NEAREST)
          SamplerTextureLinear →
            (FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            , SAMPLER_MIPMAP_MODE_NEAREST)
          SamplerFont →
            (FILTER_LINEAR, SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
            , SAMPLER_MIPMAP_MODE_LINEAR)
    in zero
        { magFilter = flt
        , minFilter = flt
        , addressModeU = addr
        , addressModeV = addr
        , addressModeW = addr
        , anisotropyEnable = False
        , maxAnisotropy = 1
        , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
        , unnormalizedCoordinates = False
        , compareEnable = False
        , compareOp = COMPARE_OP_ALWAYS
        , mipmapMode = mip
        , mipLodBias = 0
        , minLod = 0
        , maxLod = 0
        }

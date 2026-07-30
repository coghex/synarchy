{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Vertex
    ( getVertexBindingDescription
    , getVertexAttributeDescriptions
    ) where

import UPrelude
import qualified Data.Vector as V
import Vulkan.Core10
import Vulkan.Zero

-- | Get vertex binding description for pipeline creation
getVertexBindingDescription ∷ VertexInputBindingDescription
getVertexBindingDescription = zero
    { binding = 0
    , stride = 48  -- 2+2+4+1+1 floats + 2 uints = 48 bytes
    , inputRate = VERTEX_INPUT_RATE_VERTEX
    }

-- | Get vertex attribute descriptions for pipeline creation
getVertexAttributeDescriptions ∷ V.Vector VertexInputAttributeDescription
getVertexAttributeDescriptions = V.fromList
    [ zero  -- Position
        { location = 0
        , binding = 0
        , format = FORMAT_R32G32_SFLOAT
        , offset = 0
        }
    , zero  -- ^ TexCoord
        { location = 1
        , binding = 0
        , format = FORMAT_R32G32_SFLOAT
        , offset = 8
        }
    , zero  -- ^ Color
        { location = 2
        , binding = 0
        , format = FORMAT_R32G32B32A32_SFLOAT
        , offset = 16
        }
    , zero  -- ^ Atlas ID (texture index)
        { location = 3
        , binding = 0
        , format = FORMAT_R32_SFLOAT
        , offset = 32
        }
    , zero  -- ^ Face Map ID
        { location = 4
        , binding = 0
        , format = FORMAT_R32_SFLOAT
        , offset = 36
        }
    , zero  -- ^ Render flags (Word32 bitset, e.g. SELECTED_OUTLINE)
        { location = 5
        , binding = 0
        , format = FORMAT_R32_UINT
        , offset = 40
        }
    , zero  -- ^ Packed world (u,v) — #483 longitude-local day/night.
             -- Only the bindless world vertex shader declares/reads this;
             -- other pipelines (UI, font) leave it unused, like renderFlags.
        { location = 6
        , binding = 0
        , format = FORMAT_R32_UINT
        , offset = 44
        }
    ]

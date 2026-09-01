{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Vertex
    ( getVertexBindingDescription
    , getVertexAttributeDescriptions
    ) where

import qualified Data.Vector as V
import Vulkan.Core10
import Vulkan.Zero

-- | Get vertex binding description for pipeline creation
--
--   The literal @stride@ below MUST equal @vertexTotalSize@ in
--   "Engine.Graphics.Vulkan.Types.Vertex", which is what the @Storable
--   Vertex@ instance there reports from @sizeOf@. Nothing checks that
--   at compile time (#983).
getVertexBindingDescription ∷ VertexInputBindingDescription
getVertexBindingDescription = zero
    { binding = 0
    , stride = 56  -- = vertexTotalSize: 2+2+4+1+1 floats + 2 uints
                  --   + 2 signed ints (worldUV) + 1 uint
    , inputRate = VERTEX_INPUT_RATE_VERTEX
    }

-- | Get vertex attribute descriptions for pipeline creation
--
--   Each literal @offset@ below MUST equal the correspondingly named
--   constant in "Engine.Graphics.Vulkan.Types.Vertex"
--   (@vertexPositionOffset@, @vertexTexCoordOffset@, @vertexColorOffset@,
--   @vertexAtlasIdOffset@, @vertexFaceMapIdOffset@,
--   @vertexRenderFlagsOffset@, @vertexWorldUVOffset@,
--   @vertexSolarPageOffset@), which are exactly
--   the offsets the @Storable Vertex@ instance there peeks and pokes at.
--   Those constants are the authority; nothing checks the agreement at
--   compile time (#983).
getVertexAttributeDescriptions ∷ V.Vector VertexInputAttributeDescription
getVertexAttributeDescriptions = V.fromList
    [ zero  -- Position
        { location = 0
        , binding = 0
        , format = FORMAT_R32G32_SFLOAT
        , offset = 0   -- = vertexPositionOffset
        }
    , zero  -- TexCoord
        { location = 1
        , binding = 0
        , format = FORMAT_R32G32_SFLOAT
        , offset = 8   -- = vertexTexCoordOffset
        }
    , zero  -- Color
        { location = 2
        , binding = 0
        , format = FORMAT_R32G32B32A32_SFLOAT
        , offset = 16  -- = vertexColorOffset
        }
    , zero  -- Atlas ID (texture index)
        { location = 3
        , binding = 0
        , format = FORMAT_R32_SFLOAT
        , offset = 32  -- = vertexAtlasIdOffset
        }
    , zero  -- Face Map ID
        { location = 4
        , binding = 0
        , format = FORMAT_R32_SFLOAT
        , offset = 36  -- = vertexFaceMapIdOffset
        }
    , zero  -- Render flags (Word32 bitset, e.g. SELECTED_OUTLINE)
        { location = 5
        , binding = 0
        , format = FORMAT_R32_UINT
        , offset = 40  -- = vertexRenderFlagsOffset
        }
    , zero  -- World cylinder (u,v) — #483 longitude-local day/night,
             -- widened to two whole signed 32-bit components by #2019
             -- (eight bytes, which is why the solar page below sits at
             -- 52 rather than 48). Only the bindless world vertex shader
             -- declares/reads this; other pipelines (UI, font) leave it
             -- unused, like renderFlags.
        { location = 6
        , binding = 0
        , format = FORMAT_R32G32_SINT
        , offset = 44  -- = vertexWorldUVOffset
        }
    , zero  -- Solar page slot — #1869 per-page day/night attribution.
             -- 0 = none (the UBO's global sunAngle/circumference, which
             -- is what UI and generic scene sprites keep); n > 0 selects
             -- ubo.solarPages[n - 1]. Declared for every pipeline on this
             -- binding; only the bindless world vertex shader reads it.
        { location = 7
        , binding = 0
        , format = FORMAT_R32_UINT
        , offset = 52  -- = vertexSolarPageOffset
        }
    ]

{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.FloraQuads
    ( floraToQuad
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight
                  , worldLayer, applyFacing)
import World.Types
import World.Render.Textures (getVegFaceMapTexture)
import World.Flora.Types (FloraInstance(..))

-----------------------------------------------------------
-- Flora Instance → Quad
--
-- Similar to vegToQuad but positions using the sub-tile
-- float offsets (fiOffU, fiOffV) and the instance's own
-- z-slice (fiZ).
--
-- The texture handle is pre-resolved by the caller
-- (via resolveFloraTexture) so this function is pure
-- rendering geometry.
-----------------------------------------------------------

floraToQuad
    ∷ (TextureHandle → Int)     -- ^ lookupSlot
    → (TextureHandle → Float)   -- ^ lookupFmSlot
    → WorldTextures              -- ^ textures (for facemap)
    → CameraFacing               -- ^ camera facing
    → Int → Int                  -- ^ gx, gy (global tile coords)
    → FloraInstance              -- ^ the instance
    → TextureHandle              -- ^ resolved texture
    → Int → Int                  -- ^ zSlice, effectiveDepth
    → Float                      -- ^ tileAlpha (zoom fade)
    → Float                      -- ^ xOffset (world wrap)
    → Maybe SortableQuad
floraToQuad lookupSlot lookupFmSlot textures facing
            gx gy inst texHandle zSlice effDepth tileAlpha xOffset =
    let floraZ = fiZ inst

        -- Skip if below visible depth or above z-slice
        relativeZ = floraZ - zSlice
    in if floraZ > zSlice ∨ floraZ < (zSlice - effDepth)
       then Nothing
       else
        let -- Base screen position of the tile this instance sits on
            (rawX, rawY) = gridToScreen facing gx gy
            heightOffset = fromIntegral relativeZ * tileSideHeight

            -- Sub-tile offset: fiOffU/fiOffV are in (-0.5 .. 0.5) tile units
            -- Convert to screen pixels using tile dimensions
            subX = fiOffU inst * tileWidth
            subY = fiOffV inst * tileHeight * 0.5  -- iso half-height for V axis

            drawX = rawX + xOffset + subX
            drawY = rawY - heightOffset + subY

            -- Sort key: same tile sort, with tiny nudge above terrain (+0.0003)
            -- and a sub-nudge from V offset so overlapping flora sorts front-to-back
            (fa, fb) = applyFacing facing gx gy
            sortKey = fromIntegral (fa + fb)
                    + fromIntegral relativeZ * 0.001
                    + 0.0003
                    + fiOffV inst * 0.00005

            actualSlot = lookupSlot texHandle
            -- Flora uses the veg facemap (top face only, no cube sides)
            fmHandle = getVegFaceMapTexture textures 0  -- flat, no slope
            fmSlot = lookupFmSlot fmHandle

            -- Depth haze (same as vegToQuad)
            depth = zSlice - floraZ
            fadeRange = max 1 effDepth
            fadeT = clamp01 (fromIntegral depth / fromIntegral fadeRange)
            hazeT = fadeT * fadeT * 0.6
            r = 1.0 * (1.0 - hazeT) + 0.72 * hazeT
            g = 1.0 * (1.0 - hazeT) + 0.85 * hazeT
            b = 1.0 * (1.0 - hazeT) + 0.95 * hazeT

            tint = Vec4 r g b tileAlpha

            v0 = Vertex (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            v1 = Vertex (Vec2 (drawX + tileWidth) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight))
                         (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            v3 = Vertex (Vec2 drawX (drawY + tileHeight))
                         (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot

        in Just SortableQuad
            { sqSortKey = sortKey
            , sqV0      = v0
            , sqV1      = v1
            , sqV2      = v2
            , sqV3      = v3
            , sqTexture = texHandle
            , sqLayer   = worldLayer
            }

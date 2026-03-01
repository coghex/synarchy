{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.FloraQuads
    ( floraToQuad
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
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
-- Scales the quad to match the actual texture dimensions.
-- A 32×32 texture renders at exactly 1 tile.
-- A 32×64 texture renders 1 tile wide, 2 tiles tall,
-- anchored at the bottom so the base sits on the surface.
-----------------------------------------------------------

-- | Base tile size that textures are measured against.
--   A texture this size renders as exactly one tile.
baseTileW ∷ Float
baseTileW = 32.0

baseTileH ∷ Float
baseTileH = 32.0

floraToQuad
    ∷ (TextureHandle → Int)
    → (TextureHandle → Float)
    → WorldTextures
    → CameraFacing
    → Int → Int                     -- ^ gx, gy
    → FloraInstance
    → TextureHandle                 -- ^ resolved texture
    → Int → Int                     -- ^ zSlice, effectiveDepth
    → Float                         -- ^ tileAlpha
    → Float                         -- ^ xOffset
    → HM.HashMap TextureHandle (Int, Int)  -- ^ texture sizes
    → Maybe SortableQuad
floraToQuad lookupSlot lookupFmSlot textures facing
            gx gy inst texHandle zSlice effDepth tileAlpha xOffset texSizes =
    let floraZ = fiZ inst
        relativeZ = floraZ - zSlice
    in if floraZ > zSlice ∨ floraZ < (zSlice - effDepth)
       then Nothing
       else
        let -- Look up actual texture dimensions, default to one tile
            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)

            -- Scale relative to the base tile size
            scaleX = texW / baseTileW
            scaleY = texH / baseTileH

            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            -- Extra height above the base tile (for tall sprites like trees)
            extraH = quadH - tileHeight

            -- Base screen position of the tile
            (rawX, rawY) = gridToScreen facing gx gy
            heightOffset = fromIntegral relativeZ * tileSideHeight

            -- Sub-tile offset
            subX = fiOffU inst * tileWidth
            subY = fiOffV inst * tileHeight * 0.5

            -- Center horizontally on the tile, anchor at bottom
            drawX = rawX + xOffset + subX + (tileWidth - quadW) * 0.5
            -- Shift upward by extraH so the bottom of the sprite
            -- sits where a normal tile would
            drawY = rawY - heightOffset + subY - extraH

            (fa, fb) = applyFacing facing gx gy
            sortKey = fromIntegral (fa + fb)
                    + fromIntegral relativeZ * 0.001
                    + 0.0003
                    + fiOffV inst * 0.00005

            actualSlot = lookupSlot texHandle
            fmSlot = lookupFmSlot (TextureHandle 0)

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
            v1 = Vertex (Vec2 (drawX + quadW) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH))
                         (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            v3 = Vertex (Vec2 drawX (drawY + quadH))
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

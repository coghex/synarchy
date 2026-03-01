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
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, applyFacing, GridConfig(..), defaultGridConfig)
import World.Types
import World.Flora.Types (FloraInstance(..))

-- | Tile pixel dimensions — must match GridConfig.
baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)   -- 96

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)  -- 64

floraToQuad
    ∷ (TextureHandle → Int)
    → (TextureHandle → Float)
    → WorldTextures
    → CameraFacing
    → Int → Int
    → FloraInstance
    → TextureHandle
    → Int → Int
    → Float
    → Float
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
floraToQuad lookupSlot lookupFmSlot textures facing
            gx gy inst texHandle zSlice effDepth tileAlpha xOffset texSizes =
    let floraZ = fiZ inst
        relativeZ = floraZ - zSlice
    in if floraZ > zSlice ∨ floraZ < (zSlice - effDepth)
       then Nothing
       else
        let -- Look up actual texture dimensions, default to tile size
            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)

            -- Scale relative to the actual tile pixel size
            scaleX = texW / baseTileW
            scaleY = texH / baseTileH

            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            -- Base screen position of the tile
            (rawX, rawY) = gridToScreen facing gx gy
            heightOffset = fromIntegral relativeZ * tileSideHeight

            -- Sub-tile offset in isometric space
            subX = (fiOffU inst - fiOffV inst) * tileHalfWidth
            subY = (fiOffU inst + fiOffV inst) * tileHalfDiamondHeight

            -- The trunk base in the texture is a circle of diameter
            -- fiBaseWidth pixels. The ground contact center is
            -- baseRadius pixels up from the bottom of the texture.
            baseRadius = fiBaseWidth inst * 0.5 / baseTileH * tileHeight

            -- Center horizontally on the tile
            drawX = rawX + xOffset + subX + (tileWidth - quadW) * 0.5

            -- Anchor: the point (baseRadius up from quad bottom)
            -- sits at the tile diamond center Y.
            --   tile center Y = rawY - heightOffset + tileHalfDiamondHeight
            --   anchor Y      = drawY + quadH - baseRadius
            -- Solve for drawY:
            drawY = rawY - heightOffset + subY
                  + tileHalfDiamondHeight - quadH + baseRadius

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

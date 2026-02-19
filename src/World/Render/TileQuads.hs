{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.TileQuads
    ( tileToQuad
    , blankTileToQuad
    , oceanTileToQuad
    , lavaTileToQuad
    , freshwaterTileToQuad
    ) where

import UPrelude
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import World.Fluids (FluidCell(..), FluidType(..), seaLevel)
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight, worldLayer, applyFacing)
import World.Types
import World.Render.Textures (getTileTexture, getTileFaceMapTexture)

-----------------------------------------------------------
-- Convert Tile to Quad
-----------------------------------------------------------

tileToQuad lookupSlot lookupFmSlot textures facing worldX worldY worldZ tile zSlice effDepth tileAlpha xOffset mFluid chunkHasFluid =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
        texHandle = getTileTexture textures (tileType tile)
        actualSlot = lookupSlot texHandle
        fmHandle = getTileFaceMapTexture textures (tileType tile) (tileSlopeId tile)
        fmSlot = lookupFmSlot fmHandle

        depth = zSlice - worldZ
        fadeRange = max 1 effDepth
        fadeT = clamp01 (fromIntegral depth / fromIntegral fadeRange)
        brightness = clamp01 (1.0 - fadeT * 0.15)

        hazeT = fadeT * fadeT * 0.6
        hazeR = 0.72 ∷ Float
        hazeG = 0.85 ∷ Float
        hazeB = 0.95 ∷ Float

        underwaterDepth = case mFluid of
            Just fc
                | fcType fc ≡ Ocean ∧ worldZ < fcSurface fc → fcSurface fc - worldZ
            _ | chunkHasFluid ∧ worldZ < seaLevel → seaLevel - worldZ
            _ → 0

        (tintR, tintG, tintB, finalAlpha) = if underwaterDepth > 0
            then
                let t = clamp01 (fromIntegral underwaterDepth / 30.0)
                    r = 0.6 - t * 0.4
                    g = 0.7 - t * 0.4
                    b = 0.9 - t * 0.3
                in (r, g, b, tileAlpha)
            else
                let r = brightness * (1.0 - hazeT) + hazeR * hazeT
                    g = brightness * (1.0 - hazeT) + hazeG * hazeT
                    b = brightness * (1.0 - hazeT) + hazeB * hazeT
                in (r, g, b, tileAlpha)

        tint = Vec4 tintR tintG tintB finalAlpha

        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Blank Tile Quad
-----------------------------------------------------------

blankTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY worldZ zSlice tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
        texHandle = wtBlankTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        depth = zSlice - worldZ
        fadeT = clamp01 (fromIntegral depth / 50.0)
        hazeT = fadeT * fadeT * 0.6
        r = 1.0 * (1.0 - hazeT) + 0.72 * hazeT
        g = 1.0 * (1.0 - hazeT) + 0.85 * hazeT
        b = 1.0 * (1.0 - hazeT) + 0.95 * hazeT

        tint = Vec4 r g b tileAlpha
        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Ocean Surface Tile Quad
-----------------------------------------------------------

oceanTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY fluidZ zSlice effDepth tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = fluidZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
                + 0.0005

        texHandle = wtOceanTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        finalAlpha = tileAlpha
        tint = Vec4 0.7 0.8 1.0 finalAlpha

        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

lavaTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY fluidZ zSlice effDepth tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = fluidZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
                + 0.0005
        texHandle = wtLavaTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)
        finalAlpha = tileAlpha
        tint = Vec4 1.0 0.6 0.2 finalAlpha
        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Freshwater (River/Lake) Surface Tile Quad
-----------------------------------------------------------

freshwaterTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY
                     fluidZ fluidType zSlice effDepth tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = fluidZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
                + 0.0005

        texHandle = wtOceanTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        finalAlpha = tileAlpha

        tint = case fluidType of
            Lake  → Vec4 0.5 0.8 0.9 finalAlpha
            River → Vec4 0.6 0.85 0.95 finalAlpha
            _     → Vec4 0.7 0.8 1.0 finalAlpha

        v0 = Vertex (Vec2 drawX drawY)
                     (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)
                     (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight))
                     (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))
                     (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

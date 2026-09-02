{-# LANGUAGE Strict #-}
module World.Render.FloraQuads
    ( floraToQuad
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..), mkVertexWorld
                                           , tileWorldUV, noFaceMapVertexId)
import World.Grid (worldLayer)
import World.Render.FloraProjection
    (FloraGeom(..), floraGeom, floraVisibleInSlice)
import World.Types

-- | One flora instance's world quad.
--
--   Flora has no directional face map of its own, so every vertex it
--   emits carries 'noFaceMapVertexId' and the shader shades it with
--   @fragDefaultFaceMapSlot@. It deliberately takes no face-map lookup:
--   the id it used to pass — a literal zero handle through the identity
--   'lookupFmSlot' — resolved to whatever bindless slot the FIRST
--   texture allocated in the process happened to take, because handle 0
--   was an ordinary allocatable id (#1696).
floraToQuad
    ∷ (TextureHandle → Int)
    → WorldTextures
    → CameraFacing
    → Int → Int
    → FloraInstance
    → TextureHandle
    → Int → Int
    → Float
    → (Float, Float)
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
floraToQuad lookupSlot _textures facing
            gx gy inst texHandle zSlice effDepth tileAlpha wrapOff texSizes =
    if not (floraVisibleInSlice zSlice effDepth inst)
       then Nothing
       else
        -- #1856: placement, size, anchor and painter depth all come
        -- from the ONE shared projection boundary, so the selection
        -- oracle and the designation marker cannot drift from what is
        -- painted here.
        let geom = floraGeom facing gx gy inst texHandle texSizes zSlice wrapOff
            drawX = fgDrawX geom
            drawY = fgDrawY geom
            quadW = fgQuadW geom
            quadH = fgQuadH geom
            sortKey = fgSortKey geom

            actualSlot = lookupSlot texHandle
            fmSlot = noFaceMapVertexId

            depth = zSlice - fiZ inst
            fadeRange = max 1 effDepth
            fadeT = clamp01 (fromIntegral depth / fromIntegral fadeRange)
            hazeT = fadeT * fadeT * 0.6
            r = 1.0 * (1.0 - hazeT) + 0.72 * hazeT
            g = 1.0 * (1.0 - hazeT) + 0.85 * hazeT
            b = 1.0 * (1.0 - hazeT) + 0.95 * hazeT

            tint = Vec4 r g b tileAlpha
            wuv = tileWorldUV gx gy

            v0 = mkVertexWorld wuv (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            v1 = mkVertexWorld wuv (Vec2 (drawX + quadW) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            v2 = mkVertexWorld wuv (Vec2 (drawX + quadW) (drawY + quadH))
                         (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            v3 = mkVertexWorld wuv (Vec2 drawX (drawY + quadH))
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

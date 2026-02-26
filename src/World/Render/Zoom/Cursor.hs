{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Zoom-level cursor interaction: hover highlight, selection overlay,
--   and pixel-to-chunk-origin conversion.
module World.Render.Zoom.Cursor
    ( makeCursorQuad
    , makeSelectQuad
    , makeHoverQuad
    , pixelToChunkOrigin
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, IORef)
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import World.Types
import World.Grid (gridToWorld, worldToGrid, zoomMapLayer)

-- | Convert pixel coords to chunk-aligned grid origin, or Nothing if off-map
pixelToChunkOrigin ∷ CameraFacing → Camera2D → Int → Int → Int → Int → Int
                   → Int → Int → Maybe (Int, Int)
pixelToChunkOrigin facing camera winW winH fbW fbH worldSize pixX pixY =
    let aspect = fromIntegral fbW / fromIntegral fbH
        zoom   = camZoom camera
        viewW  = zoom * aspect
        viewH  = zoom
        normX  = fromIntegral pixX / fromIntegral winW
        normY  = fromIntegral pixY / fromIntegral winH
        viewX  = (normX * 2.0 - 1.0) * viewW
        viewY  = (normY * 2.0 - 1.0) * viewH
        (camX, camY) = camPosition camera
        worldX = viewX + camX
        worldY = viewY + camY
        (gx, gy) = worldToGrid facing worldX worldY
        halfTiles = (worldSize * chunkSize) `div` 2
        v = gx + gy
    in if abs v > halfTiles then Nothing
       else let chunkX = if gx >= 0 then gx `div` chunkSize
                         else -(((-gx) + chunkSize - 1) `div` chunkSize)
                chunkY = if gy >= 0 then gy `div` chunkSize
                         else -(((-gy) + chunkSize - 1) `div` chunkSize)
            in Just (chunkX * chunkSize, chunkY * chunkSize)

makeCursorQuad ∷ CameraFacing → Camera2D → Int → Int → Int → Int → Int
              → IORef CursorState
              → (TextureHandle → Int) → Float
              → IO (V.Vector SortableQuad)
makeCursorQuad facing camera winW winH fbW fbH worldSize csRef lookupSlot defFmSlot = do
    cs ← readIORef csRef

    let hoverChunk = case zoomCursorPos cs of
            Nothing → Nothing
            Just (pixX, pixY) →
                pixelToChunkOrigin facing camera winW winH fbW fbH worldSize pixX pixY

    cs' ← if zoomSelectNow cs
           then do let newCs = cs { zoomSelectNow = False
                                  , zoomSelectedPos = hoverChunk }
                   writeIORef csRef newCs
                   return newCs
           else return cs

    let selectQuad = makeSelectQuad facing worldSize cs' lookupSlot defFmSlot
        hoverQuad  = case (hoverChunk, zoomHoverTexture cs') of
            (Just (baseGX, baseGY), Just hoverTexture) →
                emitCursorQuad facing baseGX baseGY hoverTexture lookupSlot defFmSlot 0.6 100
            _ → V.empty

    return $ selectQuad <> hoverQuad

makeSelectQuad ∷ CameraFacing → Int → CursorState
               → (TextureHandle → Int) → Float → V.Vector SortableQuad
makeSelectQuad facing _worldSize cs lookupSlot defFmSlot =
    case (zoomSelectedPos cs, zoomCursorTexture cs) of
        (Just (baseGX, baseGY), Just selectTexture) →
            emitCursorQuad facing baseGX baseGY selectTexture lookupSlot defFmSlot 0.8 99
        _ → V.empty

makeHoverQuad ∷ CameraFacing → Camera2D → Int → Int → Int → Int → Int
              → CursorState → (TextureHandle → Int) → Float → V.Vector SortableQuad
makeHoverQuad facing camera winW winH fbW fbH worldSize cs lookupSlot defFmSlot =
    case zoomCursorPos cs of
        Nothing → V.empty
        Just (pixX, pixY) → case zoomHoverTexture cs of
            Nothing → V.empty
            Just hoverTexture →
                case pixelToChunkOrigin facing camera winW winH fbW fbH worldSize pixX pixY of
                    Nothing → V.empty
                    Just (baseGX, baseGY) →
                        emitCursorQuad facing baseGX baseGY hoverTexture lookupSlot defFmSlot 0.6 100

-- | Shared helper to avoid the quad-emission boilerplate being repeated 3×.
emitCursorQuad ∷ CameraFacing → Int → Int → TextureHandle
               → (TextureHandle → Int) → Float → Float → Float
               → V.Vector SortableQuad
emitCursorQuad facing baseGX baseGY tex lookupSlot defFmSlot alpha sortKey =
    let (x0, y0) = gridToWorld facing baseGX baseGY
        (x1, y1) = gridToWorld facing (baseGX + chunkSize) baseGY
        (x2, y2) = gridToWorld facing baseGX (baseGY + chunkSize)
        (x3, y3) = gridToWorld facing (baseGX + chunkSize) (baseGY + chunkSize)
        drawX = min x0 (min x1 (min x2 x3))
        drawY = min y0 (min y1 (min y2 y3))
        w     = max x0 (max x1 (max x2 x3)) - drawX
        h     = max y0 (max y1 (max y2 y3)) - drawY
        slot  = fromIntegral (lookupSlot tex)
        color = Vec4 1.0 1.0 1.0 alpha
    in V.singleton $ SortableQuad
        { sqSortKey = sortKey
        , sqV0 = Vertex (Vec2 drawX drawY)             (Vec2 0 0) color slot defFmSlot
        , sqV1 = Vertex (Vec2 (drawX + w) drawY)       (Vec2 1 0) color slot defFmSlot
        , sqV2 = Vertex (Vec2 (drawX + w) (drawY + h)) (Vec2 1 1) color slot defFmSlot
        , sqV3 = Vertex (Vec2 drawX (drawY + h))        (Vec2 0 1) color slot defFmSlot
        , sqTexture = tex
        , sqLayer   = zoomMapLayer
        }

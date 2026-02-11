{-# LANGUAGE Strict #-}
module World.ZoomMap
    ( generateZoomMapQuads
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import qualified Data.Text as T
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Monad (EngineM)
import Engine.Core.Log (logDebug, LogCategory(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Material (MaterialId(..))
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalX)
import World.Generate (chunkSize, cameraChunkCoord)
import World.Grid (tileHalfWidth, tileHalfDiamondHeight, gridToWorld,
                   chunkWorldWidth, chunkWorldDiamondHeight, zoomMapLayer,
                   zoomFadeStart, zoomFadeEnd)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Generate Zoom Map Quads
-----------------------------------------------------------

generateZoomMapQuads :: EngineM ε σ (V.Vector SortableQuad)
generateZoomMapQuads = do
    env <- ask
    camera <- liftIO $ readIORef (cameraRef env)
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    worldManager <- liftIO $ readIORef (worldManagerRef env)

    let zoom = camZoom camera
        zoomAlpha = clamp01 ((zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    if zoomAlpha <= 0.001
        then return V.empty
        else do
            quads <- forM (wmVisible worldManager) $ \pageId ->
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState -> renderZoomChunks env worldState camera
                                         fbW fbH zoomAlpha
                    Nothing         -> return V.empty
            return $ V.concat quads

-----------------------------------------------------------
-- Screen-Space View Bounds
-----------------------------------------------------------

data ZoomViewBounds = ZoomViewBounds
    { zvLeft   :: !Float
    , zvRight  :: !Float
    , zvTop    :: !Float
    , zvBottom :: !Float
    }

computeZoomViewBounds :: Camera2D -> Int -> Int -> ZoomViewBounds
computeZoomViewBounds camera fbW fbH =
    let (cx, cy) = camPosition camera
        zoom = camZoom camera
        aspect = fromIntegral fbW / fromIntegral fbH
        halfW = zoom * aspect
        halfH = zoom
        padX = chunkWorldWidth * 2.0
        padY = chunkWorldDiamondHeight * 2.0
    in ZoomViewBounds
        { zvLeft   = cx - halfW - padX
        , zvRight  = cx + halfW + padX
        , zvTop    = cy - halfH - padY
        , zvBottom = cy + halfH + padY
        }

isChunkInView :: ZoomViewBounds -> Float -> Float -> Bool
isChunkInView vb drawX drawY =
    let right  = drawX + chunkWorldWidth
        bottom = drawY + chunkWorldDiamondHeight
    in not (right  < zvLeft vb
         || drawX  > zvRight vb
         || bottom < zvTop vb
         || drawY  > zvBottom vb)

-----------------------------------------------------------
-- Render Zoom Chunks
-----------------------------------------------------------

renderZoomChunks :: EngineEnv -> WorldState -> Camera2D
                 -> Int -> Int -> Float
                 -> EngineM ε σ (V.Vector SortableQuad)
renderZoomChunks env worldState camera fbW fbH zoomAlpha = do
    mParams <- liftIO $ readIORef (wsGenParamsRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)

    case mParams of
        Nothing -> return V.empty
        Just params -> do
            let seed = wgpSeed params
                worldSize = wgpWorldSize params
                plates = generatePlates seed worldSize (wgpPlateCount params)

                vb = computeZoomViewBounds camera fbW fbH

                halfSize = worldSize `div` 2
                worldChunks = worldSize

                -- Find which chunk the camera is in (in grid space)
                (camX, camY) = camPosition camera
                camChunk = cameraChunkCoord camX camY
                ChunkCoord camCX camCY = camChunk

                -- How far we need to look in chunk-grid space to fill the screen.
                -- In isometric, screen-X = (gx-gy)*tileHalfWidth, screen-Y = (gx+gy)*tileHalfDiamondHeight
                -- A step of 1 chunk in grid-X changes screen-X by chunkSize*tileHalfWidth
                -- and screen-Y by chunkSize*tileHalfDiamondHeight.
                -- To cover the screen we need radius in BOTH grid axes.
                zoom = camZoom camera
                aspect = fromIntegral fbW / fromIntegral fbH
                halfW = zoom * aspect
                halfH = zoom
                -- Each chunk step in grid-X adds tileHalfWidth*chunkSize to screen-X
                -- and tileHalfDiamondHeight*chunkSize to screen-Y.
                -- To be safe, compute radius from the larger of the two projections.
                chunkStepScreenX = fromIntegral chunkSize * tileHalfWidth
                chunkStepScreenY = fromIntegral chunkSize * tileHalfDiamondHeight
                -- We need enough chunks in each grid axis to cover both screen axes
                radiusX = ceiling ((halfW + halfH) / chunkStepScreenX) + 2
                radiusY = ceiling ((halfW + halfH) / chunkStepScreenY) + 2

                -- Wrap a chunk X coordinate into [-halfSize, halfSize)
                wrapCX cx = ((cx + halfSize) `mod` worldChunks + worldChunks)
                            `mod` worldChunks - halfSize

                inBoundsY cy = cy >= -halfSize && cy < halfSize

            -- Iterate chunks in a radius around the camera chunk.
            -- Use RAW (unwrapped) coords for screen position so it's continuous.
            -- Use WRAPPED coords for world data lookups.
            let quadsData =
                    [ (wrappedCX, ccy, texHandle, drawX, drawY)
                    | dx <- [-radiusX .. radiusX]
                    , dy <- [-radiusY .. radiusY]
                    , let rawCX = camCX + dx
                          ccy   = camCY + dy
                    , inBoundsY ccy
                    , let wrappedCX = wrapCX rawCX
                          -- Screen position uses RAW chunk X (continuous, no seam)
                          midGX_raw = rawCX * chunkSize + chunkSize `div` 2
                          midGY     = ccy * chunkSize + chunkSize `div` 2
                          (wcx, wcy) = gridToWorld midGX_raw midGY
                          drawX = wcx - chunkWorldWidth / 2.0
                          drawY = wcy
                    , isChunkInView vb drawX drawY
                    , let -- World data uses WRAPPED chunk X
                          midGX_wrapped = wrappedCX * chunkSize + chunkSize `div` 2
                    , not (isBeyondGlacier worldSize midGX_wrapped midGY)
                    , let (elev, mat) = elevationAtGlobal seed plates worldSize midGX_wrapped midGY
                          texHandle = getZoomTexture textures (unMaterialId mat) elev
                    ]

            quads <- forM quadsData $ \(ccx, ccy, texHandle, drawX, drawY) ->
                chunkToZoomQuad texHandle drawX drawY zoomAlpha ccx ccy

            return $ V.fromList quads

-----------------------------------------------------------
-- Chunk to Zoom Quad
-----------------------------------------------------------

chunkToZoomQuad :: TextureHandle
               -> Float -> Float -> Float
               -> Int -> Int
               -> EngineM ε σ SortableQuad
chunkToZoomQuad texHandle drawX drawY alpha ccx ccy = do
    gs <- gets graphicsState
    let actualSlot = case textureSystem gs of
          Just bindless -> getTextureSlotIndex texHandle bindless
          Nothing       -> 0

        fmSlot = fromIntegral (defaultFaceMapSlot gs)

        tint = Vec4 1.0 1.0 1.0 alpha

        sortKey = fromIntegral (ccx + ccy)

        w = chunkWorldWidth
        h = chunkWorldDiamondHeight

        vertices = V.fromList
            [ Vertex (Vec2 drawX drawY)               (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + w) drawY)          (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + w) (drawY + h))    (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX drawY)                (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + w) (drawY + h))    (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX (drawY + h))          (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
            ]

    return $ SortableQuad
        { sqSortKey  = sortKey
        , sqVertices = vertices
        , sqTexture  = texHandle
        , sqLayer    = zoomMapLayer
        }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

getZoomTexture :: WorldTextures -> Word8 -> Int -> TextureHandle
getZoomTexture textures 250 _ = wtZoomGlacier textures
getZoomTexture textures _mat elev
    | elev < -100 = wtZoomOcean textures
getZoomTexture textures 1 _ = wtZoomGranite textures
getZoomTexture textures 2 _ = wtZoomDiorite textures
getZoomTexture textures 3 _ = wtZoomGabbro textures
getZoomTexture textures _ _ = wtZoomGranite textures

clamp01 :: Float -> Float
clamp01 x
    | x < 0    = 0
    | x > 1    = 1
    | otherwise = x

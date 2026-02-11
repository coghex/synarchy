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
import World.Generate (chunkSize)
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
        -- Padding: one extra chunk width/height for partial visibility
        padX = chunkWorldWidth
        padY = chunkWorldDiamondHeight
    in ZoomViewBounds
        { zvLeft   = cx - halfW - padX
        , zvRight  = cx + halfW + padX
        , zvTop    = cy - halfH - padY
        , zvBottom = cy + halfH + padY
        }

-- | Check if a chunk diamond overlaps the screen-space view bounds.
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

                -- Wrap chunk X coordinate
                wrapCX cx = ((cx + halfSize) `mod` (halfSize * 2) + (halfSize * 2))
                            `mod` (halfSize * 2) - halfSize

                -- Iterate all Y chunks, but for X we need to figure out
                -- which wrapped X range is visible. Simplest: iterate all
                -- X chunks and let the screen-space visibility test cull.
                allCoords = [ (ccx, ccy)
                            | ccx <- [-halfSize .. halfSize - 1]
                            , ccy <- [-halfSize .. halfSize - 1]
                            ]

            let quadsData = [ (ccx, ccy, texHandle, drawX, drawY)
                            | (ccx, ccy) <- allCoords
                            , let midGX = ccx * chunkSize + chunkSize `div` 2
                                  midGY = ccy * chunkSize + chunkSize `div` 2
                                  (wcx, wcy) = gridToWorld midGX midGY
                                  drawX = wcx - chunkWorldWidth / 2.0
                                  drawY = wcy
                            , isChunkInView vb drawX drawY
                            , not (isBeyondGlacier worldSize midGX midGY)
                            , let wrappedGX = wrapGlobalX worldSize midGX
                                  (elev, mat) = elevationAtGlobal seed plates worldSize wrappedGX midGY
                                  texHandle = getZoomTexture textures (unMaterialId mat) elev
                            ]
                worldScreenWidth = fromIntegral (worldSize * chunkSize) * tileHalfWidth

                 -- For each chunk in view, also consider the
                 -- wrapped neighbors on the left and right edges of the world.
                 -- This handles the case where the camera is near
                 -- the edge and chunks from the opposite edge should be visible.
                allQuadsData = quadsData
                    ++ [ (ccx, ccy, texHandle, drawX + worldScreenWidth, drawY)
                       | (ccx, ccy, texHandle, drawX, drawY) <- quadsData
                       , isChunkInView vb (drawX + worldScreenWidth) drawY
                       ]
                    ++ [ (ccx, ccy, texHandle, drawX - worldScreenWidth, drawY)
                       | (ccx, ccy, texHandle, drawX, drawY) <- quadsData
                       , isChunkInView vb (drawX - worldScreenWidth) drawY
                       ]
            quads <- forM allQuadsData $ \(ccx, ccy, texHandle, drawX, drawY) ->
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

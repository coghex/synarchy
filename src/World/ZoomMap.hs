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
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal)
import World.Generate (chunkSize, chunkToGlobal)
import World.Grid (tileHalfWidth, tileHalfDiamondHeight, gridToWorld,
                   chunkWorldWidth, chunkWorldDiamondHeight, zoomMapLayer,
                   zoomFadeStart, zoomFadeEnd)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Generate Zoom Map Quads
-----------------------------------------------------------

-- | Produce one diamond quad per visible chunk, colored by
--   the dominant material at the chunk center.
--   Alpha is controlled by zoomAlpha (0 = invisible, 1 = opaque).
generateZoomMapQuads :: EngineM ε σ (V.Vector SortableQuad)
generateZoomMapQuads = do
    env <- ask
    camera <- liftIO $ readIORef (cameraRef env)
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    worldManager <- liftIO $ readIORef (worldManagerRef env)

    let zoom = camZoom camera
        -- Compute zoom map alpha: 0 below fadeStart, 1 above fadeEnd
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

                -- Determine which chunks are in view at this zoom level
                (cx, cy) = camPosition camera
                zoom = camZoom camera
                aspect = fromIntegral fbW / fromIntegral fbH
                halfW = zoom * aspect
                halfH = zoom

                -- How many chunks fit in the view?
                -- Each chunk is chunkSize tiles wide in grid space.
                -- In screen space, roughly chunkWorldWidth across.
                -- Add padding for partial visibility
                chunkRadius = ceiling (halfW / chunkWorldWidth) + 2

                -- Camera position in grid coords, then chunk coords
                camGX = round (cx / tileHalfWidth) :: Int
                camGY = round (cy / tileHalfDiamondHeight) :: Int
                camCX = camGX `div` chunkSize
                camCY = camGY `div` chunkSize

                halfSize = worldSize `div` 2

                -- All chunk coords in view
                visibleCoords = [ (ccx, ccy)
                                | dx <- [-chunkRadius .. chunkRadius]
                                , dy <- [-chunkRadius .. chunkRadius]
                                , let ccx = camCX + dx
                                      ccy = camCY + dy
                                , ccx >= -halfSize && ccx < halfSize
                                , ccy >= -halfSize && ccy < halfSize
                                ]

            -- Build quads for each visible chunk
            quads <- forM visibleCoords $ \(ccx, ccy) -> do
                let -- Sample the center tile of this chunk
                    centerLX = chunkSize `div` 2
                    centerLY = chunkSize `div` 2
                    (centerGX, centerGY) = ( ccx * chunkSize + centerLX
                                           , ccy * chunkSize + centerLY )
                    (elev, mat) = elevationAtGlobal seed plates centerGX centerGY

                    -- Pick texture based on material and elevation
                    texHandle = getZoomTexture textures (unMaterialId mat) elev

                    -- Chunk position: use the grid coords of corner (0,0) of this chunk
                    (chunkGX, chunkGY) = (ccx * chunkSize, ccy * chunkSize)
                    -- The center of the chunk diamond in grid space
                    (midGX, midGY) = ( chunkGX + chunkSize `div` 2
                                     , chunkGY + chunkSize `div` 2 )
                    -- World-space center of the chunk diamond
                    (wcx, wcy) = gridToWorld midGX midGY
                    -- Quad top-left
                    drawX = wcx - chunkWorldWidth / 2.0
                    drawY = wcy

                chunkToZoomQuad env texHandle drawX drawY zoomAlpha ccx ccy

            return $ V.fromList quads

-----------------------------------------------------------
-- Chunk to Zoom Quad
-----------------------------------------------------------

chunkToZoomQuad :: EngineEnv -> TextureHandle
               -> Float -> Float -> Float
               -> Int -> Int
               -> EngineM ε σ SortableQuad
chunkToZoomQuad _env texHandle drawX drawY alpha ccx ccy = do
    gs <- gets graphicsState
    let actualSlot = case textureSystem gs of
          Just bindless -> getTextureSlotIndex texHandle bindless
          Nothing       -> 0

        -- No face map for flat diamond sprites — use slot 0 (default)
        fmSlot = fromIntegral (defaultFaceMapSlot gs)

        tint = Vec4 1.0 1.0 1.0 alpha

        -- Sort key: chunk coordinates for back-to-front
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

-- | Pick zoom texture based on material and elevation.
--   Ocean plates (negative elevation) get the ocean texture.
getZoomTexture :: WorldTextures -> Word8 -> Int -> TextureHandle
getZoomTexture textures _mat elev
    | elev < -100 = wtZoomOcean textures
getZoomTexture textures 1 _ = wtZoomGranite textures
getZoomTexture textures 2 _ = wtZoomDiorite textures
getZoomTexture textures 3 _ = wtZoomGabbro textures
getZoomTexture textures _ _ = wtZoomGranite textures  -- fallback

clamp01 :: Float -> Float
clamp01 x
    | x < 0    = 0
    | x > 1    = 1
    | otherwise = x

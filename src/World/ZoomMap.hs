{-# LANGUAGE Strict #-}
module World.ZoomMap
    ( generateZoomMapQuads
    , buildZoomCache
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Monad (EngineM)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Material (MaterialId(..))
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier)
import World.Generate (chunkSize)
import World.Grid (tileHalfWidth, tileHalfDiamondHeight, gridToWorld,
                   chunkWorldWidth, chunkWorldDiamondHeight, zoomMapLayer,
                   zoomFadeStart, zoomFadeEnd)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Build Zoom Cache (called once at world init)
-----------------------------------------------------------

-- | Pre-compute all zoom map entries. Called once when the
--   world is created. Returns a vector of entries for every
--   visible (non-beyond-glacier) chunk in the world.
buildZoomCache :: WorldGenParams -> V.Vector ZoomChunkEntry
buildZoomCache params =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = generatePlates seed worldSize (wgpPlateCount params)
        halfSize = worldSize `div` 2

        entries =
            [ ZoomChunkEntry
                { zceChunkX   = ccx
                , zceChunkY   = ccy
                , zceDrawX    = drawX
                , zceDrawY    = drawY
                , zceTexIndex = unMaterialId mat
                , zceElev     = elev
                }
            | ccx <- [-halfSize .. halfSize - 1]
            , ccy <- [-halfSize .. halfSize - 1]
            , let midGX = ccx * chunkSize + chunkSize `div` 2
                  midGY = ccy * chunkSize + chunkSize `div` 2
            , not (isBeyondGlacier worldSize midGX midGY)
            , let (elev, mat) = elevationAtGlobal seed plates worldSize midGX midGY
                  (wcx, wcy) = gridToWorld midGX midGY
                  drawX = wcx - chunkWorldWidth / 2.0
                  drawY = wcy
            ]
    in V.fromList entries

-----------------------------------------------------------
-- Generate Zoom Map Quads (called every frame)
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
                    Just worldState -> renderFromCache env worldState camera
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
-- Render From Cache
-----------------------------------------------------------

-- | World width in screen-space X for wrapping.
worldScreenWidth :: Int -> Float
worldScreenWidth worldSize =
    fromIntegral (worldSize * chunkSize) * tileHalfWidth

renderFromCache :: EngineEnv -> WorldState -> Camera2D
               -> Int -> Int -> Float
               -> EngineM ε σ (V.Vector SortableQuad)
renderFromCache env worldState camera fbW fbH zoomAlpha = do
    mParams  <- liftIO $ readIORef (wsGenParamsRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    cache    <- liftIO $ readIORef (wsZoomCacheRef worldState)

    case mParams of
        Nothing -> return V.empty
        Just params -> do
            let vb = computeZoomViewBounds camera fbW fbH
                wsw = worldScreenWidth (wgpWorldSize params)

            -- Iterate the cached entries. For each, try base position
            -- and ±1 world width. Only emit quads that are in view.
            let visible = V.concatMap (entryToQuads vb wsw textures) cache

            quads <- V.mapM (\(ccx, ccy, th, dx, dy) ->
                chunkToZoomQuad th dx dy zoomAlpha ccx ccy) visible

            return quads

-- | For a single cached entry, produce 0-3 candidate quads
--   (base position and ±1 wrap).
entryToQuads :: ZoomViewBounds -> Float -> WorldTextures
             -> ZoomChunkEntry
             -> V.Vector (Int, Int, TextureHandle, Float, Float)
entryToQuads vb wsw textures entry =
    let ccx = zceChunkX entry
        ccy = zceChunkY entry
        baseX = zceDrawX entry
        baseY = zceDrawY entry
        texHandle = getZoomTexture textures (zceTexIndex entry) (zceElev entry)

        candidates = filter (\(_, _, _, dx, dy) -> isChunkInView vb dx dy)
            [ (ccx, ccy, texHandle, baseX - wsw, baseY)
            , (ccx, ccy, texHandle, baseX,       baseY)
            , (ccx, ccy, texHandle, baseX + wsw, baseY)
            ]
    in V.fromList candidates

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

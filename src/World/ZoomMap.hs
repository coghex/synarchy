{-# LANGUAGE Strict #-}
module World.ZoomMap
    ( generateZoomMapQuads
    , generateBackgroundQuads
    , buildZoomCache
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Monad (EngineM)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier)
import World.Generate (chunkSize)
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Grid (tileHalfWidth, tileHalfDiamondHeight, gridToWorld,
                   chunkWorldWidth, chunkWorldDiamondHeight, zoomMapLayer,
                   backgroundMapLayer, zoomFadeStart, zoomFadeEnd)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Build Zoom Cache (called once at world init)
-----------------------------------------------------------

-- | Pre-compute all zoom map entries. Called once when the
--   world is created. Returns a vector of entries for every
--   visible (non-beyond-glacier) chunk in the world.
--
--   Now applies the full geological timeline (craters, volcanoes,
--   etc.) to each chunk's midpoint so the zoomed-out map reflects
--   the actual post-geology terrain.
buildZoomCache :: WorldGenParams -> V.Vector ZoomChunkEntry
buildZoomCache params =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = generatePlates seed worldSize (wgpPlateCount params)
        halfSize = worldSize `div` 2
        timeline = wgpGeoTimeline params

        -- Collect ALL events from every geological period, in order
        allEvents = concatMap gpEvents (gtPeriods timeline)

        entries =
            [ ZoomChunkEntry
                { zceChunkX   = ccx
                , zceChunkY   = ccy
                , zceDrawX    = drawX
                , zceDrawY    = drawY
                , zceTexIndex = finalMat
                , zceElev     = finalElev
                }
            | ccx <- [-halfSize .. halfSize - 1]
            , ccy <- [-halfSize .. halfSize - 1]
            , let midGX = ccx * chunkSize + chunkSize `div` 2
                  midGY = ccy * chunkSize + chunkSize `div` 2
            , not (isBeyondGlacier worldSize midGX midGY)
            , let (baseElev, baseMat) = elevationAtGlobal seed plates worldSize midGX midGY
                  -- Skip geology for glacier tiles
                  (finalElev, finalMat) =
                      if baseMat == matGlacier
                      then (baseElev, unMaterialId baseMat)
                      else applyAllEvents allEvents worldSize
                               midGX midGY baseElev (unMaterialId baseMat)
                  (wcx, wcy) = gridToWorld midGX midGY
                  drawX = wcx - chunkWorldWidth / 2.0
                  drawY = wcy
            ]

    in V.fromList entries

-- | Fold all geological events over a single tile position.
--   Returns the final (elevation, materialId).
--   Events are applied in timeline order. Each event's elevation
--   delta is additive; material overrides replace the current material.
applyAllEvents :: [GeoEvent] -> Int -> Int -> Int -> Int -> Word8
               -> (Int, Word8)
applyAllEvents events worldSize gx gy baseElev baseMat =
    foldl' applyOne (baseElev, baseMat) events
  where
    applyOne (elev, mat) event =
        let GeoModification deltaE mMat = applyGeoEvent event worldSize gx gy elev
            newElev = elev + deltaE
            newMat  = case mMat of
                        Just m  -> m
                        Nothing -> mat
        in (newElev, newMat)

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
                                         fbW fbH zoomAlpha getZoomTexture zoomMapLayer
                    Nothing         -> return V.empty
            return $ V.concat quads

-----------------------------------------------------------
-- Generate Background Quads (called every frame, always visible)
-----------------------------------------------------------

generateBackgroundQuads :: EngineM ε σ (V.Vector SortableQuad)
generateBackgroundQuads = do
    env <- ask
    camera <- liftIO $ readIORef (cameraRef env)
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    worldManager <- liftIO $ readIORef (worldManagerRef env)

    quads <- forM (wmVisible worldManager) $ \pageId ->
        case lookup pageId (wmWorlds worldManager) of
            Just worldState -> renderFromCache env worldState camera
                                 fbW fbH 1.0 getBgTexture backgroundMapLayer
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
-- Render From Cache (shared by zoom map and background)
-----------------------------------------------------------

-- | World width in screen-space X for wrapping.
worldScreenWidth :: Int -> Float
worldScreenWidth worldSize =
    fromIntegral (worldSize * chunkSize) * tileHalfWidth

-- | Shared render function. Takes a texture picker and target layer
--   so it can be used for both the zoom map and background.
renderFromCache :: EngineEnv -> WorldState -> Camera2D
               -> Int -> Int -> Float
               -> (WorldTextures -> Word8 -> Int -> TextureHandle)
               -> LayerId
               -> EngineM ε σ (V.Vector SortableQuad)
renderFromCache env worldState camera fbW fbH alpha texturePicker layer = do
    mParams  <- liftIO $ readIORef (wsGenParamsRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    cache    <- liftIO $ readIORef (wsZoomCacheRef worldState)

    -- Look up graphics state ONCE
    gs <- gets graphicsState
    let lookupSlot texHandle = fromIntegral $ case textureSystem gs of
            Just bindless -> getTextureSlotIndex texHandle bindless
            Nothing       -> 0
        defFmSlot = fromIntegral (defaultFaceMapSlot gs)

    case mParams of
        Nothing -> return V.empty
        Just params -> do
            let vb = computeZoomViewBounds camera fbW fbH
                wsw = worldScreenWidth (wgpWorldSize params)

                visible = concatMap
                    (entryToQuadsList vb wsw textures texturePicker)
                    (V.toList cache)

                -- Fully pure — no V.mapM needed
                quads = V.fromList
                    [ mkZoomQuad lookupSlot defFmSlot th dx dy alpha ccx ccy layer
                    | (ccx, ccy, th, dx, dy) <- visible
                    ]

            return quads

-- | Pure zoom quad builder. No EngineM needed.
mkZoomQuad :: (TextureHandle -> Int) -> Float -> TextureHandle
           -> Float -> Float -> Float -> Int -> Int -> LayerId
           -> SortableQuad
mkZoomQuad lookupSlot defFmSlot texHandle drawX drawY alpha ccx ccy layer =
    let actualSlot = lookupSlot texHandle
        w = chunkWorldWidth
        h = chunkWorldDiamondHeight
        tint = Vec4 1.0 1.0 1.0 alpha
        sortKey = fromIntegral (ccx + ccy)
        vertices = V.fromListN 6
            [ Vertex (Vec2 drawX drawY)               (Vec2 0 0) tint (fromIntegral actualSlot) defFmSlot
            , Vertex (Vec2 (drawX + w) drawY)          (Vec2 1 0) tint (fromIntegral actualSlot) defFmSlot
            , Vertex (Vec2 (drawX + w) (drawY + h))    (Vec2 1 1) tint (fromIntegral actualSlot) defFmSlot
            , Vertex (Vec2 drawX drawY)                (Vec2 0 0) tint (fromIntegral actualSlot) defFmSlot
            , Vertex (Vec2 (drawX + w) (drawY + h))    (Vec2 1 1) tint (fromIntegral actualSlot) defFmSlot
            , Vertex (Vec2 drawX (drawY + h))          (Vec2 0 1) tint (fromIntegral actualSlot) defFmSlot
            ]
    in SortableQuad
        { sqSortKey  = sortKey
        , sqVertices = vertices
        , sqTexture  = texHandle
        , sqLayer    = layer
        }

entryToQuadsList :: ZoomViewBounds -> Float -> WorldTextures
                 -> (WorldTextures -> Word8 -> Int -> TextureHandle)
                 -> ZoomChunkEntry
                 -> [(Int, Int, TextureHandle, Float, Float)]
entryToQuadsList vb wsw textures texturePicker entry =
    let ccx = zceChunkX entry
        ccy = zceChunkY entry
        baseX = zceDrawX entry
        baseY = zceDrawY entry
        texHandle = texturePicker textures (zceTexIndex entry) (zceElev entry)
    in filter (\(_, _, _, dx, dy) -> isChunkInView vb dx dy)
        [ (ccx, ccy, texHandle, baseX - wsw, baseY)
        , (ccx, ccy, texHandle, baseX,       baseY)
        , (ccx, ccy, texHandle, baseX + wsw, baseY)
        ]

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

getZoomTexture :: WorldTextures -> Word8 -> Int -> TextureHandle
getZoomTexture textures 250 _ = wtZoomGlacier textures
getZoomTexture textures _mat elev
    | elev < -100 = wtZoomOcean textures
getZoomTexture textures 1  _ = wtZoomGranite textures
getZoomTexture textures 2  _ = wtZoomDiorite textures
getZoomTexture textures 3  _ = wtZoomGabbro textures
getZoomTexture textures 4  _ = wtZoomBasalt textures
getZoomTexture textures 5  _ = wtZoomObsidian textures
getZoomTexture textures 20 _ = wtZoomImpactite textures
getZoomTexture textures _  _ = wtZoomGranite textures

getBgTexture :: WorldTextures -> Word8 -> Int -> TextureHandle
getBgTexture textures 250 _ = wtBgGlacier textures
getBgTexture textures _mat elev
    | elev < -100 = wtBgOcean textures
getBgTexture textures 1  _ = wtBgGranite textures
getBgTexture textures 2  _ = wtBgDiorite textures
getBgTexture textures 3  _ = wtBgGabbro textures
getBgTexture textures 4  _ = wtBgBasalt textures
getBgTexture textures 20 _ = wtBgImpactite textures
getBgTexture textures _  _ = wtBgGranite textures

clamp01 :: Float -> Float
clamp01 x
    | x < 0    = 0
    | x > 1    = 1
    | otherwise = x

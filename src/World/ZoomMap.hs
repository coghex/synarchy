{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.ZoomMap
    ( generateZoomMapQuads
    , generateBackgroundQuads
    , buildZoomCache
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Data.IORef (readIORef, writeIORef, IORef)
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
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Grid (tileHalfWidth, tileHalfDiamondHeight, gridToWorld,
                   chunkWorldWidth, chunkWorldDiamondHeight, zoomMapLayer,
                   backgroundMapLayer, zoomFadeStart, zoomFadeEnd,
                   worldScreenWidth)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Build Zoom Cache (called once at world init)
-----------------------------------------------------------

buildZoomCache ∷ WorldGenParams → V.Vector ZoomChunkEntry
buildZoomCache params =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = generatePlates seed worldSize (wgpPlateCount params)
        halfSize = worldSize `div` 2
        timeline = wgpGeoTimeline params
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
            | ccx ← [-halfSize .. halfSize - 1]
            , ccy ← [-halfSize .. halfSize - 1]
            , let midGX = ccx * chunkSize + chunkSize `div` 2
                  midGY = ccy * chunkSize + chunkSize `div` 2
            , not (isBeyondGlacier worldSize midGX midGY)
            , let (baseElev, baseMat) = elevationAtGlobal seed plates worldSize midGX midGY
                  (finalElev, finalMat) =
                      if baseMat ≡ matGlacier
                      then (baseElev, unMaterialId baseMat)
                      else applyAllEvents allEvents worldSize
                               midGX midGY baseElev (unMaterialId baseMat)
                  (wcx, wcy) = gridToWorld midGX midGY
                  drawX = wcx - chunkWorldWidth / 2.0
                  drawY = wcy
            ]

    in V.fromList entries

applyAllEvents ∷ [GeoEvent] → Int → Int → Int → Int → Word8
               → (Int, Word8)
applyAllEvents events worldSize gx gy baseElev baseMat =
    foldl' applyOne (baseElev, baseMat) events
  where
    applyOne (elev, mat) event =
        let GeoModification deltaE mMat = applyGeoEvent event worldSize gx gy elev
            newElev = elev + deltaE
            newMat  = case mMat of
                        Just m  → m
                        Nothing → mat
        in (newElev, newMat)

-----------------------------------------------------------
-- Bake Zoom Vertices (internal, called lazily on render thread)
-----------------------------------------------------------

-- | Pre-bake per-entry vertex data. Called once on the render thread
--   the first time we try to render and the baked cache is empty.
--   All expensive work (texture picker, slot lookup, vertex construction)
--   happens here and is never repeated.
bakeEntries ∷ V.Vector ZoomChunkEntry
            → (Word8 → Int → TextureHandle)
            → (TextureHandle → Int)
            → Float
            → V.Vector BakedZoomEntry
bakeEntries cache texPicker lookupSlot defFmSlot =
    V.map bakeOne cache
  where
    bakeOne entry =
        let texHandle = texPicker (zceTexIndex entry) (zceElev entry)
            actualSlot = fromIntegral (lookupSlot texHandle)
            drawX = zceDrawX entry
            drawY = zceDrawY entry
            w = chunkWorldWidth
            h = chunkWorldDiamondHeight
            white = Vec4 1.0 1.0 1.0 1.0
        in BakedZoomEntry
            { bzeChunkX  = zceChunkX entry
            , bzeChunkY  = zceChunkY entry
            , bzeDrawX   = drawX
            , bzeDrawY   = drawY
            , bzeSortKey = fromIntegral (zceChunkX entry + zceChunkY entry)
            , bzeV0      = Vertex (Vec2 drawX drawY)            (Vec2 0 0) white actualSlot defFmSlot
            , bzeV1      = Vertex (Vec2 (drawX + w) drawY)       (Vec2 1 0) white actualSlot defFmSlot
            , bzeV2      = Vertex (Vec2 (drawX + w) (drawY + h)) (Vec2 1 1) white actualSlot defFmSlot
            , bzeV3      = Vertex (Vec2 drawX (drawY + h))       (Vec2 0 1) white actualSlot defFmSlot
            , bzeTexture = texHandle
            }

-- | Ensure the baked cache is populated, baking on first call.
--   Returns the baked vector. Subsequent calls just read the IORef.
ensureBaked ∷ IORef (V.Vector BakedZoomEntry)
            → V.Vector ZoomChunkEntry
            → WorldTextures
            → (WorldTextures → Word8 → Int → TextureHandle)
            → (TextureHandle → Int)
            → Float
            → EngineM ε σ (V.Vector BakedZoomEntry)
ensureBaked bakedRef rawCache textures texPicker lookupSlot defFmSlot = do
    existing ← liftIO $ readIORef bakedRef
    if V.null existing ∧ not (V.null rawCache)
        then do
            let baked = bakeEntries rawCache
                            (\mat elev → texPicker textures mat elev)
                            lookupSlot defFmSlot
            liftIO $ writeIORef bakedRef baked
            return baked
        else return existing

-----------------------------------------------------------
-- Generate Zoom Map Quads
-----------------------------------------------------------

generateZoomMapQuads ∷ EngineM ε σ (V.Vector SortableQuad)
generateZoomMapQuads = do
    env ← ask
    camera ← liftIO $ readIORef (cameraRef env)
    (fbW, fbH) ← liftIO $ readIORef (framebufferSizeRef env)
    worldManager ← liftIO $ readIORef (worldManagerRef env)

    let zoom = camZoom camera
        zoomAlpha = clamp01 ((zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    if zoomAlpha ≤ 0.001
        then return V.empty
        else do
            quads ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState →
                        renderFromBaked env worldState camera
                            fbW fbH zoomAlpha getZoomTexture
                            (wsBakedZoomRef worldState) zoomMapLayer
                    Nothing → return V.empty
            return $ V.concat quads

-----------------------------------------------------------
-- Generate Background Quads
-----------------------------------------------------------

generateBackgroundQuads ∷ EngineM ε σ (V.Vector SortableQuad)
generateBackgroundQuads = do
    env ← ask
    camera ← liftIO $ readIORef (cameraRef env)
    (fbW, fbH) ← liftIO $ readIORef (framebufferSizeRef env)
    worldManager ← liftIO $ readIORef (worldManagerRef env)

    quads ← forM (wmVisible worldManager) $ \pageId →
        case lookup pageId (wmWorlds worldManager) of
            Just worldState →
                renderFromBaked env worldState camera
                    fbW fbH 1.0 getBgTexture
                    (wsBakedBgRef worldState) backgroundMapLayer
            Nothing → return V.empty
    return $ V.concat quads

-----------------------------------------------------------
-- Screen-Space View Bounds
-----------------------------------------------------------

data ZoomViewBounds = ZoomViewBounds
    { zvLeft   ∷ !Float
    , zvRight  ∷ !Float
    , zvTop    ∷ !Float
    , zvBottom ∷ !Float
    }

computeZoomViewBounds ∷ Camera2D → Int → Int → ZoomViewBounds
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

isChunkInView ∷ ZoomViewBounds → Float → Float → Bool
isChunkInView vb drawX drawY =
    let right  = drawX + chunkWorldWidth
        bottom = drawY + chunkWorldDiamondHeight
    in not (right  < zvLeft vb
         ∨ drawX  > zvRight vb
         ∨ bottom < zvTop vb
         ∨ drawY  > zvBottom vb)

-----------------------------------------------------------
-- Render From Baked Cache (hot path)
-----------------------------------------------------------

-- | The hot render path. Lazily bakes on first call, then
--   every subsequent frame just does view-cull + alpha stamp.
renderFromBaked ∷ EngineEnv → WorldState → Camera2D
               → Int → Int → Float
               → (WorldTextures → Word8 → Int → TextureHandle)
               → IORef (V.Vector BakedZoomEntry)
               → LayerId
               → EngineM ε σ (V.Vector SortableQuad)
renderFromBaked env worldState camera fbW fbH alpha texturePicker bakedRef layer = do
    mParams  ← liftIO $ readIORef (wsGenParamsRef worldState)
    textures ← liftIO $ readIORef (wsTexturesRef worldState)
    rawCache ← liftIO $ readIORef (wsZoomCacheRef worldState)

    gs ← gets graphicsState
    let lookupSlot texHandle = fromIntegral $ case textureSystem gs of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral (defaultFaceMapSlot gs)

    case mParams of
        Nothing → return V.empty
        Just params → do
            -- Lazy bake: only runs once, when bakedRef is empty
            baked ← ensureBaked bakedRef rawCache textures
                        texturePicker lookupSlot defFmSlot

            let vb = computeZoomViewBounds camera fbW fbH
                wsw = worldScreenWidth (wgpWorldSize params)

                -- Hot fold: only visibility + emit. No texture lookups.
                !visibleQuads = V.foldl' (\acc entry →
                    let baseX = bzeDrawX entry
                        baseY = bzeDrawY entry
                        tryOffset dx acc'
                            | isChunkInView vb dx baseY =
                                emitQuad entry dx alpha layer : acc'
                            | otherwise = acc'
                    in tryOffset (baseX - wsw)
                     $ tryOffset  baseX
                     $ tryOffset (baseX + wsw) acc
                    ) [] baked

            return $! V.fromList visibleQuads

-- | Emit a SortableQuad from a baked entry.
--   Shifts X position for wrap offset, patches alpha channel.
emitQuad ∷ BakedZoomEntry → Float → Float → LayerId → SortableQuad
emitQuad entry dx alpha layer =
    let !baseX = bzeDrawX entry
        !xShift = dx - baseX
        shiftV (Vertex (Vec2 px py) uv (Vec4 cr cg cb _) aid fid) =
            Vertex (Vec2 (px + xShift) py) uv (Vec4 cr cg cb alpha) aid fid
        v0 = shiftV (bzeV0 entry)
        v1 = shiftV (bzeV1 entry)
        v2 = shiftV (bzeV2 entry)
        v3 = shiftV (bzeV3 entry)
        vertices = V.fromListN 6 [v0, v1, v2, v0, v2, v3]
    in SortableQuad
        { sqSortKey  = bzeSortKey entry
        , sqVertices = vertices
        , sqTexture  = bzeTexture entry
        , sqLayer    = layer
        }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

getZoomTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getZoomTexture textures 250 _ = wtZoomGlacier textures
getZoomTexture textures _mat elev
    | elev < -100 = wtZoomOcean textures
getZoomTexture textures 1   _ = wtZoomGranite textures
getZoomTexture textures 2   _ = wtZoomDiorite textures
getZoomTexture textures 3   _ = wtZoomGabbro textures
getZoomTexture textures 4   _ = wtZoomBasalt textures
getZoomTexture textures 5   _ = wtZoomObsidian textures
getZoomTexture textures 20  _ = wtZoomImpactite textures
getZoomTexture textures 100 _ = wtZoomLava textures
getZoomTexture textures _   _ = wtZoomGranite textures

getBgTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getBgTexture textures 250 _ = wtBgGlacier textures
getBgTexture textures _mat elev
    | elev < -100 = wtBgOcean textures
getBgTexture textures 1   _ = wtBgGranite textures
getBgTexture textures 2   _ = wtBgDiorite textures
getBgTexture textures 3   _ = wtBgGabbro textures
getBgTexture textures 4   _ = wtBgBasalt textures
getBgTexture textures 5   _ = wtBgObsidian textures
getBgTexture textures 20  _ = wtBgImpactite textures
getBgTexture textures 100 _ = wtBgLava textures
getBgTexture textures _   _ = wtBgGranite textures

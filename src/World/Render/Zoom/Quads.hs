{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Generate zoom-map quads (the main zoomed-out world view).
module World.Render.Zoom.Quads
    ( generateZoomMapQuads
    , renderFromBaked
    , makeMapQuads
    , emitQuad
    ) where

import UPrelude
import Data.IORef (readIORef, IORef)
import qualified Data.Vector as V
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Grid (zoomMapLayer, zoomFadeStart, zoomFadeEnd)
import World.Weather.Types (ClimateCoord(..), ClimateGrid(..), ClimateState(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , OceanCell(..), OceanGrid(..))

import World.Render.Zoom.Bake (ensureBaked)
import World.Render.Zoom.ViewBounds (ZoomViewBounds(..), computeZoomViewBounds
                                    , isChunkInView, bestZoomWrapOffset)
import World.Render.Zoom.Climate (tempToColorAt, pressureToColorAt, humidityToColorAt
                                 , precipToColorAt, precipTypeToColorAt, evapToColorAt
                                 , seaTempToColorAt)
import World.Render.Zoom.Cursor (makeCursorQuad)
import World.Render.Zoom.Textures (getZoomTexture)

-----------------------------------------------------------
-- Generate Zoom Map Quads
-----------------------------------------------------------

generateZoomMapQuads ∷ EngineEnv → Camera2D → Int → Int → IO (V.Vector SortableQuad)
generateZoomMapQuads env camera fbW fbH = do
    worldManager ← readIORef (worldManagerRef env)

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

renderFromBaked ∷ EngineEnv → WorldState → Camera2D → Int → Int → Float
              → (WorldTextures → Word8 → Int → TextureHandle)
              → IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)
              → LayerId
              → IO (V.Vector SortableQuad)
renderFromBaked env worldState camera fbW fbH alpha texturePicker bakedRef layer = do
    mParams  ← readIORef (wsGenParamsRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    rawCache ← readIORef (wsZoomCacheRef worldState)

    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
    mapMode ← readIORef (wsMapModeRef worldState)
    (winW, winH) ← readIORef (windowSizeRef env)
    let lookupSlot texHandle = fromIntegral $ case mBindless of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral defFmSlotWord
        facing = camFacing camera
    case mParams of
        Nothing → return V.empty
        Just params → do
            baked ← ensureBaked bakedRef rawCache textures facing
                        texturePicker lookupSlot defFmSlot
            let vb = computeZoomViewBounds camera fbW fbH
                ws = wgpWorldSize params
                (camX, camY) = camPosition camera

                !visibleQuads = makeMapQuads params mapMode baked facing
                                             vb camX camY alpha layer
            cursorQuad ← makeCursorQuad facing camera winW winH
                                        fbW fbH ws (wsCursorRef worldState)
                                        lookupSlot defFmSlot
            return $ visibleQuads <> cursorQuad

-----------------------------------------------------------
-- Map Quads by Mode
-----------------------------------------------------------

makeMapQuads ∷ WorldGenParams → ZoomMapMode → V.Vector BakedZoomEntry
  → CameraFacing → ZoomViewBounds → Float → Float → Float
  → LayerId → V.Vector SortableQuad
makeMapQuads params mapMode baked facing vb camX camY alpha layer =
  let ws = wgpWorldSize params
      climateState = wgpClimateState params
      cgrid = cgRegions (csClimate climateState)
      seagrid = ogCells (csOcean climateState)

      -- Shared per-entry logic: compute wrap offset, check visibility, emit
      go colorFn = V.mapMaybe (\entry →
          let baseX = bzeDrawX entry
              baseY = bzeDrawY entry
              w = bzeWidth entry
              h = bzeHeight entry
              centerX = baseX + w / 2.0
              centerY = baseY + h / 2.0
              (offX, offY) = bestZoomWrapOffset facing ws camX camY
                                                centerX centerY
              wrappedX = baseX + offX
              wrappedY = baseY + offY
              color = colorFn entry wrappedX wrappedY
          in if isChunkInView vb wrappedX wrappedY w h
             then Just (emitQuad entry color wrappedX wrappedY layer)
             else Nothing
          ) baked

  in case mapMode of
    ZMTemp → go $ \_ wx wy →
        let (cr, cg, cb) = tempToColorAt facing ws wx wy cgrid
        in Vec4 cr cg cb alpha
    ZMSeaTemp → go $ \entry wx wy →
        if bzeIsOcean entry
        then let (cr, cg, cb) = seaTempToColorAt facing ws wx wy seagrid
             in Vec4 cr cg cb alpha
        else Vec4 0.4 0.4 0.4 alpha
    ZMPressure → go $ \_ wx wy →
        let (cr, cg, cb) = pressureToColorAt facing ws wx wy cgrid
        in Vec4 cr cg cb alpha
    ZMPrecipitation → go $ \_ wx wy →
        let (cr, cg, cb) = precipToColorAt facing ws wx wy cgrid
        in Vec4 cr cg cb alpha
    ZMPrecipType → go $ \_ wx wy →
        let (cr, cg, cb) = precipTypeToColorAt facing ws wx wy cgrid
        in Vec4 cr cg cb alpha
    ZMEvaporation → go $ \_ wx wy →
        let (cr, cg, cb) = evapToColorAt facing ws wx wy cgrid
        in Vec4 cr cg cb alpha
    ZMHumidity → go $ \_ wx wy →
        let (cr, cg, cb) = humidityToColorAt facing ws wx wy cgrid
        in Vec4 cr cg cb alpha
    _ → go $ \_ _ _ → Vec4 1.0 1.0 1.0 alpha

-----------------------------------------------------------
-- Emit a Single Quad
-----------------------------------------------------------

emitQuad ∷ BakedZoomEntry → Vec4 → Float → Float → LayerId → SortableQuad
emitQuad entry (Vec4 cr cg cb alpha) dx dy layer =
    let !baseX = bzeDrawX entry
        !baseY = bzeDrawY entry
        !xShift = dx - baseX
        !yShift = dy - baseY
        shiftV (Vertex (Vec2 px py) uv _ aid fid) =
            Vertex (Vec2 (px + xShift) (py + yShift)) uv (Vec4 cr cg cb alpha) aid fid
        v0 = shiftV (bzeV0 entry)
        v1 = shiftV (bzeV1 entry)
        v2 = shiftV (bzeV2 entry)
        v3 = shiftV (bzeV3 entry)
    in SortableQuad
        { sqSortKey  = bzeSortKey entry
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = bzeTexture entry
        , sqLayer    = layer
        }

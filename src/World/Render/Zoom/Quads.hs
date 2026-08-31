{-# LANGUAGE Strict #-}
-- | Generate zoom-map quads (the main zoomed-out world view).
module World.Render.Zoom.Quads
    ( generateZoomMapQuads
    , generateZoomMapQuadsScanned
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Data.IORef (readIORef, IORef)
import qualified Data.Vector as V
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..), stampSolarPage)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..)
                                           , noFaceMapVertexId)
import World.Types
import World.Render.Zoom.Types (BakedZoomEntry(..), ZoomMapMode(..))
import World.Grid (zoomMapLayer, zoomFadeStart, zoomFadeEnd)
import World.Weather.Types (ClimateGrid(..), ClimateState(..), OceanGrid(..))

import World.Render.Zoom.Bake (ensureBakedAtlas)
import World.Render.Zoom.ViewBounds (ZoomViewBounds(..), computeZoomViewBounds
                                    , isChunkInView, bestZoomWrapOffset)
import World.Render.Zoom.Climate (tempToColorAt, pressureToColorAt, humidityToColorAt
                                 , precipToColorAt, precipTypeToColorAt, evapToColorAt
                                 , seaTempToColorAt)
import World.Render.Zoom.Cursor (makeCursorQuadScanned)
import Location.Instance (instancesCount)
import World.Render.Zoom.Textures (getZoomTexture)
import World.Render.Zoom.Icons (locationIconTargetPixels, iconWorldSize
                               , buildLocationIconMap, makeLocationIconQuads)

-- * Generate Zoom Map Quads

-- | The zoom map is longitude-lit like every other world quad — its
--   bake stamps packed world coordinates through 'mkVertexWorld' and it
--   draws through the same bindless pipeline — so its terrain quads,
--   its location icons and its cursor all take their OWN page's solar
--   slot (#1869). The stamp lands on the finished per-page run, so a
--   future zoom overlay is attributed correctly without being told to
--   be.
generateZoomMapQuads ∷ EngineEnv → (WorldPageId → Word32) → Camera2D → Int → Int
                     → IO (V.Vector SortableQuad)
generateZoomMapQuads env solarSlotOf camera fbW fbH =
    snd ⊚ generateZoomMapQuadsScanned env solarSlotOf camera fbW fbH

-- | 'generateZoomMapQuads' with the scene-assembly telemetry (#1921)
--   this pass contributes: the candidates evaluated while the zoom pass
--   is ACTIVE, paired with the quads it produced.
--
--   A candidate is a baked zoom entry, a location instance, or a
--   present hover/selection cursor position — summed over the visible
--   pages. When the zoom fade leaves the pass inactive, or a page has
--   no generation parameters yet, nothing is enumerated and the count
--   is zero.
generateZoomMapQuadsScanned
    ∷ EngineEnv → (WorldPageId → Word32) → Camera2D → Int → Int
    → IO (Int, V.Vector SortableQuad)
generateZoomMapQuadsScanned env solarSlotOf camera fbW fbH = do
    worldManager ← readIORef (wsWorldManagerRef (toWorldSimCapability env))

    let zoom = camZoom camera
        zoomAlpha = clamp01 ((zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    if zoomAlpha ≤ 0.001
        then return (0, V.empty)
        else do
            pages ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState → do
                        (scanned, quads) ←
                            renderFromBaked env worldState camera
                                fbW fbH zoomAlpha getZoomTexture
                                (wsBakedZoomRef worldState) zoomMapLayer
                        return (scanned, stampSolarPage (solarSlotOf pageId) quads)
                    Nothing → return (0, V.empty)
            return (sum (map fst pages), V.concat (map snd pages))

-- | One page's zoom-map contribution, paired with the candidates it
--   evaluated (#1921): the baked entries the terrain quads are built
--   from, the location instances the icon overlay walks (only when the
--   icon size leaves that builder anything to enumerate), and the
--   present hover and selection cursor candidates.
renderFromBaked ∷ EngineEnv → WorldState → Camera2D → Int → Int → Float
              → (WorldTextures → Word8 → Int → TextureHandle)
              → IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)
              → LayerId
              → IO (Int, V.Vector SortableQuad)
renderFromBaked env worldState camera fbW fbH alpha texturePicker bakedRef layer = do
    mParams  ← readIORef (wsGenParamsRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    rawCache ← readIORef (wsZoomCacheRef worldState)

    mapMode ← readIORef (wsMapModeRef worldState)
    (winW, winH) ← readIORef (rvWindowSizeRef (toRenderViewCapability env))
    mAtlas ← readIORef (wsZoomAtlasRef worldState)
    -- Stable handle id resolved in the shader (#286); the flat zoom map
    -- has no directional face map of its own (#1696).
    let lookupSlot texHandle = fromIntegral (toInt texHandle)
        defFmSlot = noFaceMapVertexId
        facing = camFacing camera
    case mParams of
        Nothing → return (0, V.empty)
        Just params → do
            baked ← ensureBakedAtlas bakedRef rawCache textures facing
                        mAtlas texturePicker lookupSlot defFmSlot
            -- Location defs come through the `content-registries`
            -- capability (#890); the texture-name registry beside it is
            -- still `render-gpu-asset` state (SS7.2).
            registry ← readIORef
                (crLocationDefsRef (toContentRegistriesCapability env))
            nameReg  ← readIORef (rvTextureNameRegistryRef (toRenderViewCapability env))
            let vb = computeZoomViewBounds camera fbW fbH
                ws = wgpWorldSize params
                (camX, camY) = camPosition camera

                !visibleQuads = makeMapQuads params mapMode baked facing
                                             vb camX camY alpha layer
                -- Lifecycle-state map icons (#781, #1230): a dedicated
                -- overlay above every terrain/climate mode, texture- and
                -- colour-selected live from 'params' each frame — never
                -- routed through 'mapMode's color function, so it's never
                -- tinted/dimmed by whichever climate palette is active.
                iconSet  = buildLocationIconMap registry nameReg (wtNoTexture textures)
                iconSize = iconWorldSize locationIconTargetPixels (camZoom camera)
                                          (fromIntegral winH)
                !iconQuads = makeLocationIconQuads params iconSet facing vb
                                 camX camY alpha iconSize layer lookupSlot defFmSlot
            (cursorScanned, cursorQuad) ←
                makeCursorQuadScanned facing camera winW winH
                                      fbW fbH ws (wsCursorRef worldState)
                                      lookupSlot defFmSlot
            let scanned = V.length baked
                        + (if iconSize ≤ 0 then 0
                           else instancesCount
                                    (wgpLocationInstances params))
                        + cursorScanned
            return (scanned, visibleQuads <> iconQuads <> cursorQuad)

-- * Map Quads by Mode

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

-- * Emit a Single Quad

emitQuad ∷ BakedZoomEntry → Vec4 → Float → Float → LayerId → SortableQuad
emitQuad entry (Vec4 cr cg cb alpha) dx dy layer =
    let !baseX = bzeDrawX entry
        !baseY = bzeDrawY entry
        !xShift = dx - baseX
        !yShift = dy - baseY
        shiftV (Vertex (Vec2 px py) uv _ aid fid flags wuv sp) =
            Vertex (Vec2 (px + xShift) (py + yShift)) uv (Vec4 cr cg cb alpha)
                   aid fid flags wuv sp
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

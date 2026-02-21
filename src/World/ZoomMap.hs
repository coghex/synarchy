{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.ZoomMap
    ( generateZoomMapQuads
    , generateBackgroundQuads
    , buildZoomCache
    ) where

import UPrelude
import Control.Parallel.Strategies (parListChunk, using, rdeepseq)
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Data.IORef (readIORef, writeIORef, IORef)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Monad (EngineM)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalU)
import World.Fluids (seaLevel, isOceanChunk, hasAnyLavaQuick, hasAnyOceanFluid)
import World.Generate (applyTimelineFast)
import World.Grid (tileHalfWidth, tileHalfDiamondHeight, gridToWorld,
                   chunkWorldWidth, chunkWorldDiamondHeight, zoomMapLayer,
                   backgroundMapLayer, zoomFadeStart, zoomFadeEnd,
                   worldScreenWidth, worldToGrid)
import World.Weather.Types (ClimateCoord(..), ClimateGrid(..), ClimateState(..)
                           , climateRegionSize, RegionClimate(..), SeasonalClimate(..))

-----------------------------------------------------------
-- Sampling Configuration
-----------------------------------------------------------

sampleGridSize ∷ Int
sampleGridSize = 5

sampleOffsets ∷ [(Int, Int)]
sampleOffsets =
    let step = max 1 (chunkSize `div` (sampleGridSize + 1))
    in [ (sx * step, sy * step)
       | sx ← [1 .. sampleGridSize]
       , sy ← [1 .. sampleGridSize]
       ]

-----------------------------------------------------------
-- Build Zoom Cache (called once at world init)
-----------------------------------------------------------

buildZoomCache ∷ WorldGenParams → V.Vector ZoomChunkEntry
buildZoomCache params =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = wgpPlates params
        halfSize = worldSize `div` 2
        timeline = wgpGeoTimeline params
        oceanMap = wgpOceanMap params
        features = gtFeatures timeline

        -- World width in chunks (u-axis wraps at this period)
        w = halfSize * 2

        buildOne (ccx, ccy) =
            let baseGX = ccx * chunkSize
                baseGY = ccy * chunkSize

                samples = [ let gx = baseGX + ox
                                gy = baseGY + oy
                                (gx', gy') = wrapGlobalU worldSize gx gy
                                (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy'
                            in if baseMat ≡ matGlacier
                               then (baseElev, unMaterialId baseMat)
                               else if baseElev < -100 then (baseElev, 0)
                               else let (e, m) = applyTimelineFast timeline worldSize gx' gy'
                                                     (baseElev, baseMat)
                                    in (e, unMaterialId m)
                          | (ox, oy) ← sampleOffsets
                          ]
                winnerMat = majorityMaterial samples
                avgElev = let s = sum (map fst samples)
                          in s `div` length samples

                coord = ChunkCoord ccx ccy
                chunkLava = hasAnyLavaQuick features seed plates worldSize coord avgElev
                chunkOcean = isOceanChunk oceanMap coord
                          ∨ hasAnyOceanFluid oceanMap coord
            in ZoomChunkEntry
                { zceChunkX = ccx
                , zceChunkY = ccy
                , zceBaseGX = baseGX
                , zceBaseGY = baseGY
                , zceTexIndex = if isOceanChunk oceanMap coord
                                then 0
                                else winnerMat
                , zceElev     = if isOceanChunk oceanMap coord
                                then seaLevel else avgElev
                , zceIsOcean  = chunkOcean
                , zceHasLava  = chunkLava
                }

        -- Iterate in (u, v) space.
        -- u wraps: [-halfSize, halfSize)
        -- v does NOT wrap (it's the diagonal axis, clamped by the world bounds)
        -- ccx = (u + v) / 2, ccy = (v - u) / 2
        -- We need (u + v) to be even so ccx/ccy are integers.
        allCoords = [ let wrappedU = ((u + halfSize) `mod` w + w) `mod` w - halfSize
                          ccx = (wrappedU + v) `div` 2
                          ccy = (v - wrappedU) `div` 2
                      in (ccx, ccy)
                    | v ← [-halfSize .. halfSize - 1]
                    , u ← [-halfSize .. halfSize - 1]
                    , even (u + v)
                    ]

        -- Deduplicate: wrapping u can map two different (u,v) pairs
        -- to the same (ccx, ccy). Use a Set to keep only unique chunks.
        uniqueCoords = Set.toList $ Set.fromList allCoords

        chunkBatchSize = max 1 (length uniqueCoords `div` 128)
        results = map buildOne uniqueCoords `using` parListChunk chunkBatchSize rdeepseq
    in V.fromList results

wrapChunkX ∷ Int → Int → Int
wrapChunkX halfSize cx =
    let w = halfSize * 2
    in ((cx + halfSize) `mod` w + w) `mod` w - halfSize

wrapChunkY ∷ Int → Int → Int
wrapChunkY halfSize cy = max (-halfSize) (min (halfSize - 1) cy)

majorityMaterial ∷ [(Int, Word8)] → Word8
majorityMaterial samples =
    let counts = foldl' (\m (_, mat) → Map.insertWith (+) mat (1 ∷ Int) m)
                        Map.empty samples
        (winner, _) = Map.foldlWithKey' (\(bestMat, bestCount) mat count →
            if count > bestCount ∨ (count ≡ bestCount ∧ mat > bestMat)
            then (mat, count)
            else (bestMat, bestCount)
            ) (0, 0) counts
    in winner

-----------------------------------------------------------
-- Bake Zoom Vertices (internal, called lazily on render thread)
-----------------------------------------------------------

bakeEntries ∷ CameraFacing → V.Vector ZoomChunkEntry
            → (Word8 → Int → TextureHandle)
            → (TextureHandle → Int)
            → Float
            → V.Vector BakedZoomEntry
bakeEntries facing cache texPicker lookupSlot defFmSlot =
    V.map bakeOne cache
  where
    bakeOne entry =
        let texHandle = texPicker (zceTexIndex entry) (zceElev entry)
            actualSlot = fromIntegral (lookupSlot texHandle)
            baseGX = zceBaseGX entry
            baseGY = zceBaseGY entry
            (x0,y0) = gridToWorld facing baseGX baseGY
            (x1,y1) = gridToWorld facing (baseGX + chunkSize) baseGY
            (x2,y2) = gridToWorld facing baseGX (baseGY + chunkSize)
            (x3,y3) = gridToWorld facing (baseGX + chunkSize) (baseGY + chunkSize)
            drawX = min x0 (min x1 (min x2 x3))
            drawY = min y0 (min y1 (min y2 y3))
            w = max x0 (max x1 (max x2 x3)) - drawX
            h = max y0 (max y1 (max y2 y3)) - drawY
            white = Vec4 1.0 1.0 1.0 1.0
        in BakedZoomEntry
            { bzeChunkX  = zceChunkX entry
            , bzeChunkY  = zceChunkY entry
            , bzeDrawX   = drawX
            , bzeDrawY   = drawY
            , bzeWidth   = w
            , bzeHeight  = h
            , bzeSortKey = fromIntegral (zceChunkY entry)
                         + fromIntegral (zceChunkX entry) * 0.0001
            , bzeV0      = Vertex (Vec2 drawX drawY)            (Vec2 0 0) white actualSlot defFmSlot
            , bzeV1      = Vertex (Vec2 (drawX + w) drawY)       (Vec2 1 0) white actualSlot defFmSlot
            , bzeV2      = Vertex (Vec2 (drawX + w) (drawY + h)) (Vec2 1 1) white actualSlot defFmSlot
            , bzeV3      = Vertex (Vec2 drawX (drawY + h))       (Vec2 0 1) white actualSlot defFmSlot
            , bzeTexture = texHandle
            , bzeIsOcean = zceIsOcean entry
            , bzeHasLava = zceHasLava entry
            , bzeElev    = zceElev entry
            }

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

renderFromBaked env worldState camera fbW fbH alpha texturePicker bakedRef layer = do
    mParams  ← readIORef (wsGenParamsRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    rawCache ← readIORef (wsZoomCacheRef worldState)

    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
    mapMode ← readIORef (wsMapModeRef worldState)
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
                (camX, camY) = camPosition camera
                ws = wgpWorldSize params
                cgrid = cgRegions (csClimate (wgpClimateState params))

                !visibleQuads = case mapMode of
                    ZMTemp → V.mapMaybe (\entry →
                        let baseX = bzeDrawX entry
                            baseY = bzeDrawY entry
                            w = bzeWidth entry
                            h = bzeHeight entry
                            centerX = baseX + w / 2.0
                            centerY = baseY + h / 2.0
                            (offX, offY) = bestZoomWrapOffset facing ws camX camY centerX centerY
                            wrappedX = baseX + offX
                            wrappedY = baseY + offY
                            (cr, cb, cg) = tempToColorAt facing ws wrappedX
                                                         wrappedY cgrid
                            color = Vec4 cr cb cg alpha
                        in if isChunkInView vb wrappedX wrappedY w h
                           then Just (emitQuad entry color wrappedX wrappedY layer)
                           else Nothing
                        ) baked
                    _ → V.mapMaybe (\entry →
                        let baseX = bzeDrawX entry
                            baseY = bzeDrawY entry
                            w = bzeWidth entry
                            h = bzeHeight entry
                            centerX = baseX + w / 2.0
                            centerY = baseY + h / 2.0
                            (offX, offY) = bestZoomWrapOffset facing ws camX camY centerX centerY
                            wrappedX = baseX + offX
                            wrappedY = baseY + offY
                            color = Vec4 1.0 1.0 1.0 alpha
                        in if isChunkInView vb wrappedX wrappedY w h
                           then Just (emitQuad entry color wrappedX wrappedY layer)
                           else Nothing
                        ) baked

            return visibleQuads

tempToColorAt ∷ CameraFacing → Int → Float → Float
              → HM.HashMap ClimateCoord RegionClimate
              → (Float, Float, Float)
tempToColorAt facing worldSize x y cg =
    let -- 1. Screen-space → grid-tile (gx, gy)
        (gx, gy) = worldToGrid facing x y

        -- 2. Convert to (u, v) and wrap u
        u = gx - gy
        v = gx + gy
        w = worldSize * chunkSize
        halfW = w `div` 2
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW

        -- 3. (u, v) in tiles → (u, v) in chunks → climate region (ru, rv)
        --    Chunk-u = u / chunkSize, then same offset as initEarlyClimate:
        --    ru = (chunkU + halfChunks) / climateRegionSize
        halfChunks = worldSize `div` 2
        chunkU = floorDiv wrappedU chunkSize
        chunkV = floorDiv v chunkSize
        ru = (chunkU + halfChunks) `div` climateRegionSize
        rv = (chunkV + halfChunks) `div` climateRegionSize

        coord = ClimateCoord ru rv
    in case HM.lookup coord cg of
        Just region → let t = clamp01 $
                                (scWinter (rcAirTemp region)) / 30.0
                       in (t, 0, 1 - t)
        Nothing     → (1.0, 1.0, 1.0)
  where
    floorDiv a b
      | b > 0     = if a >= 0 then a `div` b else -(((-a) + b - 1) `div` b)
      | otherwise = error "floorDiv: non-positive divisor"

bestZoomWrapOffset ∷ CameraFacing → Int → Float → Float → Float → Float → (Float, Float)
bestZoomWrapOffset facing worldSize camX camY centerX centerY =
    let worldTiles = worldSize * chunkSize
        wswX = fromIntegral worldTiles * tileHalfWidth
        wswY = fromIntegral worldTiles * tileHalfDiamondHeight
    in case facing of
        FaceSouth → (pickBest wswX camX centerX, 0)
        FaceNorth → (pickBest wswX camX centerX, 0)
        FaceWest  → (0, pickBest wswY camY centerY)
        FaceEast  → (0, pickBest wswY camY centerY)
  where
    pickBest w cam center =
        let d0 = abs (center - cam)
            d1 = abs (center + w - cam)
            d2 = abs (center - w - cam)
        in if d1 < d0 then (if d2 < d1 then -w else w)
           else (if d2 < d0 then -w else 0)

ensureBaked ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)
              → V.Vector ZoomChunkEntry → WorldTextures
              → CameraFacing
              → (WorldTextures → Word8 → Int → TextureHandle)
              → (TextureHandle → Int) → Float
              → IO (V.Vector BakedZoomEntry)
ensureBaked bakedRef rawCache textures facing texPicker lookupSlot defFmSlot = do
    (existing, bakedWith, bakedFacing) ← readIORef bakedRef
    let texturesChanged = bakedWith ≢ textures
        facingChanged   = bakedFacing ≢ facing
        needsBake = not (V.null rawCache)
                  ∧ (V.null existing ∨ texturesChanged ∨ facingChanged)
    if needsBake
        then do
            let baked = bakeEntries facing rawCache
                            (\mat elev → texPicker textures mat elev)
                            lookupSlot defFmSlot
            writeIORef bakedRef (baked, textures, facing)
            return baked
        else return existing

-----------------------------------------------------------
-- Generate Background Quads
-----------------------------------------------------------

generateBackgroundQuads ∷ EngineEnv → Camera2D → Int → Int → IO (V.Vector SortableQuad)
generateBackgroundQuads env camera fbW fbH = do
    worldManager ← readIORef (worldManagerRef env)

    let zSlice = camZSlice camera

    quads ← forM (wmVisible worldManager) $ \pageId →
        case lookup pageId (wmWorlds worldManager) of
            Just worldState →
                renderFromBakedBg env worldState camera
                    fbW fbH 1.0 getBgTexture
                    (wsBakedBgRef worldState) backgroundMapLayer zSlice
            Nothing → return V.empty
    return $ V.concat quads

-----------------------------------------------------------
-- Background Render From Baked
-----------------------------------------------------------

renderFromBakedBg env worldState camera fbW fbH alpha texturePicker bakedRef layer zSlice = do
    mParams  ← readIORef (wsGenParamsRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    rawCache ← readIORef (wsZoomCacheRef worldState)

    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
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
                (camX, camY) = camPosition camera
                ws = wgpWorldSize params

                !visibleQuads = V.mapMaybe (\entry →
                    let baseX = bzeDrawX entry
                        baseY = bzeDrawY entry
                        w = bzeWidth entry
                        h = bzeHeight entry
                        centerX = baseX + w / 2.0
                        centerY = baseY + h / 2.0
                        (offX, offY) = bestZoomWrapOffset facing ws camX camY centerX centerY
                        wrappedX = baseX + offX
                        wrappedY = baseY + offY
                    in if isChunkInView vb wrappedX wrappedY w h
                       then Just (emitQuadBg entry wrappedX wrappedY alpha layer zSlice)
                       else Nothing
                    ) baked

            return visibleQuads

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

isChunkInView ∷ ZoomViewBounds → Float → Float → Float → Float → Bool
isChunkInView vb drawX drawY w h =
    let right  = drawX + w
        bottom = drawY + h
    in not (right  < zvLeft vb
         ∨ drawX  > zvRight vb
         ∨ bottom < zvTop vb
         ∨ drawY  > zvBottom vb)

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

emitQuadBg ∷ BakedZoomEntry → Float → Float → Float → LayerId → Int → SortableQuad
emitQuadBg entry dx dy alpha layer zSlice =
    let !baseX = bzeDrawX entry
        !baseY = bzeDrawY entry
        !xShift = dx - baseX
        !yShift = dy - baseY

        (tintR, tintG, tintB) =
            if bzeIsOcean entry ∧ zSlice < seaLevel
            then let waterDepth = max 0 (seaLevel - zSlice)
                     t = clamp01 (fromIntegral waterDepth / 30.0)
                     r = 0.6 - t * 0.4
                     g = 0.7 - t * 0.4
                     b = 0.9 - t * 0.3
                 in (r, g, b)
            else (1.0, 1.0, 1.0)

        shiftV (Vertex (Vec2 px py) uv (Vec4 _ _ _ _) aid fid) =
            Vertex (Vec2 (px + xShift) (py + yShift)) uv (Vec4 tintR tintG tintB alpha) aid fid
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

-----------------------------------------------------------
-- Texture Pickers
-----------------------------------------------------------

getZoomTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getZoomTexture textures 250 _  = wtZoomGlacier textures
getZoomTexture textures 0   _  = wtZoomOcean textures
getZoomTexture textures 1   _  = wtZoomGranite textures
getZoomTexture textures 2   _  = wtZoomDiorite textures
getZoomTexture textures 3   _  = wtZoomGabbro textures
getZoomTexture textures 4   _  = wtZoomBasalt textures
getZoomTexture textures 5   _  = wtZoomObsidian textures
getZoomTexture textures 10  _  = wtZoomSandstone textures
getZoomTexture textures 11  _  = wtZoomLimestone textures
getZoomTexture textures 12  _  = wtZoomShale textures
getZoomTexture textures 20  _  = wtZoomImpactite textures
getZoomTexture textures 30  _  = wtZoomIron textures
getZoomTexture textures 31  _  = wtZoomOlivine textures
getZoomTexture textures 32  _  = wtZoomPyroxene textures
getZoomTexture textures 33  _  = wtZoomFeldspar textures
getZoomTexture textures 100 _  = wtZoomLava textures
getZoomTexture textures _   _  = wtZoomGranite textures

getBgTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getBgTexture textures 250 _  = wtBgGlacier textures
getBgTexture textures 0   _  = wtBgOcean textures
getBgTexture textures 1   _  = wtBgGranite textures
getBgTexture textures 2   _  = wtBgDiorite textures
getBgTexture textures 3   _  = wtBgGabbro textures
getBgTexture textures 4   _  = wtBgBasalt textures
getBgTexture textures 5   _  = wtBgObsidian textures
getBgTexture textures 10  _  = wtBgSandstone textures
getBgTexture textures 11  _  = wtBgLimestone textures
getBgTexture textures 12  _  = wtBgShale textures
getBgTexture textures 20  _  = wtBgImpactite textures
getBgTexture textures 30  _  = wtBgIron textures
getBgTexture textures 31  _  = wtBgOlivine textures
getBgTexture textures 32  _  = wtBgPyroxene textures
getBgTexture textures 33  _  = wtBgFeldspar textures
getBgTexture textures 100 _  = wtBgLava textures
getBgTexture textures _   _  = wtBgGranite textures

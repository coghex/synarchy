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
import qualified Data.Map.Strict as Map
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
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalX)
import World.Fluids (seaLevel, isOceanChunk)
import World.Generate (applyTimeline)
import World.Grid (tileHalfWidth, tileHalfDiamondHeight, gridToWorld,
                   chunkWorldWidth, chunkWorldDiamondHeight, zoomMapLayer,
                   backgroundMapLayer, zoomFadeStart, zoomFadeEnd,
                   worldScreenWidth)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Sampling Configuration
-----------------------------------------------------------

-- | Number of sample points per axis within a chunk.
--   5×5 = 25 samples gives good coverage without being expensive.
--   Samples are evenly spaced within the chunk.
sampleGridSize ∷ Int
sampleGridSize = 5

-- | Generate the sample offsets within a chunk.
--   For a 16-tile chunk with 5 samples, positions are at
--   tiles 1, 4, 7, 10, 13 — evenly spaced, avoiding edges.
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

buildZoomCache ∷ CameraFacing → WorldGenParams → V.Vector ZoomChunkEntry
buildZoomCache facing params =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = generatePlates seed worldSize (wgpPlateCount params)
        halfSize = worldSize `div` 2
        timeline = wgpGeoTimeline params
        oceanMap = wgpOceanMap params

        entries =
            [ ZoomChunkEntry
                { zceChunkX   = ccx
                , zceChunkY   = ccy
                , zceDrawX    = drawX
                , zceDrawY    = drawY
                , zceWidth    = maximum cxs - minimum cxs
                , zceHeight   = maximum cys - minimum cys
                , zceTexIndex = if isOceanChunk oceanMap (ChunkCoord ccx ccy)
                                then 0 else winnerMat
                , zceElev     = if isOceanChunk oceanMap (ChunkCoord ccx ccy)
                                then seaLevel else avgElev
                , zceIsOcean  = isOceanChunk oceanMap (ChunkCoord ccx ccy)
                }
            | ccx ← [-halfSize .. halfSize - 1]
            , ccy ← [-halfSize .. halfSize - 1]
            , let baseGX = ccx * chunkSize
                  baseGY = ccy * chunkSize
                  midGX  = baseGX + chunkSize `div` 2
                  midGY  = baseGY + chunkSize `div` 2
            , not (isBeyondGlacier worldSize midGX midGY)
            , let -- Sample multiple points in this chunk
                  samples = [ let gx = baseGX + ox
                                  gy = baseGY + oy
                                  gx' = wrapGlobalX worldSize gx
                                  (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy
                              in if baseMat ≡ matGlacier
                                 then (baseElev, unMaterialId baseMat)
                                 else if baseElev < -100 then (baseElev, 0)
                                 else let (e, m) = applyTimeline timeline worldSize gx' gy
                                                       (baseElev, baseMat)
                                      in (e, unMaterialId m)
                            | (ox, oy) ← sampleOffsets
                            ]

                  -- Majority vote on material
                  winnerMat = majorityMaterial samples

                  -- Average elevation for texture selection (ocean check)
                  avgElev = let s = sum (map fst samples)
                            in s `div` length samples

                  -- Compute draw position from chunk's grid-space corners.
                  -- Convert all 4 corners through gridToWorld, then take
                  -- the axis-aligned bounding box. This works for any facing.
                  corners = [ gridToWorld facing gx gy
                            | gx ← [baseGX, baseGX + chunkSize]
                            , gy ← [baseGY, baseGY + chunkSize]
                            ]
                  cxs = map fst corners
                  cys = map snd corners
                  drawX = minimum cxs
                  drawY = minimum cys
            ]

    in V.fromList entries

-- | Pick the material that appears most often in the samples.
--   On ties, prefers the material with higher material ID
--   (volcanic/impact materials tend to be more visually interesting
--   and have higher IDs than base plate rock).
majorityMaterial ∷ [(Int, Word8)] → Word8
majorityMaterial samples =
    let counts = foldl' (\m (_, mat) → Map.insertWith (+) mat (1 ∷ Int) m)
                        Map.empty samples
        -- Find max count, break ties with higher material ID
        (winner, _) = Map.foldlWithKey' (\(bestMat, bestCount) mat count →
            if count > bestCount ∨ (count ≡ bestCount ∧ mat > bestMat)
            then (mat, count)
            else (bestMat, bestCount)
            ) (0, 0) counts
    in winner

-----------------------------------------------------------
-- Bake Zoom Vertices (internal, called lazily on render thread)
-----------------------------------------------------------

-- | Pre-bake per-entry vertex data. Called once on the render thread
--   the first time we try to render and the baked cache is empty.
--   All expensive work (texture picker, slot lookup, vertex construction)
--   happens here and is never repeated.
bakeEntries cache texPicker lookupSlot defFmSlot =
    V.map bakeOne cache
  where
    bakeOne entry =
        let texHandle = texPicker (zceTexIndex entry) (zceElev entry)
            actualSlot = fromIntegral (lookupSlot texHandle)
            drawX = zceDrawX entry
            drawY = zceDrawY entry
            w = zceWidth entry
            h = zceHeight entry
            white = Vec4 1.0 1.0 1.0 1.0
        in BakedZoomEntry
            { bzeChunkX  = zceChunkX entry
            , bzeChunkY  = zceChunkY entry
            , bzeDrawX   = drawX
            , bzeDrawY   = drawY
            , bzeWidth   = w
            , bzeHeight  = h
            , bzeSortKey = fromIntegral (zceChunkX entry + zceChunkY entry)
            , bzeV0      = Vertex (Vec2 drawX drawY)            (Vec2 0 0) white actualSlot defFmSlot
            , bzeV1      = Vertex (Vec2 (drawX + w) drawY)       (Vec2 1 0) white actualSlot defFmSlot
            , bzeV2      = Vertex (Vec2 (drawX + w) (drawY + h)) (Vec2 1 1) white actualSlot defFmSlot
            , bzeV3      = Vertex (Vec2 drawX (drawY + h))       (Vec2 0 1) white actualSlot defFmSlot
            , bzeTexture = texHandle
            , bzeIsOcean = zceIsOcean entry
            , bzeElev    = zceElev entry
            }

-----------------------------------------------------------
-- Generate Zoom Map Quads
-----------------------------------------------------------

-- | IO version — called from world thread
generateZoomMapQuads ∷ EngineEnv → IO (V.Vector SortableQuad)
generateZoomMapQuads env = do
    camera ← readIORef (cameraRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)
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

-- | IO version of renderFromBaked
renderFromBaked ∷ EngineEnv → WorldState → Camera2D
               → Int → Int → Float
               → (WorldTextures → Word8 → Int → TextureHandle)
               → IORef (V.Vector BakedZoomEntry, WorldTextures)
               → LayerId
               → IO (V.Vector SortableQuad)
renderFromBaked env worldState camera fbW fbH alpha texturePicker bakedRef layer = do
    mParams  ← readIORef (wsGenParamsRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    rawCache ← readIORef (wsZoomCacheRef worldState)

    -- Shared IORefs instead of gets graphicsState
    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
    let lookupSlot texHandle = fromIntegral $ case mBindless of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral defFmSlotWord

    case mParams of
        Nothing → return V.empty
        Just params → do
            baked ← ensureBaked bakedRef rawCache textures
                        texturePicker lookupSlot defFmSlot
            let vb = computeZoomViewBounds camera fbW fbH
                wsw = worldScreenWidth (wgpWorldSize params)

                -- Hot fold: only visibility + emit. No texture lookups.
                !visibleQuads = V.foldl' (\acc entry →
                    let baseX = bzeDrawX entry
                        baseY = bzeDrawY entry
                        w = bzeWidth entry
                        h = bzeHeight entry
                        tryOffset dx acc'
                            | isChunkInView vb dx baseY w h =
                                emitQuad entry dx alpha layer : acc'
                            | otherwise = acc'
                    in tryOffset (baseX - wsw)
                     $ tryOffset  baseX
                     $ tryOffset (baseX + wsw) acc
                    ) [] baked

            return $! V.fromList visibleQuads

ensureBaked bakedRef rawCache textures texPicker lookupSlot defFmSlot = do
    (existing, bakedWith) ← readIORef bakedRef
    let texturesChanged = bakedWith ≢ textures
        slotsStale = case V.null existing of
            True  → False
            False → let entry = V.head existing
                        currentSlot = fromIntegral (lookupSlot (bzeTexture entry))
                        Vertex _ _ _ bakedSlot _ = bzeV0 entry
                    in currentSlot ≢ bakedSlot
        needsBake = not (V.null rawCache)
                  ∧ (V.null existing ∨ texturesChanged ∨ slotsStale)
    if needsBake
        then do
            let baked = bakeEntries rawCache
                            (\mat elev → texPicker textures mat elev)
                            lookupSlot defFmSlot
            writeIORef bakedRef (baked, textures)
            return baked
        else return existing

-----------------------------------------------------------
-- Generate Background Quads (IO version for world thread)
-----------------------------------------------------------

generateBackgroundQuads ∷ EngineEnv → IO (V.Vector SortableQuad)
generateBackgroundQuads env = do
    camera ← readIORef (cameraRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)
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
-- Background Render From Baked (IO version for world thread)
-----------------------------------------------------------

renderFromBakedBg ∷ EngineEnv → WorldState → Camera2D
                    → Int → Int → Float
                    → (WorldTextures → Word8 → Int → TextureHandle)
                    → IORef (V.Vector BakedZoomEntry, WorldTextures)
                    → LayerId → Int
                    → IO (V.Vector SortableQuad)
renderFromBakedBg env worldState camera fbW fbH alpha texturePicker bakedRef layer zSlice = do
    mParams  ← readIORef (wsGenParamsRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    rawCache ← readIORef (wsZoomCacheRef worldState)

    -- Read from shared IORefs instead of gets graphicsState
    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
    let lookupSlot texHandle = fromIntegral $ case mBindless of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral defFmSlotWord

    case mParams of
        Nothing → return V.empty
        Just params → do
            baked ← ensureBaked bakedRef rawCache textures
                        texturePicker lookupSlot defFmSlot

            let vb = computeZoomViewBounds camera fbW fbH
                wsw = worldScreenWidth (wgpWorldSize params)

                !visibleQuads = V.foldl' (\acc entry →
                    let baseX = bzeDrawX entry
                        baseY = bzeDrawY entry
                        w = bzeWidth entry
                        h = bzeHeight entry
                        tryOffset dx acc'
                            | isChunkInView vb dx baseY w h =
                                emitQuadBg entry dx alpha layer zSlice : acc'
                            | otherwise = acc'
                    in tryOffset (baseX - wsw)
                     $ tryOffset  baseX
                     $ tryOffset (baseX + wsw) acc
                    ) [] baked

            return $! V.fromList visibleQuads

-- | Emit a background quad with underwater tinting.
--   Ocean backgrounds progressively darken and blue-shift
--   the deeper below sea level the z-slice goes.

emitQuadBg ∷ BakedZoomEntry → Float → Float → LayerId → Int → SortableQuad
emitQuadBg entry dx alpha layer zSlice =
    let !baseX = bzeDrawX entry
        !xShift = dx - baseX

        (tintR, tintG, tintB) =
            if bzeIsOcean entry ∧ zSlice < seaLevel
            then let waterDepth = seaLevel - zSlice
                     t = clamp01 (fromIntegral waterDepth / 30.0)
                     r = 0.6 - t * 0.4       -- 0.6 → 0.2
                     g = 0.7 - t * 0.4        -- 0.7 → 0.3
                     b = 0.9 - t * 0.3        -- 0.9 → 0.6
                 in (r, g, b)
            else (1.0, 1.0, 1.0)

        shiftV (Vertex (Vec2 px py) uv (Vec4 _ _ _ _) aid fid) =
            Vertex (Vec2 (px + xShift) py) uv (Vec4 tintR tintG tintB alpha) aid fid
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
-- Texture Pickers (complete for all materials)
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

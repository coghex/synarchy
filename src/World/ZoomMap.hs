{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.ZoomMap
    ( generateZoomMapQuads
    , generateBackgroundQuads
    , buildZoomCache
    ) where

import UPrelude
import Debug.Trace (trace)
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

buildZoomCache ∷ WorldGenParams → V.Vector ZoomChunkEntry
buildZoomCache params =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = generatePlates seed worldSize (wgpPlateCount params)
        halfSize = worldSize `div` 2
        timeline = wgpGeoTimeline params
        oceanMap = wgpOceanMap params

        -- To fill a screen-space rectangle, we need chunks covering:
        --   |v| = |gx + gy| ≤ halfTiles      (glacier)
        --   u = gx - gy spans one full wrap period (worldTiles)
        --
        -- With ccy fixed, we need ccx to range wide enough that
        -- for every valid v = gx+gy, the u = gx-gy range covers
        -- worldTiles. Since u = (ccx - ccy) * chunkSize, and ccy
        -- ranges [-halfSize, halfSize-1], ccx must range 
        -- [-halfSize*2, halfSize*2-1] to cover the full rectangle.
        -- We then wrap ccx back to canonical coords for data lookup.

        entries =
            [ ZoomChunkEntry
                { zceChunkX   = wrappedCcx
                , zceChunkY   = ccy
                , zceBaseGX   = baseGX  -- unwrapped, for screen position
                , zceBaseGY   = baseGY
                , zceTexIndex = if isOceanChunk oceanMap (ChunkCoord wrappedCcx ccy)
                                then 0 else winnerMat
                , zceElev     = if isOceanChunk oceanMap (ChunkCoord wrappedCcx ccy)
                                then seaLevel else avgElev
                , zceIsOcean  = isOceanChunk oceanMap (ChunkCoord wrappedCcx ccy)
                }
            | ccx ← [-halfSize * 2 .. halfSize * 2 - 1]
            , ccy ← [-halfSize .. halfSize - 1]
            , let baseGX = ccx * chunkSize
                  baseGY = ccy * chunkSize
                  midGX  = baseGX + chunkSize `div` 2
                  midGY  = baseGY + chunkSize `div` 2
                  -- Glacier check uses raw (unwrapped) coords
                  -- because v = gx + gy must be in range
            , abs (midGX + midGY) ≤ halfSize * chunkSize
            , let -- Wrap ccx for data lookup
                  wrappedCcx = wrapChunkX halfSize ccx
                  wrappedBaseGX = wrappedCcx * chunkSize
                  samples = [ let gx = wrappedBaseGX + ox
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
                  winnerMat = majorityMaterial samples
                  avgElev = let s = sum (map fst samples)
                            in s `div` length samples
            ]

    in V.fromList entries

-- | Wrap chunk X coordinate into [-halfSize, halfSize)
wrapChunkX ∷ Int → Int → Int
wrapChunkX halfSize cx =
    let w = halfSize * 2
    in ((cx + halfSize) `mod` w + w) `mod` w - halfSize

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
            -- Compute bounding box from grid corners using CURRENT facing
            corners = [ gridToWorld facing gx gy
                      | gx ← [baseGX, baseGX + chunkSize]
                      , gy ← [baseGY, baseGY + chunkSize]
                      ]
            cxs = map fst corners
            cys = map snd corners
            drawX = minimum cxs
            drawY = minimum cys
            w = maximum cxs - minimum cxs
            h = maximum cys - minimum cys
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
            , bzeElev    = zceElev entry
            }

-----------------------------------------------------------
-- Generate Zoom Map Quads
-----------------------------------------------------------

-- | IO version — called from world thread
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

-- | IO version of renderFromBaked
renderFromBaked env worldState camera fbW fbH alpha texturePicker bakedRef layer = do
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

                -- No wrapping — just render each chunk at its baked position.
                -- Camera X is wrapped by the input handler, so it always
                -- sees the correct part of the map.
                !visibleQuads = V.foldl' (\acc entry →
                    let baseX = bzeDrawX entry
                        baseY = bzeDrawY entry
                        w = bzeWidth entry
                        h = bzeHeight entry
                    in if isChunkInView vb baseX baseY w h
                       then emitQuad entry baseX alpha layer : acc
                       else acc
                    ) [] baked

            return $! V.fromList visibleQuads

-- | Pick the wrap offset that puts the chunk closest to the camera.
--   Same logic as bestWrapOffset in Render.hs.
bestZoomWrapOffset ∷ Float → Float → Float → Float
bestZoomWrapOffset wsw camX chunkCenterX =
    let candidates = [0, wsw, -wsw]
        dist offset = abs (chunkCenterX + offset - camX)
    in pickMin candidates
  where
    pickMin [c] = c
    pickMin (c:cs) = let best = pickMin cs
                     in if abs (chunkCenterX + c - camX) ≤ abs (chunkCenterX + best - camX)
                        then c else best
    pickMin []  = 0

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
-- Generate Background Quads (IO version for world thread)
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
-- Background Render From Baked (IO version for world thread)
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

                !visibleQuads = V.foldl' (\acc entry →
                    let baseX = bzeDrawX entry
                        baseY = bzeDrawY entry
                        w = bzeWidth entry
                        h = bzeHeight entry
                    in if isChunkInView vb baseX baseY w h
                       then emitQuad entry baseX alpha layer : acc
                       else acc
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
                     r = 0.6 - t * 0.4
                     g = 0.7 - t * 0.4
                     b = 0.9 - t * 0.3
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

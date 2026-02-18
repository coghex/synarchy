{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render
    ( updateWorldTiles
    , surfaceHeadroom
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Monad (EngineM)
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Types (RenderBatch(..), SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import World.Slope (slopeToFaceMapIndex)
import World.Fluids (FluidCell(..), FluidType(..), seaLevel)
import World.Generate (chunkToGlobal, chunkWorldBounds, viewDepth, globalToChunk)
import World.Grid (tileWidth, tileHeight, gridToScreen, tileSideHeight, worldLayer,
                   tileHalfWidth, tileHalfDiamondHeight, zoomFadeStart, zoomFadeEnd
                   , worldToGrid, worldScreenWidth, applyFacing)
import World.ZoomMap (generateZoomMapQuads, generateBackgroundQuads)

-----------------------------------------------------------
-- Camera Change Detection
-----------------------------------------------------------

camEpsilon ∷ Float
camEpsilon = tileHalfWidth

cameraChanged ∷ WorldCameraSnapshot → WorldCameraSnapshot → Bool
cameraChanged old new =
    let (ox, oy) = wcsPosition old
        (nx, ny) = wcsPosition new
    in abs (ox - nx) > camEpsilon
     ∨ abs (oy - ny) > camEpsilon
     ∨ abs (wcsZoom old - wcsZoom new) > camEpsilon
     ∨ wcsZSlice old ≢ wcsZSlice new
     ∨ wcsFbSize old ≢ wcsFbSize new
     ∨ wcsFacing old ≢ wcsFacing new

-----------------------------------------------------------
-- Surface Headroom
-----------------------------------------------------------

surfaceHeadroom ∷ Int
surfaceHeadroom = 10

-----------------------------------------------------------
-- Top-Level Entry Point
-----------------------------------------------------------

updateWorldTiles ∷ EngineEnv → IO (V.Vector SortableQuad)
updateWorldTiles env = do
    camera ← readIORef (cameraRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)

    let zoom = camZoom camera
        tileAlpha = clamp01 (1.0 - (zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))
        zoomAlpha = clamp01 ((zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    worldManager ← readIORef (worldManagerRef env)

    tileQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            let currentSnap = WorldCameraSnapshot
                    { wcsPosition = camPosition camera
                    , wcsZoom     = zoom
                    , wcsZSlice   = camZSlice camera
                    , wcsFbSize   = (fbW, fbH)
                    , wcsFacing   = camFacing camera
                    }
            quads ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState → do
                        cached ← readIORef (wsQuadCacheRef worldState)
                        case cached of
                            Just wqc | not (cameraChanged (wqcCamera wqc) currentSnap) →
                                return (wqcQuads wqc)
                            _ → do
                                result ← renderWorldQuads env worldState tileAlpha currentSnap
                                writeIORef (wsQuadCacheRef worldState) $
                                    Just (WorldQuadCache currentSnap result)
                                return result
                    Nothing → return V.empty
            return $ V.concat quads

    zoomQuads ← generateZoomMapQuads env camera fbW fbH

    let shouldTrack = camZTracking camera
                    ∨ (tileAlpha > 0.001 ∧ tileAlpha < 0.999)
    when shouldTrack $ do
        when (not (camZTracking camera)) $
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZTracking = True }, ())
        worldManager' ← readIORef (worldManagerRef env)
        forM_ (wmVisible worldManager') $ \pageId →
            case lookup pageId (wmWorlds worldManager') of
                Just worldState → do
                    tileData ← readIORef (wsTilesRef worldState)
                    let (camX, camY) = camPosition camera
                        facing = camFacing camera
                        (gx, gy) = worldToGrid facing camX camY
                        (chunkCoord, (lx, ly)) = globalToChunk gx gy
                    case lookupChunk chunkCoord tileData of
                        Just lc → do
                            let surfElev = (lcSurfaceMap lc) VU.! columnIndex lx ly
                                targetZ = surfElev + surfaceHeadroom
                            atomicModifyIORef' (cameraRef env) $ \cam →
                                (cam { camZSlice = targetZ }, ())
                        Nothing → return ()
                Nothing → return ()

    let allQuads = tileQuads <> zoomQuads
    return allQuads

-----------------------------------------------------------
-- View Bounds
-----------------------------------------------------------

data ViewBounds = ViewBounds
    { vbLeft   ∷ !Float
    , vbRight  ∷ !Float
    , vbTop    ∷ !Float
    , vbBottom ∷ !Float
    } deriving (Show)

computeViewBounds ∷ Camera2D → Int → Int → Int → ViewBounds
computeViewBounds camera fbW fbH effDepth =
    let (cx, cy) = camPosition camera
        zoom     = camZoom camera
        aspect   = fromIntegral fbW / fromIntegral fbH
        halfW    = zoom * aspect
        halfH    = zoom
        maxHeightPad = fromIntegral effDepth * tileSideHeight
        padX     = tileWidth + camEpsilon
        padY     = tileHeight + maxHeightPad + camEpsilon
    in ViewBounds
        { vbLeft   = cx - halfW - padX
        , vbRight  = cx + halfW + padX
        , vbTop    = cy - halfH - padY
        , vbBottom = cy + halfH + padY
        }

-----------------------------------------------------------
-- Chunk-Level Culling
-----------------------------------------------------------

bestWrapOffset ∷ Int → Float → Float → Float
bestWrapOffset worldSize camX chunkScreenX =
    let wsw = worldScreenWidth worldSize
        candidates = [0, wsw, -wsw]
        dist offset = abs (chunkScreenX + offset - camX)
    in minimumBy (\ac bc → compare (dist ac) (dist bc)) candidates
  where
    minimumBy f (hd:tl) = foldl' (\best c → if f c best ≡ LT then c else best) hd tl
    minimumBy _ []       = 0

isChunkVisibleWrapped ∷ CameraFacing → Int → ViewBounds → Float
  → ChunkCoord → Maybe Float
isChunkVisibleWrapped facing worldSize vb camX coord =
    let ((minGX, minGY), (maxGX, maxGY)) = chunkWorldBounds coord
        corners = [ gridToScreen facing gx gy
                  | gx ← [minGX, maxGX]
                  , gy ← [minGY, maxGY]
                  ]
        sxs = map fst corners
        sys = map snd corners
        sxMin = minimum sxs
        sxMax = maximum sxs
        syMin = minimum sys
        syMax = maximum sys

        chunkCenterX = (sxMin + sxMax + tileWidth) / 2.0
        offset = bestWrapOffset worldSize camX chunkCenterX

        chunkLeft   = sxMin + offset
        chunkRight  = sxMax + tileWidth + offset
        chunkTop    = syMin
        chunkBottom = syMax + tileHeight

        visible = not (chunkRight  < vbLeft vb
                    ∨ chunkLeft   > vbRight vb
                    ∨ chunkBottom < vbTop vb
                    ∨ chunkTop    > vbBottom vb)
    in if visible then Just offset else Nothing

isChunkRelevantForSlice ∷ Int → LoadedChunk → Bool
isChunkRelevantForSlice _zSlice lc =
    VU.any (/= minBound) (lcSurfaceMap lc)

-----------------------------------------------------------
-- Render World Quads
-----------------------------------------------------------

renderWorldQuads ∷ EngineEnv → WorldState → Float → WorldCameraSnapshot
  → IO (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha snap = do
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM ← readIORef (wsGenParamsRef worldState)
    camera ← readIORef (cameraRef env)

    let (fbW, fbH) = wcsFbSize snap
        facing = camFacing camera

    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
    let lookupSlot texHandle = fromIntegral $ case mBindless of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral defFmSlotWord
        lookupFmSlot texHandle =
            let s = lookupSlot texHandle
            in if s ≡ 0 then defFmSlot else fromIntegral s
        worldSize = case paramsM of
                      Nothing → 128
                      Just params → wgpWorldSize params
    let zSlice = camZSlice camera
        zoom   = camZoom camera
        chunks = HM.elems (wtdChunks tileData)
        (camX, _camY) = camPosition camera

        effectiveDepth = min viewDepth (max 8 (round (zoom * 40.0 ∷ Float)))

        vb = computeViewBounds camera fbW fbH effectiveDepth

        visibleChunksWithOffset =
            [ (lc, offset)
            | lc ← chunks
            , isChunkRelevantForSlice zSlice lc
            , Just offset ← [isChunkVisibleWrapped facing worldSize vb camX (lcCoord lc)]
            ]

    let chunkVectors = map (\(lc, xOffset) →
            let coord  = lcCoord lc
                tileMap = lcTiles lc
                surfMap = lcSurfaceMap lc
                fluidMap = lcFluidMap lc
                chunkHasFluid = V.any isJust fluidMap
                terrainSurfMap = lcTerrainSurfaceMap lc

                !realQuads = VU.ifoldl' (\acc idx _surfZ ->
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                            mFluid = fluidMap V.! idx
                            (gx, gy) = chunkToGlobal coord lx ly
                            (rawX, rawY) = gridToScreen facing gx gy
                            isUnderLava = case mFluid of
                                Just fc → fcType fc ≡ Lava ∧ fcSurface fc > zSlice - effectiveDepth
                                Nothing → False
                        in foldl' (\acc2 z →
                            case HM.lookup (lx, ly, z) tileMap of
                                Nothing → acc2
                                Just tile →
                                    if isUnderLava ∧ z < maybe 0 fcSurface mFluid
                                    then acc2
                                    else
                                    let relativeZ = z - zSlice
                                        heightOffset = fromIntegral relativeZ * tileSideHeight
                                        drawX = rawX + xOffset
                                        drawY = rawY - heightOffset
                                    in if isTileVisible vb drawX drawY
                                       then tileToQuad lookupSlot lookupFmSlot textures facing
                                              gx gy z tile zSlice effectiveDepth zoomAlpha xOffset
                                              mFluid chunkHasFluid : acc2
                                       else acc2
                           ) acc [(zSlice - effectiveDepth) .. zSlice]
                    ) [] surfMap

                !blankQuads =
                    [ blankTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy zSlice zSlice zoomAlpha xOffset
                    | lx ← [0 .. chunkSize - 1]
                    , ly ← [0 .. chunkSize - 1]
                    , let surfZ = surfMap VU.! columnIndex lx ly
                          terrainZ = terrainSurfMap VU.! columnIndex lx ly
                    , terrainZ > zSlice
                    , not (HM.member (lx, ly, zSlice) tileMap)
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen facing gx gy
                          drawX = rawX + xOffset
                          drawY = rawY
                    , isTileVisible vb drawX drawY
                    ]

                mkFreshwaterQuad gx gy ft fc =
                        freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                            gx gy (fcSurface fc) ft zSlice effectiveDepth
                            zoomAlpha xOffset

                (!oceanQuads, !lavaQuads, !freshwaterQuads) =
                    V.ifoldl' (\(!oAcc, !lAcc, !fAcc) idx mFluid ->
                        case mFluid of
                            Nothing -> (oAcc, lAcc, fAcc)
                            Just fc ->
                                if fcSurface fc > zSlice ∨ fcSurface fc < (zSlice - effectiveDepth)
                                then (oAcc, lAcc, fAcc)
                                else
                                    let lx = idx `mod` chunkSize
                                        ly = idx `div` chunkSize
                                        (gx, gy) = chunkToGlobal coord lx ly
                                        (rawX, rawY) = gridToScreen facing gx gy
                                        relativeZ = fcSurface fc - zSlice
                                        heightOffset = fromIntegral relativeZ * tileSideHeight
                                        drawX = rawX + xOffset
                                        drawY = rawY - heightOffset
                                    in if not (isTileVisible vb drawX drawY)
                                       then (oAcc, lAcc, fAcc)
                                       else case fcType fc of
                                            Ocean ->
                                                ( oceanTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha xOffset
                                                  : oAcc
                                                , lAcc
                                                , fAcc
                                                )
                                            Lava  ->
                                                ( oAcc
                                                , lavaTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha xOffset
                                                  : lAcc
                                                , fAcc
                                                )
                                            Lake  ->
                                                ( oAcc
                                                , lAcc
                                                , freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) Lake zSlice effectiveDepth
                                                    zoomAlpha xOffset
                                                  : fAcc
                                                )
                                            River ->
                                                ( oAcc
                                                , lAcc
                                                , freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) River zSlice effectiveDepth
                                                    zoomAlpha xOffset
                                                  : fAcc
                                                )
                    ) ([], [], []) fluidMap

            in V.fromList (realQuads <> blankQuads <> oceanQuads <> lavaQuads <> freshwaterQuads)
            ) visibleChunksWithOffset

    return $! V.concat chunkVectors
  where
    isTileVisible ∷ ViewBounds → Float → Float → Bool
    isTileVisible vb drawX drawY =
        let tileRight  = drawX + tileWidth
            tileBottom = drawY + tileHeight
        in not (tileRight  < vbLeft vb
             ∨ drawX      > vbRight vb
             ∨ tileBottom < vbTop vb
             ∨ drawY      > vbBottom vb)

-----------------------------------------------------------
-- Convert Tile to Quad
-----------------------------------------------------------

tileToQuad lookupSlot lookupFmSlot textures facing worldX worldY worldZ tile zSlice effDepth tileAlpha xOffset mFluid chunkHasFluid =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
        texHandle = getTileTexture textures (tileType tile)
        actualSlot = lookupSlot texHandle
        fmHandle = getTileFaceMapTexture textures (tileType tile) (tileSlopeId tile)
        fmSlot = lookupFmSlot fmHandle

        depth = zSlice - worldZ
        fadeRange = max 1 effDepth
        fadeT = clamp01 (fromIntegral depth / fromIntegral fadeRange)
        brightness = clamp01 (1.0 - fadeT * 0.15)

        hazeT = fadeT * fadeT * 0.6
        hazeR = 0.72 ∷ Float
        hazeG = 0.85 ∷ Float
        hazeB = 0.95 ∷ Float

        underwaterDepth = case mFluid of
            Just fc
                | fcType fc ≡ Ocean ∧ worldZ < fcSurface fc → fcSurface fc - worldZ
            _ | chunkHasFluid ∧ worldZ < seaLevel → seaLevel - worldZ
            _ → 0

        (tintR, tintG, tintB, finalAlpha) = if underwaterDepth > 0
            then
                let t = clamp01 (fromIntegral underwaterDepth / 30.0)
                    r = 0.6 - t * 0.4
                    g = 0.7 - t * 0.4
                    b = 0.9 - t * 0.3
                in (r, g, b, tileAlpha)
            else
                let r = brightness * (1.0 - hazeT) + hazeR * hazeT
                    g = brightness * (1.0 - hazeT) + hazeG * hazeT
                    b = brightness * (1.0 - hazeT) + hazeB * hazeT
                in (r, g, b, tileAlpha)

        tint = Vec4 tintR tintG tintB finalAlpha

        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Blank Tile Quad
-----------------------------------------------------------

blankTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY worldZ zSlice tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
        texHandle = wtBlankTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        depth = zSlice - worldZ
        fadeT = clamp01 (fromIntegral depth / 50.0)
        hazeT = fadeT * fadeT * 0.6
        r = 1.0 * (1.0 - hazeT) + 0.72 * hazeT
        g = 1.0 * (1.0 - hazeT) + 0.85 * hazeT
        b = 1.0 * (1.0 - hazeT) + 0.95 * hazeT

        tint = Vec4 r g b tileAlpha
        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Ocean Surface Tile Quad
-----------------------------------------------------------

oceanTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY fluidZ zSlice effDepth tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = fluidZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
                + 0.0005

        texHandle = wtOceanTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        finalAlpha = tileAlpha
        tint = Vec4 0.7 0.8 1.0 finalAlpha

        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

lavaTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY fluidZ zSlice effDepth tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = fluidZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
                + 0.0005
        texHandle = wtLavaTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)
        finalAlpha = tileAlpha
        tint = Vec4 1.0 0.6 0.2 finalAlpha
        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Freshwater (River/Lake) Surface Tile Quad
-----------------------------------------------------------

freshwaterTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY
                     fluidZ fluidType zSlice effDepth tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = fluidZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
                + 0.0005

        texHandle = wtOceanTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        finalAlpha = tileAlpha

        tint = case fluidType of
            Lake  → Vec4 0.5 0.8 0.9 finalAlpha
            River → Vec4 0.6 0.85 0.95 finalAlpha
            _     → Vec4 0.7 0.8 1.0 finalAlpha

        v0 = Vertex (Vec2 drawX drawY)
                     (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)
                     (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight))
                     (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))
                     (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
    in SortableQuad
        { sqSortKey  = sortKey
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

getTileTexture ∷ WorldTextures → Word8 → TextureHandle
getTileTexture textures 1   = wtGraniteTexture textures
getTileTexture textures 2   = wtDioriteTexture textures
getTileTexture textures 3   = wtGabbroTexture textures
getTileTexture textures 4   = wtBasaltTexture textures
getTileTexture textures 5   = wtObsidianTexture textures
getTileTexture textures 10  = wtSandstoneTexture textures
getTileTexture textures 11  = wtLimestoneTexture textures
getTileTexture textures 12  = wtShaleTexture textures
getTileTexture textures 20  = wtImpactiteTexture textures
getTileTexture textures 30  = wtIronTexture textures
getTileTexture textures 31  = wtOlivineTexture textures
getTileTexture textures 32  = wtPyroxeneTexture textures
getTileTexture textures 33  = wtFeldsparTexture textures
getTileTexture textures 100 = wtLavaTexture textures
getTileTexture textures 250 = wtGlacierTexture textures
getTileTexture textures 255 = wtBlankTexture textures
getTileTexture textures _   = wtNoTexture textures

getTileFaceMapTexture ∷ WorldTextures → Word8 → Word8 → TextureHandle
getTileFaceMapTexture textures _mat slopeId =
    case slopeToFaceMapIndex slopeId of
        0  → wtIsoFaceMap textures
        1  → wtSlopeFaceMapN textures
        2  → wtSlopeFaceMapE textures
        3  → wtSlopeFaceMapNE textures
        4  → wtSlopeFaceMapS textures
        5  → wtSlopeFaceMapNS textures
        6  → wtSlopeFaceMapES textures
        7  → wtSlopeFaceMapNES textures
        8  → wtSlopeFaceMapW textures
        9  → wtSlopeFaceMapNW textures
        10 → wtSlopeFaceMapEW textures
        11 → wtSlopeFaceMapNEW textures
        12 → wtSlopeFaceMapSW textures
        13 → wtSlopeFaceMapNSW textures
        14 → wtSlopeFaceMapESW textures
        15 → wtSlopeFaceMapNESW textures
        _  → wtIsoFaceMap textures

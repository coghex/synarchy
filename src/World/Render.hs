{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render
    ( updateWorldTiles
    , surfaceHeadroom
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import qualified Data.HashMap.Strict as HM
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
import World.Fluids (FluidCell(..), FluidType(..), seaLevel)
import World.Generate (chunkToGlobal, chunkWorldBounds, viewDepth, globalToChunk)
import World.Grid (tileWidth, tileHeight, gridToScreen, tileSideHeight, worldLayer,
                   tileHalfWidth, tileHalfDiamondHeight, zoomFadeStart, zoomFadeEnd
                   , worldToGrid, worldScreenWidth, applyFacing)
import World.ZoomMap (generateZoomMapQuads, generateBackgroundQuads)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Camera Change Detection
-----------------------------------------------------------

camEpsilon ∷ Float
camEpsilon = 0.005

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

-- | the amount of z headroom above the surface 
-- to keep the camera at, in grid units
surfaceHeadroom ∷ Int
surfaceHeadroom = 3

-----------------------------------------------------------
-- Top-Level Entry Point
-----------------------------------------------------------

updateWorldTiles ∷ EngineM ε σ (V.Vector SortableQuad)
updateWorldTiles = do
    env ← ask
    camera ← liftIO $ readIORef (cameraRef env)
    (fbW, fbH) ← liftIO $ readIORef (framebufferSizeRef env)

    let zoom = camZoom camera
        tileAlpha = clamp01 (1.0 - (zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))
        zoomAlpha = clamp01 ((zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    worldManager ← liftIO $ readIORef (worldManagerRef env)

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
                        cached ← liftIO $ readIORef (wsQuadCacheRef worldState)
                        case cached of
                            Just wqc | not (cameraChanged (wqcCamera wqc) currentSnap) →
                                return (wqcQuads wqc)
                            _ → do
                                result ← renderWorldQuads env worldState tileAlpha currentSnap
                                liftIO $ writeIORef (wsQuadCacheRef worldState) $
                                    Just (WorldQuadCache currentSnap result)
                                return result
                    Nothing → return V.empty
            return $ V.concat quads

    zoomQuads ← generateZoomMapQuads
    bgQuads ← generateBackgroundQuads

    -- Auto-adjust zSlice: always track surface when camZTracking is True,
    -- or during zoom crossfade (so zooming in always lands correctly).
    let shouldTrack = camZTracking camera
                    ∨ (tileAlpha > 0.001 ∧ tileAlpha < 0.999)
    when shouldTrack $ do
        -- re-enable tracking if we are in a crossfade
        when (not (camZTracking camera)) $
            liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZTracking = True }, ())
        worldManager' ← liftIO $ readIORef (worldManagerRef env)
        forM_ (wmVisible worldManager') $ \pageId →
            case lookup pageId (wmWorlds worldManager') of
                Just worldState → do
                    tileData ← liftIO $ readIORef (wsTilesRef worldState)
                    let (camX, camY) = camPosition camera
                        facing = camFacing camera
                        (gx, gy) = worldToGrid facing camX camY
                        (chunkCoord, (lx, ly)) = globalToChunk gx gy
                    case lookupChunk chunkCoord tileData of
                        Just lc → do
                            let surfElev = HM.lookupDefault 0 (lx, ly) (lcSurfaceMap lc)
                                targetZ = surfElev + 3
                            liftIO $ atomicModifyIORef' (cameraRef env) $ \cam →
                                (cam { camZSlice = targetZ }, ())
                        Nothing → return ()
                Nothing → return ()

    let allQuads = bgQuads <> tileQuads <> zoomQuads
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
        padX     = tileWidth
        padY     = tileHeight + maxHeightPad
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
    not (HM.null (lcSurfaceMap lc))

-----------------------------------------------------------
-- Render World Quads
-----------------------------------------------------------

renderWorldQuads ∷ EngineEnv → WorldState → Float → WorldCameraSnapshot
  → EngineM ε σ (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha snap = do
    tileData ← liftIO $ readIORef (wsTilesRef worldState)
    textures ← liftIO $ readIORef (wsTexturesRef worldState)
    paramsM ← liftIO $ readIORef (wsGenParamsRef worldState)
    camera ← liftIO $ readIORef (cameraRef env)

    let (fbW, fbH) = wcsFbSize snap
        facing = camFacing camera

    gs ← gets graphicsState
    let lookupSlot texHandle = fromIntegral $ case textureSystem gs of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral (defaultFaceMapSlot gs)
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

                !realQuads = HM.foldlWithKey'
                    (\acc (lx, ly, z) tile →
                        if z ≤ zSlice ∧ z ≥ (zSlice - effectiveDepth)
                        then let (gx, gy) = chunkToGlobal coord lx ly
                                 (rawX, rawY) = gridToScreen facing gx gy
                                 relativeZ = z - zSlice
                                 heightOffset = fromIntegral relativeZ * tileSideHeight
                                 drawX = rawX + xOffset
                                 drawY = rawY - heightOffset
                             in if isTileVisible vb drawX drawY
                                then tileToQuad lookupSlot lookupFmSlot textures facing
                                       gx gy z tile zSlice effectiveDepth zoomAlpha xOffset : acc
                                else acc
                        else acc
                    ) [] tileMap

                !blankQuads =
                    [ blankTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy zSlice zSlice zoomAlpha xOffset
                    | lx ← [0 .. chunkSize - 1]
                    , ly ← [0 .. chunkSize - 1]
                    , let surfZ = HM.lookupDefault minBound (lx, ly) surfMap
                    , surfZ > zSlice
                    , not (HM.member (lx, ly, zSlice) tileMap)
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen facing gx gy
                          drawX = rawX + xOffset
                          drawY = rawY
                    , isTileVisible vb drawX drawY
                    ]
                !oceanQuads =
                    [ oceanTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha xOffset
                    | (lx, ly) ← HM.keys fluidMap
                    , let fc = fluidMap HM.! (lx, ly)
                    -- Only render if fluid surface is within the visible z range
                    , fcSurface fc ≤ zSlice
                    , fcSurface fc ≥ (zSlice - effectiveDepth)
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen facing gx gy
                          relativeZ = fcSurface fc - zSlice
                          heightOffset = fromIntegral relativeZ * tileSideHeight
                          drawX = rawX + xOffset
                          drawY = rawY - heightOffset
                    , isTileVisible vb drawX drawY
                    ]

            in V.fromList (realQuads <> blankQuads <> oceanQuads)
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

tileToQuad ∷ (TextureHandle → Int) → (TextureHandle → Float)
           → WorldTextures → CameraFacing
           → Int → Int → Int → Tile → Int → Int → Float → Float
           → SortableQuad
tileToQuad lookupSlot lookupFmSlot textures facing worldX worldY worldZ tile zSlice effDepth tileAlpha xOffset =
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
        fmHandle = getTileFaceMapTexture textures (tileType tile)
        fmSlot = lookupFmSlot fmHandle

        -- Depth fade: tiles dissolve into the background layer
        depth = zSlice - worldZ
        fadeRange = max 1 effDepth

        -- Brightness: gentle shadow, bottoms out at 0.6
        -- so cliff material colors remain identifiable
        brightnessT = fromIntegral depth / fromIntegral fadeRange
        brightness = clamp01 (1.0 - brightnessT * 0.4)

        -- Alpha: quadratic curve — stays opaque for the top ~50%
        -- of the cliff, then accelerates into transparency.
        -- The background layer (always full brightness, correct
        -- material color) shows through at the bottom.
        fadeT = fromIntegral depth / fromIntegral fadeRange
        depthAlpha = clamp01 (1.0 - fadeT * fadeT)

        finalAlpha = tileAlpha * depthAlpha
        tint = Vec4 brightness brightness brightness finalAlpha

        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
        vertices = V.fromListN 6 [v0, v1, v2, v0, v2, v3]
    in SortableQuad
        { sqSortKey  = sortKey
        , sqVertices = vertices
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Blank Tile Quad
-----------------------------------------------------------

blankTileToQuad ∷ (TextureHandle → Int) → (TextureHandle → Float)
               → WorldTextures → CameraFacing
               → Int → Int → Int → Int → Float → Float
               → SortableQuad
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
        tint = Vec4 1.0 1.0 1.0 tileAlpha
        vertices = V.fromListN 6
            [ Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
            ]
    in SortableQuad
        { sqSortKey  = sortKey
        , sqVertices = vertices
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Ocean Surface Tile Quad
-----------------------------------------------------------

oceanTileToQuad ∷ (TextureHandle → Int) → (TextureHandle → Float)
               → WorldTextures → CameraFacing
               → Int → Int → Int → Int → Int → Float → Float
               → SortableQuad
oceanTileToQuad lookupSlot lookupFmSlot textures facing worldX worldY fluidZ zSlice effDepth tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen facing worldX worldY
        (fa, fb) = applyFacing facing worldX worldY
        relativeZ = fluidZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        -- Ocean renders slightly above terrain at same z for sort order
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001
                + 0.0005  -- sort above terrain at same z

        texHandle = wtOceanTexture textures
        actualSlot = lookupSlot texHandle
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        -- Depth fade same as terrain tiles
        depth = zSlice - fluidZ
        fadeRange = max 1 effDepth
        brightnessT = fromIntegral depth / fromIntegral fadeRange
        brightness = clamp01 (1.0 - brightnessT * 0.4)
        fadeT = fromIntegral depth / fromIntegral fadeRange
        depthAlpha = clamp01 (1.0 - fadeT * fadeT)
        finalAlpha = tileAlpha * depthAlpha

        -- Slight blue tint for ocean
        tint = Vec4 (brightness * 0.7) (brightness * 0.8) brightness finalAlpha

        v0 = Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
        v1 = Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
        v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
        v3 = Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
        vertices = V.fromListN 6 [v0, v1, v2, v0, v2, v3]
    in SortableQuad
        { sqSortKey  = sortKey
        , sqVertices = vertices
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

getTileFaceMapTexture ∷ WorldTextures → Word8 → TextureHandle
getTileFaceMapTexture textures mat
    | mat ≥ 1 ∧ mat ≤ 5   = wtIsoFaceMap textures
    | mat ≥ 10 ∧ mat ≤ 12 = wtIsoFaceMap textures
    | mat ≡ 20              = wtIsoFaceMap textures
    | mat ≥ 30 ∧ mat ≤ 33 = wtIsoFaceMap textures
    | mat ≡ 100             = wtIsoFaceMap textures
    | mat ≡ 250             = wtIsoFaceMap textures
    | otherwise              = wtNoFaceMap textures

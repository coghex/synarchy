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

-- | Threshold for quad cache invalidation.
--   Must be smaller than the view bounds padding (padX = tileWidth = 0.15)
--   but large enough to skip most per-frame rebuilds during panning.
--   Half a tile width means we regenerate ~every 8 frames at typical pan speed,
--   instead of every frame.
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

-- | the amount of z headroom above the surface 
-- to keep the camera at, in grid units
surfaceHeadroom ∷ Int
surfaceHeadroom = 3

-----------------------------------------------------------
-- Top-Level Entry Point
-----------------------------------------------------------

-- | IO version of world quad generation, called from the world thread.
--   Reads texture system from shared IORefs instead of GraphicsState.
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
    bgQuads ← generateBackgroundQuads env camera fbW fbH

    -- Auto-adjust zSlice (same logic as before)
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
                            let surfElev = HM.lookupDefault 0 (lx, ly) (lcSurfaceMap lc)
                                targetZ = surfElev + 3
                            atomicModifyIORef' (cameraRef env) $ \cam →
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
        -- Extra padding: enough to cover camera movement between cache rebuilds.
        -- camEpsilon = tileHalfWidth, so tiles within one full tile width
        -- outside the viewport are pre-generated.
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
    let wsw = tileWrapWidth worldSize
        candidates = [0, wsw, -wsw]
        dist offset = abs (chunkScreenX + offset - camX)
    in minimumBy (\ac bc → compare (dist ac) (dist bc)) candidates
  where
    minimumBy f (hd:tl) = foldl' (\best c → if f c best ≡ LT then c else best) hd tl
    minimumBy _ []       = 0

-- | Per-row wrap period: screen-X shift when gx wraps by worldTiles
tileWrapWidth ∷ Int → Float
tileWrapWidth worldSizeChunks =
    let worldTiles = worldSizeChunks * chunkSize
    in fromIntegral worldTiles * tileHalfWidth

-- | Full camera wrap period: equatorial width of the isometric map
worldScreenWidth ∷ Int → Float
worldScreenWidth worldSizeChunks = tileWrapWidth worldSizeChunks * 2.0

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
  → IO (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha snap = do
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM ← readIORef (wsGenParamsRef worldState)
    camera ← readIORef (cameraRef env)

    let (fbW, fbH) = wcsFbSize snap
        facing = camFacing camera

    -- Read from shared IORefs instead of GraphicsState
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
                chunkHasFluid = not (HM.null fluidMap)
                terrainSurfMap = lcTerrainSurfaceMap lc

                -- Iterate by column, not by individual tile.
                -- For each (lx,ly) column, look up fluid ONCE, then
                -- iterate only the z-levels that have tiles.
                !realQuads = HM.foldlWithKey'
                    (\acc (lx, ly) _surfZ →
                        let mFluid = HM.lookup (lx, ly) fluidMap
                            (gx, gy) = chunkToGlobal coord lx ly
                            (rawX, rawY) = gridToScreen facing gx gy
                            -- Also compute gridToScreen once per column
                            -- instead of once per tile
                        in foldl' (\acc2 z →
                            case HM.lookup (lx, ly, z) tileMap of
                                Nothing → acc2
                                Just tile →
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
                    , let surfZ = HM.lookupDefault minBound (lx, ly) surfMap
                          terrainZ = HM.lookupDefault minBound (lx, ly) terrainSurfMap
                    , terrainZ > zSlice
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
           → Maybe FluidCell → Bool
           → SortableQuad
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
        fmHandle = getTileFaceMapTexture textures (tileType tile)
        fmSlot = lookupFmSlot fmHandle

        -- Depth fade (cliff shading for above-water tiles)
        depth = zSlice - worldZ
        fadeRange = max 1 effDepth
        brightnessT = fromIntegral depth / fromIntegral fadeRange
        brightness = clamp01 (1.0 - brightnessT * 0.4)
        fadeT = fromIntegral depth / fromIntegral fadeRange
        depthAlpha = clamp01 (1.0 - fadeT * fadeT)
        -- Determine if this tile is underwater and by how much.
        -- Two cases:
        --   1. This column has fluid and the tile is below the fluid surface
        --   2. This column has no fluid (cliff/land column) but the tile
        --      is below sea level AND the chunk contains fluid elsewhere
        underwaterDepth = case mFluid of
            Just fc
                | worldZ < fcSurface fc → fcSurface fc - worldZ
            _ | chunkHasFluid ∧ worldZ < seaLevel → seaLevel - worldZ
            _ → 0

        -- Underwater vs above-water tinting
        (tintR, tintG, tintB, finalAlpha) = if underwaterDepth > 0
            then
                -- Underwater: tint by water depth only.
                -- Ignore cliff depth shading — water depth is the visual cue.
                let t = clamp01 (fromIntegral underwaterDepth / 30.0)
                    r = 0.6 - t * 0.4       -- 0.6 → 0.2
                    g = 0.7 - t * 0.4        -- 0.7 → 0.3
                    b = 0.9 - t * 0.3        -- 0.9 → 0.6
                in (r, g, b, tileAlpha)
            else
                (brightness, brightness, brightness, tileAlpha * depthAlpha)

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
        -- Blank tiles are always neutral grey — they represent
        -- unexposed solid ground, never underwater surfaces.
        tint = Vec4 1.0 1.0 1.0 tileAlpha
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
        -- Use the default face map directly (1×1 green = pure top-facing)
        -- This bypasses lookupFmSlot which might resolve wtNoFaceMap
        -- to a wrong slot. Ocean is flat — it should always be top-lit.
        fmSlot = lookupFmSlot (wtIsoFaceMap textures)

        -- No depth fade for ocean surface — it should be fully bright
        -- at its surface level, not dimmed by cliff shading
        finalAlpha = tileAlpha

        -- Blue tint for ocean, full brightness (shader handles sun lighting)
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

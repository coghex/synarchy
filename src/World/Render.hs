{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render
    ( updateWorldTiles
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle
import Engine.Asset.Manager
import Engine.Asset.Base
import Engine.Asset.Types (AssetPool(..), TextureAtlas(..))
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Monad (EngineM)
import Engine.Core.Log (LogCategory(..), logInfo, logWarn)
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Types (RenderBatch(..), SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import World.Generate (chunkSize, chunkToGlobal, chunkWorldBounds, viewDepth
                      , globalToChunk)
import World.Grid (tileWidth, tileHeight, gridToScreen, tileSideHeight, worldLayer,
                   tileHalfWidth, tileHalfDiamondHeight, zoomFadeStart, zoomFadeEnd
                   , worldToGrid, zoomFadeStart, zoomFadeEnd)
import World.ZoomMap (generateZoomMapQuads, generateBackgroundQuads)
import qualified Data.Vector as V

-----------------------------------------------------------
-- World Screen Width (wrapping period in screen-space X)
-----------------------------------------------------------

worldScreenWidth ∷ Float
worldScreenWidth =
    let worldSizeChunks = 128
        worldTiles = worldSizeChunks * chunkSize
    in fromIntegral worldTiles * tileHalfWidth

updateWorldTiles ∷ EngineM ε σ (V.Vector SortableQuad)
updateWorldTiles = do
    env ← ask
    logger ← liftIO $ readIORef (loggerRef env)
    camera ← liftIO $ readIORef (cameraRef env)

    let zoom = camZoom camera
        tileAlpha = clamp01 (1.0 - (zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    worldManager ← liftIO $ readIORef (worldManagerRef env)

    tileQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            quads ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState → renderWorldQuads env worldState tileAlpha
                    Nothing         → return V.empty
            return $ V.concat quads

    zoomQuads ← generateZoomMapQuads
    bgQuads   ← generateBackgroundQuads

    when (tileAlpha > 0.001 ∧ tileAlpha < 0.999) $ do
        worldManager' ← liftIO $ readIORef (worldManagerRef env)
        forM_ (wmVisible worldManager') $ \pageId →
            case lookup pageId (wmWorlds worldManager') of
                Just worldState → do
                    tileData ← liftIO $ readIORef (wsTilesRef worldState)
                    let (camX, camY) = camPosition camera
                        (gx, gy) = worldToGrid camX camY
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
-- View Bounds (for culling)
-----------------------------------------------------------

data ViewBounds = ViewBounds
    { vbLeft   ∷ !Float
    , vbRight  ∷ !Float
    , vbTop    ∷ !Float
    , vbBottom ∷ !Float
    } deriving (Show)

computeViewBounds ∷ Camera2D → Int → Int → ViewBounds
computeViewBounds camera fbW fbH =
    let (cx, cy) = camPosition camera
        zoom     = camZoom camera
        aspect   = fromIntegral fbW / fromIntegral fbH
        halfW    = zoom * aspect
        halfH    = zoom
        maxHeightPad = fromIntegral viewDepth * tileSideHeight
        padX     = tileWidth
        padY     = tileHeight + maxHeightPad
    in ViewBounds
        { vbLeft   = cx - halfW - padX
        , vbRight  = cx + halfW + padX
        , vbTop    = cy - halfH - padY
        , vbBottom = cy + halfH + padY
        }

-----------------------------------------------------------
-- Chunk-Level Culling (wrap-aware)
-----------------------------------------------------------

bestWrapOffset ∷ Float → Float → Float
bestWrapOffset camX chunkScreenX =
    let wsw = worldScreenWidth
        candidates = [0, wsw, -wsw]
        dist offset = abs (chunkScreenX + offset - camX)
    in minimumBy (\a b → compare (dist a) (dist b)) candidates
  where
    minimumBy f (x:xs) = foldl' (\best c → if f c best ≡ LT then c else best) x xs
    minimumBy _ []      = 0

isChunkVisibleWrapped ∷ ViewBounds → Float → ChunkCoord → Maybe Float
isChunkVisibleWrapped vb camX coord =
    let ((minGX, minGY), (maxGX, maxGY)) = chunkWorldBounds coord
        (sxMin, _) = gridToScreen minGX maxGY
        (sxMax, _) = gridToScreen maxGX minGY
        (_, syMin) = gridToScreen minGX minGY
        (_, syMax) = gridToScreen maxGX maxGY

        chunkCenterX = (sxMin + sxMax + tileWidth) / 2.0
        offset = bestWrapOffset camX chunkCenterX

        chunkLeft   = sxMin + offset
        chunkRight  = sxMax + tileWidth + offset
        chunkTop    = syMin
        chunkBottom = syMax + tileHeight

        visible = not (chunkRight  < vbLeft vb
                    ∨ chunkLeft   > vbRight vb
                    ∨ chunkBottom < vbTop vb
                    ∨ chunkTop    > vbBottom vb)
    in if visible then Just offset else Nothing

-- | Now checks whether a chunk has any column whose surface
--   is at or below the z-slice (meaning it could have real tiles)
--   OR any column whose surface is above the z-slice (blank fill needed).
--   In practice: any chunk with non-empty surface map is potentially visible.
isChunkRelevantForSlice ∷ Int → LoadedChunk → Bool
isChunkRelevantForSlice _zSlice lc =
    not (HM.null (lcSurfaceMap lc))

-----------------------------------------------------------
-- Tile-Level Culling
-----------------------------------------------------------

isTileVisible ∷ ViewBounds → Float → Float → Bool
isTileVisible vb drawX drawY =
    let tileRight  = drawX + tileWidth
        tileBottom = drawY + tileHeight
    in not (tileRight  < vbLeft vb
         ∨ drawX      > vbRight vb
         ∨ tileBottom < vbTop vb
         ∨ drawY      > vbBottom vb)

-----------------------------------------------------------
-- Render World Quads
-----------------------------------------------------------

renderWorldQuads ∷ EngineEnv → WorldState → Float
  → EngineM ε σ (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha = do
    tileData ← liftIO $ readIORef (wsTilesRef worldState)
    textures ← liftIO $ readIORef (wsTexturesRef worldState)
    logger ← liftIO $ readIORef (loggerRef env)
    camera ← liftIO $ readIORef (cameraRef env)
    
    (fbW, fbH) ← liftIO $ readIORef (framebufferSizeRef env)
    
    -- Look up graphics state ONCE for the whole frame
    gs ← gets graphicsState
    let lookupSlot texHandle = fromIntegral $ case textureSystem gs of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral (defaultFaceMapSlot gs)
        lookupFmSlot texHandle =
            let s = lookupSlot texHandle
            in if s ≡ 0 then defFmSlot else fromIntegral s
    
    let vb = computeViewBounds camera fbW fbH
        zSlice = camZSlice camera
        chunks = HM.elems (wtdChunks tileData)
        (camX, _camY) = camPosition camera
        
        visibleChunksWithOffset =
            [ (lc, offset)
            | lc ← chunks
            , isChunkRelevantForSlice zSlice lc
            , Just offset ← [isChunkVisibleWrapped vb camX (lcCoord lc)]
            ]
    
    let chunkQuads = concatMap (\(lc, xOffset) →
            let coord = lcCoord lc
                tileMap = lcTiles lc
                surfMap = lcSurfaceMap lc
                
                realQuads =
                    [ tileToQuad lookupSlot lookupFmSlot textures gx gy z tile zSlice zoomAlpha xOffset
                    | ((lx, ly, z), tile) ← HM.toList tileMap
                    , z ≤ zSlice ∧ z ≥ (zSlice - viewDepth)
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen gx gy
                          relativeZ = z - zSlice
                          heightOffset = fromIntegral relativeZ * tileSideHeight
                          drawX = rawX + xOffset
                          drawY = rawY - heightOffset
                    , isTileVisible vb drawX drawY
                    ]
                
                blankQuads =
                    [ blankTileToQuad lookupSlot lookupFmSlot textures gx gy zSlice zSlice zoomAlpha xOffset
                    | lx ← [0 .. chunkSize - 1]
                    , ly ← [0 .. chunkSize - 1]
                    , let surfZ = HM.lookupDefault minBound (lx, ly) surfMap
                    , surfZ > zSlice
                    , not (HM.member (lx, ly, zSlice) tileMap)
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen gx gy
                          drawX = rawX + xOffset
                          drawY = rawY
                    , isTileVisible vb drawX drawY
                    ]
            
            in realQuads <> blankQuads
            ) visibleChunksWithOffset
    
    return $ V.fromList chunkQuads
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
           → WorldTextures
           → Int → Int → Int → Tile → Int → Float → Float
           → SortableQuad
tileToQuad lookupSlot lookupFmSlot textures worldX worldY worldZ tile zSlice tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen worldX worldY
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (worldX + worldY) 
                + fromIntegral relativeZ * 0.001
        texHandle = getTileTexture textures (tileType tile)
        actualSlot = lookupSlot texHandle
        fmHandle = getTileFaceMapTexture textures (tileType tile)
        fmSlot = lookupFmSlot fmHandle
        depth = zSlice - worldZ
        brightness = clamp01 (1.0 - fromIntegral depth * 0.12)
        tint = Vec4 brightness brightness brightness tileAlpha
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
-- Blank Tile Quad (fills gaps in solid interior)
-----------------------------------------------------------

-- | Render a blank tile at the z-slice for columns where the surface
--   is above the slice but no real tile exists at that elevation.
blankTileToQuad ∷ (TextureHandle → Int) → (TextureHandle → Float)
               → WorldTextures
               → Int → Int → Int → Int → Float → Float
               → SortableQuad
blankTileToQuad lookupSlot lookupFmSlot textures worldX worldY worldZ zSlice tileAlpha xOffset =
    let (rawX, rawY) = gridToScreen worldX worldY
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset
        sortKey = fromIntegral (worldX + worldY)
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
-- Helpers
-----------------------------------------------------------

clamp01 ∷ Float → Float
clamp01 x
    | x < 0    = 0
    | x > 1    = 1
    | otherwise = x

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
    | mat ≥ 1 ∧ mat ≤ 5   = wtIsoFaceMap textures  -- all igneous
    | mat ≥ 10 ∧ mat ≤ 12 = wtIsoFaceMap textures  -- sedimentary
    | mat ≡ 20              = wtIsoFaceMap textures  -- impactite
    | mat ≥ 30 ∧ mat ≤ 33 = wtIsoFaceMap textures  -- meteorite
    | mat ≡ 100             = wtIsoFaceMap textures  -- lava
    | mat ≡ 250             = wtIsoFaceMap textures  -- glacier
    | otherwise              = wtNoFaceMap textures

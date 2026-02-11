{-# LANGUAGE Strict #-}
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
import Engine.Core.Log (logDebug, LogCategory(..), logInfo, logWarn)
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Types (RenderBatch(..), SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import World.Generate (chunkSize, chunkToGlobal, chunkWorldBounds, viewDepth)
import World.Grid (tileWidth, tileHeight, gridToScreen, tileSideHeight, worldLayer,
                   tileHalfWidth, tileHalfDiamondHeight, zoomFadeStart, zoomFadeEnd
                   , worldToGrid, zoomFadeStart, zoomFadeEnd)
import World.Plate (generatePlates, elevationAtGlobal)
import World.ZoomMap (generateZoomMapQuads, generateBackgroundQuads)
import qualified Data.Vector as V

-----------------------------------------------------------
-- World Screen Width (wrapping period in screen-space X)
-----------------------------------------------------------

worldScreenWidth :: Float
worldScreenWidth =
    let worldSizeChunks = 128
        worldTiles = worldSizeChunks * chunkSize
    in fromIntegral worldTiles * tileHalfWidth

-----------------------------------------------------------
-- Update World Tiles (called before scene rendering)
-----------------------------------------------------------

updateWorldTiles :: EngineM ε σ (V.Vector SortableQuad)
updateWorldTiles = do
    env <- ask
    logger <- liftIO $ readIORef (loggerRef env)
    camera <- liftIO $ readIORef (cameraRef env)

    let zoom = camZoom camera
        tileAlpha = clamp01 (1.0 - (zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    worldManager <- liftIO $ readIORef (worldManagerRef env)

    bgQuads <- generateBackgroundQuads

    tileQuads <- if tileAlpha <= 0.001
        then return V.empty
        else do
            quads <- forM (wmVisible worldManager) $ \pageId ->
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState -> renderWorldQuads env worldState tileAlpha
                    Nothing         -> return V.empty
            return $ V.concat quads

    zoomQuads <- generateZoomMapQuads

    when (tileAlpha > 0.001 && tileAlpha < 0.999) $ do
        worldManager' <- liftIO $ readIORef (worldManagerRef env)
        forM_ (wmVisible worldManager') $ \pageId ->
            case lookup pageId (wmWorlds worldManager') of
                Just worldState -> do
                    mParams <- liftIO $ readIORef (wsGenParamsRef worldState)
                    case mParams of
                        Just params -> do
                            let seed = wgpSeed params
                                worldSize = wgpWorldSize params
                                plates = generatePlates seed (wgpWorldSize params) (wgpPlateCount params)
                                (camX, camY) = camPosition camera
                                (gx, gy) = worldToGrid camX camY
                                (surfElev, _) = elevationAtGlobal seed plates worldSize gx gy
                                targetZ = surfElev + 3
                            liftIO $ atomicModifyIORef' (cameraRef env) $ \cam ->
                                (cam { camZSlice = targetZ }, ())
                        Nothing -> return ()
                Nothing -> return ()

    let allQuads = bgQuads <> tileQuads <> zoomQuads
    liftIO $ logDebug logger CatWorld $
        "Generated " <> T.pack (show $ V.length bgQuads) <> " background quads, "
        <> T.pack (show $ V.length tileQuads) <> " tile quads, "
        <> T.pack (show $ V.length zoomQuads) <> " zoom quads"

    return allQuads

-----------------------------------------------------------
-- View Bounds (for culling)
-----------------------------------------------------------

data ViewBounds = ViewBounds
    { vbLeft   :: !Float
    , vbRight  :: !Float
    , vbTop    :: !Float
    , vbBottom :: !Float
    } deriving (Show)

computeViewBounds :: Camera2D -> Int -> Int -> ViewBounds
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

bestWrapOffset :: Float -> Float -> Float
bestWrapOffset camX chunkScreenX =
    let wsw = worldScreenWidth
        candidates = [0, wsw, -wsw]
        dist offset = abs (chunkScreenX + offset - camX)
    in minimumBy (\a b -> compare (dist a) (dist b)) candidates
  where
    minimumBy f (x:xs) = foldl' (\best c -> if f c best == LT then c else best) x xs
    minimumBy _ []      = 0

isChunkVisibleWrapped :: ViewBounds -> Float -> ChunkCoord -> Maybe Float
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
                    || chunkLeft   > vbRight vb
                    || chunkBottom < vbTop vb
                    || chunkTop    > vbBottom vb)
    in if visible then Just offset else Nothing

-- | Now checks whether a chunk has any column whose surface
--   is at or below the z-slice (meaning it could have real tiles)
--   OR any column whose surface is above the z-slice (blank fill needed).
--   In practice: any chunk with non-empty surface map is potentially visible.
isChunkRelevantForSlice :: Int -> LoadedChunk -> Bool
isChunkRelevantForSlice _zSlice lc =
    not (HM.null (lcSurfaceMap lc))

-----------------------------------------------------------
-- Tile-Level Culling
-----------------------------------------------------------

isTileVisible :: ViewBounds -> Float -> Float -> Bool
isTileVisible vb drawX drawY =
    let tileRight  = drawX + tileWidth
        tileBottom = drawY + tileHeight
    in not (tileRight  < vbLeft vb
         || drawX      > vbRight vb
         || tileBottom < vbTop vb
         || drawY      > vbBottom vb)

-----------------------------------------------------------
-- Render World Quads
-----------------------------------------------------------

renderWorldQuads :: EngineEnv -> WorldState -> Float
  -> EngineM ε σ (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha = do
    tileData <- liftIO $ readIORef (wsTilesRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    logger <- liftIO $ readIORef (loggerRef env)
    camera <- liftIO $ readIORef (cameraRef env)
    
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    
    let vb = computeViewBounds camera fbW fbH
        zSlice = camZSlice camera
        chunks = wtdChunks tileData
        (camX, _camY) = camPosition camera
        
        visibleChunksWithOffset =
            [ (lc, offset)
            | lc <- chunks
            , isChunkRelevantForSlice zSlice lc
            , Just offset <- [isChunkVisibleWrapped vb camX (lcCoord lc)]
            ]
    
    liftIO $ logDebug logger CatWorld $
        "Chunk culling: " <> T.pack (show $ length chunks) <> " loaded, "
        <> T.pack (show $ length visibleChunksWithOffset) <> " visible"
        <> " (zSlice=" <> T.pack (show zSlice) <> ")"
    
    chunkQuads <- forM visibleChunksWithOffset $ \(lc, xOffset) -> do
        let coord = lcCoord lc
            tileMap = lcTiles lc
            surfMap = lcSurfaceMap lc
            tileList = HM.toList tileMap
            
            -- Real tiles in the z-slice window
            visibleTiles = filter (\t -> isTileInView vb coord zSlice xOffset t
                                      && tileInSlice zSlice t) tileList
        
        realQuads <- forM visibleTiles $ \((lx, ly, z), tile) -> do
            let (gx, gy) = chunkToGlobal coord lx ly
            tileToQuad env textures gx gy z tile zSlice zoomAlpha xOffset

        -- Blank fill tiles: for every column where surfaceZ > zSlice
        -- and there's no real tile at the zSlice level, emit a blank quad.
        let blankTiles =
                [ (lx, ly)
                | lx <- [0 .. chunkSize - 1]
                , ly <- [0 .. chunkSize - 1]
                , let surfZ = HM.lookupDefault minBound (lx, ly) surfMap
                -- Surface is above the z-slice (column is solid here)
                , surfZ > zSlice
                -- No real tile stored at this z level
                , not (HM.member (lx, ly, zSlice) tileMap)
                -- Check if it's on screen
                , let (gx, gy) = chunkToGlobal coord lx ly
                      (rawX, rawY) = gridToScreen gx gy
                      drawX = rawX + xOffset
                      drawY = rawY  -- relativeZ = zSlice - zSlice = 0
                , isTileVisible vb drawX drawY
                ]

        blankQuads <- forM blankTiles $ \(lx, ly) -> do
            let (gx, gy) = chunkToGlobal coord lx ly
            blankTileToQuad env textures gx gy zSlice zSlice zoomAlpha xOffset
        
        return $ V.fromList realQuads <> V.fromList blankQuads
    
    return $ V.concat chunkQuads
  where
    isTileInView :: ViewBounds -> ChunkCoord -> Int -> Float
                 -> ((Int, Int, Int), Tile) -> Bool
    isTileInView vb coord zSlice' xOffset ((lx, ly, z), _) =
        let (gx, gy) = chunkToGlobal coord lx ly
            (rawX, rawY) = gridToScreen gx gy
            relativeZ = z - zSlice'
            heightOffset = fromIntegral relativeZ * tileSideHeight
            drawX = rawX + xOffset
            drawY = rawY - heightOffset
        in isTileVisible vb drawX drawY
    tileInSlice :: Int -> ((Int, Int, Int), Tile) -> Bool
    tileInSlice zSlice ((_, _, z), _) = z <= zSlice && z >= (zSlice - viewDepth)

-----------------------------------------------------------
-- Convert Tile to Quad
-----------------------------------------------------------

tileToQuad :: EngineEnv -> WorldTextures
  -> Int -> Int -> Int -> Tile -> Int -> Float -> Float
           -> EngineM ε σ SortableQuad
tileToQuad env textures worldX worldY worldZ tile zSlice tileAlpha xOffset = do
    logger <- liftIO $ readIORef (loggerRef env)
    
    let (rawX, rawY) = gridToScreen worldX worldY
        
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset

        sortKey = fromIntegral (worldX + worldY) 
                + fromIntegral relativeZ * 0.001

    let texHandle = getTileTexture textures (tileType tile)
    
    gs <- gets graphicsState
    let actualSlot = case textureSystem gs of
          Just bindless -> getTextureSlotIndex texHandle bindless
          Nothing       -> 0

    let fmHandle = getTileFaceMapTexture textures (tileType tile)
        fmSlot = case textureSystem gs of
          Just bindless ->
            let s = getTextureSlotIndex fmHandle bindless
            in if s == 0
               then fromIntegral (defaultFaceMapSlot gs)
               else fromIntegral s
          Nothing -> fromIntegral (defaultFaceMapSlot gs)

    let depth = zSlice - worldZ
        brightness = clamp01 (1.0 - fromIntegral depth * 0.12)
        tint = Vec4 brightness brightness brightness tileAlpha

    liftIO $ logDebug logger CatWorld $ 
        "TILE RENDER: texHandle=" <> T.pack (show texHandle)
        <> " slot=" <> T.pack (show actualSlot)
        <> " faceMap=" <> T.pack (show fmSlot)
        
    let vertices = V.fromList
            [ Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
            ]
    
    return $ SortableQuad
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
blankTileToQuad :: EngineEnv -> WorldTextures
  -> Int -> Int -> Int -> Int -> Float -> Float
  -> EngineM ε σ SortableQuad
blankTileToQuad env textures worldX worldY worldZ zSlice tileAlpha xOffset = do
    let (rawX, rawY) = gridToScreen worldX worldY
        
        relativeZ = worldZ - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight
        drawX = rawX + xOffset
        drawY = rawY - heightOffset

        sortKey = fromIntegral (worldX + worldY) 
                + fromIntegral relativeZ * 0.001

    let texHandle = wtBlankTexture textures
    
    gs <- gets graphicsState
    let actualSlot = case textureSystem gs of
          Just bindless -> getTextureSlotIndex texHandle bindless
          Nothing       -> 0

    -- Use the isometric face map for the blank tile so it
    -- has the same diamond shape as real tiles
    let fmHandle = wtIsoFaceMap textures
        fmSlot = case textureSystem gs of
          Just bindless ->
            let s = getTextureSlotIndex fmHandle bindless
            in if s == 0
               then fromIntegral (defaultFaceMapSlot gs)
               else fromIntegral s
          Nothing -> fromIntegral (defaultFaceMapSlot gs)

    let tint = Vec4 1.0 1.0 1.0 tileAlpha

    let vertices = V.fromList
            [ Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX drawY)                              (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            , Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot
            ]
    
    return $ SortableQuad
        { sqSortKey  = sortKey
        , sqVertices = vertices
        , sqTexture  = texHandle
        , sqLayer    = worldLayer
        }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

clamp01 :: Float -> Float
clamp01 x
    | x < 0    = 0
    | x > 1    = 1
    | otherwise = x

getTileTexture :: WorldTextures -> Word8 -> TextureHandle
getTileTexture textures 1 = wtGraniteTexture textures
getTileTexture textures 2 = wtGabbroTexture textures
getTileTexture textures 3 = wtDioriteTexture textures
getTileTexture textures 250 = wtGlacierTexture textures
getTileTexture textures 255 = wtBlankTexture textures
getTileTexture textures _ = wtNoTexture textures

getTileFaceMapTexture :: WorldTextures -> Word8 -> TextureHandle
getTileFaceMapTexture textures mat
    | mat >= 1 && mat <= 3 = wtIsoFaceMap textures
    | mat == 250           = wtIsoFaceMap textures
    | otherwise            = wtNoFaceMap textures

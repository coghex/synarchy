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
import Data.IORef (readIORef)
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
import World.Generate (chunkSize, chunkToGlobal, chunkWorldBounds)
import World.Grid (tileWidth, tileHeight, gridToScreen, tileSideHeight, worldLayer,
                   tileHalfWidth, tileHalfDiamondHeight)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Update World Tiles (called before scene rendering)
-----------------------------------------------------------

updateWorldTiles :: EngineM ε σ (V.Vector SortableQuad)
updateWorldTiles = do
    env <- ask
    logger <- liftIO $ readIORef (loggerRef env)
    
    worldManager <- liftIO $ readIORef (worldManagerRef env)
    
    quads <- forM (wmVisible worldManager) $ \pageId ->
        case lookup pageId (wmWorlds worldManager) of
            Just worldState -> renderWorldQuads env worldState
            Nothing         -> return V.empty
    
    let allQuads = V.concat quads
    liftIO $ logDebug logger CatWorld $ 
        "Generated " <> T.pack (show $ V.length allQuads) <> " world tile quads"
    
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
        -- Extra padding to account for height offset of elevated tiles.
        -- A tile at max z-slice shifts up by zSlice * tileSideHeight,
        -- so it could be visible even if its base grid position is
        -- below the view bottom.
        zSlice   = camZSlice camera
        maxHeightPad = fromIntegral (abs zSlice) * tileSideHeight
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

-- | Test whether a chunk's bounding box overlaps the view.
isChunkVisible :: ViewBounds -> ChunkCoord -> Bool
isChunkVisible vb coord =
    let ((minGX, minGY), (maxGX, maxGY)) = chunkWorldBounds coord
        (sxMin, _) = gridToScreen minGX maxGY
        (sxMax, _) = gridToScreen maxGX minGY
        chunkLeft  = sxMin
        chunkRight = sxMax + tileWidth
        (_, syMin) = gridToScreen minGX minGY
        (_, syMax) = gridToScreen maxGX maxGY
        chunkTop    = syMin
        chunkBottom = syMax + tileHeight
    in not (chunkRight  < vbLeft vb
         || chunkLeft   > vbRight vb
         || chunkBottom < vbTop vb
         || chunkTop    > vbBottom vb)

-- | Test whether a chunk has ANY tiles at or below the z-slice.
--   If the minimum z in the chunk is above the slice, skip it entirely.
isChunkBelowSlice :: Int -> LoadedChunk -> Bool
isChunkBelowSlice zSlice lc =
    -- Find the minimum z in this chunk.
    -- If even the lowest tile is above the slice, the whole chunk is hidden.
    case HM.keys (lcTiles lc) of
        [] -> False
        keys -> let minZ = minimum [ z | (_, _, z) <- keys ]
                in minZ <= zSlice

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

renderWorldQuads :: EngineEnv -> WorldState -> EngineM ε σ (V.Vector SortableQuad)
renderWorldQuads env worldState = do
    tileData <- liftIO $ readIORef (wsTilesRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    logger <- liftIO $ readIORef (loggerRef env)
    camera <- liftIO $ readIORef (cameraRef env)
    
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    
    let vb = computeViewBounds camera fbW fbH
        zSlice = camZSlice camera
        chunks = wtdChunks tileData
        
        -- Chunk-level culling: XY bounds + z-slice
        visibleChunks = filter (\lc -> isChunkVisible vb (lcCoord lc)
                                    && isChunkBelowSlice zSlice lc) chunks
    
    liftIO $ logDebug logger CatWorld $
        "Chunk culling: " <> T.pack (show $ length chunks) <> " loaded, "
        <> T.pack (show $ length visibleChunks) <> " visible"
        <> " (zSlice=" <> T.pack (show zSlice) <> ")"
    
    -- For each visible chunk, extract visible tiles and build quads
    chunkQuads <- forM visibleChunks $ \lc -> do
        let coord = lcCoord lc
            tileList = HM.toList (lcTiles lc)
            
            -- Filter tiles: XY visibility AND z <= zSlice
            visibleTiles = filter (\t -> isTileInView vb coord t
                                      && tileInSlice zSlice t) tileList
        
        quads <- forM visibleTiles $ \((lx, ly, z), tile) -> do
            let (gx, gy) = chunkToGlobal coord lx ly
            tileToQuad env textures gx gy z tile zSlice
        
        return $ V.fromList quads
    
    return $ V.concat chunkQuads
  where
    isTileInView :: ViewBounds -> ChunkCoord -> ((Int, Int, Int), Tile) -> Bool
    isTileInView vb coord ((lx, ly, z), _) =
        let (gx, gy) = chunkToGlobal coord lx ly
            (rawX, rawY) = gridToScreen gx gy
            heightOffset = fromIntegral z * tileSideHeight
            drawX = rawX
            drawY = rawY - heightOffset
        in isTileVisible vb drawX drawY
    
    tileInSlice :: Int -> ((Int, Int, Int), Tile) -> Bool
    tileInSlice zSlice ((_, _, z), _) = z <= zSlice

-----------------------------------------------------------
-- Convert Tile to Quad
-----------------------------------------------------------

tileToQuad :: EngineEnv -> WorldTextures
  -> Int -> Int -> Int -> Tile -> Int
           -> EngineM ε σ SortableQuad
tileToQuad env textures worldX worldY worldZ tile zSlice = do
    logger <- liftIO $ readIORef (loggerRef env)
    
    let (rawX, rawY) = gridToScreen worldX worldY
        
        heightOffset = fromIntegral worldZ * tileSideHeight
        drawX = rawX
        drawY = rawY - heightOffset

        sortKey = fromIntegral (worldX + worldY) 
                + fromIntegral worldZ * 0.001

    let texHandle = getTileTexture textures (tileType tile)
    
    gs <- gets graphicsState
    let actualSlot = case textureSystem gs of
          Just bindless -> getTextureSlotIndex texHandle bindless
          Nothing       -> 0

    -- Look up the face map for this tile type
    let fmHandle = getTileFaceMapTexture textures (tileType tile)
        fmSlot = case textureSystem gs of
          Just bindless ->
            let s = getTextureSlotIndex fmHandle bindless
            in if s == 0
               -- Handle not registered yet, fall back to default face map
               then fromIntegral (defaultFaceMapSlot gs)
               else fromIntegral s
          Nothing -> fromIntegral (defaultFaceMapSlot gs)

    -- Depth tint: tiles at the z-slice are full brightness,
    -- tiles further below get progressively darker
    let depth = zSlice - worldZ  -- 0 at slice, positive going deeper
        brightness = clamp01 (1.0 - fromIntegral depth * 0.12)
        tint = Vec4 brightness brightness brightness 1

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
-- Helpers
-----------------------------------------------------------

-- | Clamp a float to [0, 1]
clamp01 :: Float -> Float
clamp01 x
    | x < 0    = 0
    | x > 1    = 1
    | otherwise = x

getTileTexture :: WorldTextures -> Word8 -> TextureHandle
getTileTexture textures 1 = wtGraniteTexture textures  -- grass
getTileTexture _        _ = TextureHandle 0

getTileFaceMapTexture :: WorldTextures -> Word8 -> TextureHandle
getTileFaceMapTexture textures 1 = wtIsoFaceMap textures  -- grass face map
getTileFaceMapTexture _        _ = TextureHandle 0

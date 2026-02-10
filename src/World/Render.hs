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
    liftIO $ logDebug logger CatSystem $ 
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
        padX     = tileWidth
        padY     = tileHeight
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
--   Uses the chunk's global tile range converted to world-space.
isChunkVisible :: ViewBounds -> ChunkCoord -> Bool
isChunkVisible vb coord =
    let ((minGX, minGY), (maxGX, maxGY)) = chunkWorldBounds coord
        -- Get world-space bounds of the chunk's tile quads.
        -- gridToScreen gives top-left of each tile, so we need:
        --   leftmost  = gridToScreen of the tile with smallest screen X
        --   rightmost = gridToScreen of the tile with largest screen X + tileWidth
        -- In isometric coords, screen X = (gx - gy) * halfWidth
        -- so min screen X comes from (minGX, maxGY) and max from (maxGX, minGY)
        (sxMin, _) = gridToScreen minGX maxGY
        (sxMax, _) = gridToScreen maxGX minGY
        chunkLeft  = sxMin
        chunkRight = sxMax + tileWidth
        -- Screen Y = (gx + gy) * halfDiamondHeight
        -- min screen Y from (minGX, minGY), max from (maxGX, maxGY)
        (_, syMin) = gridToScreen minGX minGY
        (_, syMax) = gridToScreen maxGX maxGY
        chunkTop    = syMin
        chunkBottom = syMax + tileHeight
    in not (chunkRight  < vbLeft vb
         || chunkLeft   > vbRight vb
         || chunkBottom < vbTop vb
         || chunkTop    > vbBottom vb)

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
        chunks = wtdChunks tileData
        
        -- Chunk-level culling
        visibleChunks = filter (isChunkVisible vb . lcCoord) chunks
    
    liftIO $ logDebug logger CatSystem $
        "Chunk culling: " <> T.pack (show $ length chunks) <> " loaded, "
        <> T.pack (show $ length visibleChunks) <> " visible"
    
    -- For each visible chunk, extract visible tiles and build quads
    chunkQuads <- forM visibleChunks $ \lc -> do
        let coord = lcCoord lc
            tileList = HM.toList (lcTiles lc)
            
            -- Filter tiles within this chunk by visibility
            visibleTiles = filter (isTileInView vb coord) tileList
        
        quads <- forM visibleTiles $ \((lx, ly, z), tile) -> do
            let (gx, gy) = chunkToGlobal coord lx ly
            tileToQuad env textures gx gy z tile
        
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

-----------------------------------------------------------
-- Convert Tile to Quad
-----------------------------------------------------------

tileToQuad :: EngineEnv -> WorldTextures
  -> Int -> Int -> Int -> Tile 
           -> EngineM ε σ SortableQuad
tileToQuad env textures worldX worldY worldZ tile = do
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

    liftIO $ logDebug logger CatSystem $ 
        "TILE RENDER: texHandle=" <> T.pack (show texHandle)
        <> " slot=" <> T.pack (show actualSlot)
        
    let vertices = V.fromList
            [ Vertex (Vec2 drawX drawY)                              (Vec2 0 0) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 (drawX + tileWidth) drawY)                (Vec2 1 0) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 drawX drawY)                              (Vec2 0 0) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight)) (Vec2 1 1) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 drawX (drawY + tileHeight))               (Vec2 0 1) (Vec4 1 1 1 1) (fromIntegral actualSlot)
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

getTileTexture :: WorldTextures -> Word8 -> TextureHandle
getTileTexture textures 1 = wtGrassTexture textures  -- grass
getTileTexture _        _ = TextureHandle 0

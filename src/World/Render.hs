{-# LANGUAGE Strict #-}
module World.Render
    ( updateWorldTiles
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef)
import Engine.Asset.Handle
import Engine.Asset.Manager
import Engine.Asset.Base
import Engine.Asset.Types (AssetPool(..), TextureAtlas(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Monad (EngineM)
import Engine.Core.Log (logDebug, LogCategory(..), logInfo, logWarn)
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Types (RenderBatch(..), SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import World.Grid (tileWidth, tileHeight, gridToScreen, tileSideHeight, worldLayer)
import qualified Data.Vector as V

-----------------------------------------------------------
-- Update World Tiles (called before scene rendering)
-----------------------------------------------------------

-- | Generate quads for all visible world tiles,
--   sorted by depth for painter's algorithm
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

unWorldPageId :: WorldPageId -> Text
unWorldPageId (WorldPageId t) = t

-----------------------------------------------------------
-- Render Single World to Quads
-----------------------------------------------------------

renderWorldQuads :: EngineEnv -> WorldState -> EngineM ε σ (V.Vector SortableQuad)
renderWorldQuads env worldState = do
    tileData <- liftIO $ readIORef (wsTilesRef worldState)
    camera <- liftIO $ readIORef (wsCameraRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    logger <- liftIO $ readIORef (loggerRef env)
    
    let tiles = HM.toList (wtdTiles tileData)
    
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    
    quads <- forM tiles $ \((x, y, z), tile) ->
        tileToQuad env camera textures fbW fbH x y z tile
    
    return $ V.fromList quads

-----------------------------------------------------------
-- Convert Tile to Render Batch
-----------------------------------------------------------

tileToQuad :: EngineEnv -> WorldCamera -> WorldTextures
  -> Int -> Int -> Int -> Int -> Int -> Tile 
           -> EngineM ε σ SortableQuad
tileToQuad env camera textures _fbW _fbH worldX worldY worldZ tile = do
    logger <- liftIO $ readIORef (loggerRef env)
    
    refPool ← liftIO $ readIORef (assetPoolRef env)
    
    let (rawX, rawY) = gridToScreen worldX worldY
        
        -- Apply world camera offset
        drawX = rawX - wcX camera
        drawY = rawY - wcY camera
        
        -- Apply height offset (elevated tiles shift up)
        heightOffset = fromIntegral worldZ * tileSideHeight
        finalY = drawY - heightOffset

        -- Sort key: higher (gx + gy) = closer to viewer = drawn later
        sortKey = fromIntegral (worldX + worldY) 
                + fromIntegral worldZ * 0.001

    let texHandle = getTileTexture textures (tileType tile)
    
    pool <- liftIO $ readIORef (assetPoolRef env)
    mbAssetState <- liftIO $ lookupTextureAsset texHandle pool
    
    actualSlot' <- case mbAssetState of
        Just (AssetReady atlasId []) -> return atlasId
        _ -> do
            liftIO $ logWarn logger CatSystem $ 
                "Texture not ready or not found! Handle: " <> T.pack (show texHandle)
            return $ AssetId 0
        
    let AssetId actualSlot = actualSlot'
        vertices = V.fromList
            [ Vertex (Vec2 drawX finalY)                          (Vec2 0 0) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 (drawX + tileWidth) finalY)            (Vec2 1 0) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 (drawX + tileWidth) (finalY + tileHeight)) (Vec2 1 1) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 drawX finalY)                          (Vec2 0 0) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 (drawX + tileWidth) (finalY + tileHeight)) (Vec2 1 1) (Vec4 1 1 1 1) (fromIntegral actualSlot)
            , Vertex (Vec2 drawX (finalY + tileHeight))           (Vec2 0 1) (Vec4 1 1 1 1) (fromIntegral actualSlot)
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

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
import Engine.Asset.Types (AssetPool(..), TextureAtlas(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Monad (EngineM)
import Engine.Core.Log (logDebug, LogCategory(..), logInfo, logWarn)
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Types (RenderBatch(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import qualified Data.Vector as V

-----------------------------------------------------------
-- Update World Tiles (called before scene rendering)
-----------------------------------------------------------

-- | Generate render batches for all visible world tiles
-- Returns batches to be added to the scene
updateWorldTiles :: EngineM ε σ (V.Vector RenderBatch)
updateWorldTiles = do
    env <- ask
    logger <- liftIO $ readIORef (loggerRef env)
    
    -- Always log that we're being called
    liftIO $ logDebug logger CatSystem "updateWorldTiles called"
    
    worldManager <- liftIO $ readIORef (worldManagerRef env)
    
    -- Log visible worlds
    liftIO $ logDebug logger CatSystem $ 
        "Visible worlds: " <> T.pack (show $ length $ wmVisible worldManager) 
        <> " | Total worlds: " <> T.pack (show $ length $ wmWorlds worldManager)
    
    -- Log each visible world ID
    forM_ (wmVisible worldManager) $ \pageId -> do
        liftIO $ logDebug logger CatSystem $ "Visible world ID: " <> unWorldPageId pageId
    
    -- Collect batches from all visible worlds
    batches <- forM (wmVisible worldManager) $ \pageId -> do
        liftIO $ logDebug logger CatSystem $ 
            "Processing world: " <> unWorldPageId pageId
        case lookup pageId (wmWorlds worldManager) of
            Just worldState -> do
                liftIO $ logDebug logger CatSystem $ 
                    "Found world state for: " <> unWorldPageId pageId
                renderWorldBatches env worldState
            Nothing -> do
                liftIO $ logDebug logger CatSystem $ 
                    "No world state found for: " <> unWorldPageId pageId
                return V.empty
    
    let allBatches = V.concat batches
    liftIO $ logDebug logger CatSystem $ 
        "Generated " <> T.pack (show $ V.length allBatches) <> " world tile batches"
    
    return allBatches

unWorldPageId :: WorldPageId -> Text
unWorldPageId (WorldPageId t) = t

-----------------------------------------------------------
-- Render Single World to Batches
-----------------------------------------------------------

renderWorldBatches :: EngineEnv -> WorldState -> EngineM ε σ (V.Vector RenderBatch)
renderWorldBatches env worldState = do
    -- Read tile data
    tileData <- liftIO $ readIORef (wsTilesRef worldState)
    camera <- liftIO $ readIORef (wsCameraRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    logger <- liftIO $ readIORef (loggerRef env)
    
    let tiles = HM.toList (wtdTiles tileData)
    liftIO $ logDebug logger CatSystem $ 
        "World has " <> T.pack (show $ length tiles) <> " tiles, grass texture: " 
        <> T.pack (show $ wtGrassTexture textures)
    
    -- Get framebuffer size for projection
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    
    -- Convert each tile to a render batch
    batches <- forM tiles $ \((x, y), tile) -> do
        liftIO $ logDebug logger CatSystem $ 
            "Rendering tile at (" <> T.pack (show x) <> "," <> T.pack (show y) 
            <> ") type: " <> T.pack (show $ tileType tile)
        tileToRenderBatch env camera textures fbW fbH x y tile
    
    return $ V.fromList batches

-----------------------------------------------------------
-- Convert Tile to Render Batch
-----------------------------------------------------------

tileToRenderBatch :: EngineEnv -> WorldCamera -> WorldTextures -> Int -> Int -> Int -> Int -> Tile 
                  -> EngineM ε σ RenderBatch
tileToRenderBatch env camera textures fbW fbH worldX worldY tile = do
    logger <- liftIO $ readIORef (loggerRef env)
    
    -- Get world camera for proper coordinate space
    worldCam <- liftIO $ readIORef (cameraRef env)

    refPool ← liftIO $ readIORef (assetPoolRef env)
    liftIO $ logDebug logger CatSystem $ 
        "Ref pool has " <> T.pack (show $ Map.size $ apTextureAtlases refPool) <> " atlases before tile batch creation"
    
    -- Tile size in world units (not pixels!)
    let tileSizeW = 0.1
        tileSizeH = 0.05
        
        -- Isometric projection in world space
        isoX = (fromIntegral worldX - fromIntegral worldY) * (tileSizeW / 2)
        isoY = (fromIntegral worldX + fromIntegral worldY) * (tileSizeH / 2)
        
        -- Apply world camera offset
        worldX' = isoX - wcX camera
        worldY' = isoY - wcY camera
        
        -- Apply height offset
        heightOffset = (fromIntegral (tileHeight tile) :: Float) * 0.01
        finalY = worldY' - heightOffset
    
    let texHandle = getTileTexture textures (tileType tile)
    
    -- Look up the bindless slot for this texture handle
    pool <- liftIO $ readIORef (assetPoolRef env)
    liftIO $ logDebug logger CatSystem $ 
        "Pool has " <> T.pack (show $ Map.size $ apTextureAtlases pool) <> " atlases"
    mbAssetState <- liftIO $ lookupTextureAsset texHandle pool
    
    liftIO $ logDebug logger CatSystem $ 
        "Looking up texture handle " <> T.pack (show texHandle) <> 
        " got state: " <> T.pack (show (isJust mbAssetState))
    
    actualSlot <- case mbAssetState of
        Just (AssetReady assetId _) -> do
            liftIO $ logDebug logger CatSystem $ 
                "Asset ready, AssetId: " <> T.pack (show assetId)
            
            -- Look up the atlas
            let mbAtlas = Map.lookup assetId (apTextureAtlases pool)
            case mbAtlas of
                Just atlas -> do
                    case taBindlessSlot atlas of
                        Just slot -> do
                            liftIO $ logDebug logger CatSystem $ 
                                "Found bindless slot: " <> T.pack (show slot)
                            return slot
                        Nothing -> do
                            liftIO $ logWarn logger CatSystem $ 
                                "Texture has no bindless slot!"
                            return 0
                Nothing -> do
                    liftIO $ logWarn logger CatSystem $ 
                        "Atlas not found for AssetId: " <> T.pack (show assetId)
                    return 0
        _ -> do
            liftIO $ logWarn logger CatSystem $ 
                "Texture not ready or not found! Handle: " <> T.pack (show texHandle)
            return 0
        
    let -- Create quad vertices with SLOT not HANDLE
        vertices = V.fromList
            [ Vertex (Vec2 worldX' finalY) (Vec2 0.0 0.0) (Vec4 1.0 1.0 1.0 1.0) (fromIntegral actualSlot)
            , Vertex (Vec2 (worldX' + tileSizeW) finalY) (Vec2 1.0 0.0) (Vec4 1.0 1.0 1.0 1.0) (fromIntegral actualSlot)
            , Vertex (Vec2 (worldX' + tileSizeW) (finalY + tileSizeH)) (Vec2 1.0 1.0) (Vec4 1.0 1.0 1.0 1.0) (fromIntegral actualSlot)
            , Vertex (Vec2 worldX' finalY) (Vec2 0.0 0.0) (Vec4 1.0 1.0 1.0 1.0) (fromIntegral actualSlot)
            , Vertex (Vec2 (worldX' + tileSizeW) (finalY + tileSizeH)) (Vec2 1.0 1.0) (Vec4 1.0 1.0 1.0 1.0) (fromIntegral actualSlot)
            , Vertex (Vec2 worldX' (finalY + tileSizeH)) (Vec2 0.0 1.0) (Vec4 1.0 1.0 0.0 1.0) (fromIntegral actualSlot)
            ]
    
    liftIO $ logDebug logger CatSystem $ 
        "Created tile batch with bindless slot: " <> T.pack (show actualSlot)
    
    return $ RenderBatch
        { rbVertices = vertices
        , rbTexture = texHandle
        , rbLayer = LayerId 0
        , rbObjects = V.empty
        , rbDirty = False
        }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

getTileTexture :: WorldTextures -> Word8 -> TextureHandle
getTileTexture textures 1 = wtGrassTexture textures  -- grass
getTileTexture _        _ = TextureHandle 0

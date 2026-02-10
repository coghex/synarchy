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
-- Tile Visibility
-----------------------------------------------------------

-- | Camera view bounds in world-space.
-- The projection maps [-zoom*aspect, +zoom*aspect] × [-zoom, +zoom]
-- centered on camPosition. We add one tile of padding so tiles
-- that are partially on-screen still get drawn.
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
        -- Pad by one full tile in each direction so partially-visible
        -- tiles at the edges are not popped in/out
        padX     = tileWidth
        padY     = tileHeight
    in ViewBounds
        { vbLeft   = cx - halfW - padX
        , vbRight  = cx + halfW + padX
        , vbTop    = cy - halfH - padY
        , vbBottom = cy + halfH + padY
        }

-- | Check whether a tile's quad overlaps the camera view bounds.
-- drawX/drawY is the top-left of the sprite quad in world-space.
isTileVisible :: ViewBounds -> Float -> Float -> Bool
isTileVisible vb drawX drawY =
    let tileRight  = drawX + tileWidth
        tileBottom = drawY + tileHeight
    in not (tileRight  < vbLeft vb
         || drawX      > vbRight vb
         || tileBottom < vbTop vb
         || drawY      > vbBottom vb)

-----------------------------------------------------------
-- Render Single World to Quads
-----------------------------------------------------------

renderWorldQuads :: EngineEnv -> WorldState -> EngineM ε σ (V.Vector SortableQuad)
renderWorldQuads env worldState = do
    tileData <- liftIO $ readIORef (wsTilesRef worldState)
    textures <- liftIO $ readIORef (wsTexturesRef worldState)
    logger <- liftIO $ readIORef (loggerRef env)
    camera <- liftIO $ readIORef (cameraRef env)
    
    let tiles = HM.toList (wtdTiles tileData)
    
    (fbW, fbH) <- liftIO $ readIORef (framebufferSizeRef env)
    
    let vb = computeViewBounds camera fbW fbH
    
    -- Filter tiles by visibility before building quads
    let visibleTiles = filter (isTileInView vb) tiles
    
    liftIO $ logDebug logger CatSystem $
        "Tile culling: " <> T.pack (show $ length tiles) <> " total, "
        <> T.pack (show $ length visibleTiles) <> " visible"
    
    quads <- forM visibleTiles $ \((x, y, z), tile) ->
        tileToQuad env textures x y z tile
    
    return $ V.fromList quads
  where
    -- | Check a tile by its grid coords against the view bounds.
    -- Computes the draw position (same math as tileToQuad) and tests.
    isTileInView :: ViewBounds -> ((Int, Int, Int), Tile) -> Bool
    isTileInView vb ((gx, gy, gz), _) =
        let (rawX, rawY) = gridToScreen gx gy
            heightOffset = fromIntegral gz * tileSideHeight
            drawX = rawX
            drawY = rawY - heightOffset
        in isTileVisible vb drawX drawY

-----------------------------------------------------------
-- Convert Tile to Render Batch
-----------------------------------------------------------

tileToQuad :: EngineEnv -> WorldTextures
  -> Int -> Int -> Int -> Tile 
           -> EngineM ε σ SortableQuad
tileToQuad env textures worldX worldY worldZ tile = do
    logger <- liftIO $ readIORef (loggerRef env)
    
    let (rawX, rawY) = gridToScreen worldX worldY
        
        -- Apply height offset (elevated tiles shift up)
        heightOffset = fromIntegral worldZ * tileSideHeight
        drawX = rawX
        drawY = rawY - heightOffset

        -- Sort key: higher (gx + gy) = closer to viewer = drawn later
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

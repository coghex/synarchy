{-# LANGUAGE Strict #-}
module UI.Render
  ( renderUIPages
  , uiLayerToLayerId
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.List (sortOn)
import Data.IORef (readIORef)
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))
import Engine.Core.Monad
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Error.Exception (logInfo)
import Engine.Graphics.Font.Data (FontCache(..), fcFonts)
import Engine.Graphics.Font.Draw (layoutTextUI)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..), fromBindlessHandle)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types.Batch (RenderBatch(..), RenderItem(..), TextRenderBatch(..))
import UI.Types
import UI.Manager (getVisiblePages, getElementAbsolutePosition)

-- | Base layer offset for UI (world uses 0-9)
uiLayerBase :: Int
uiLayerBase = 10

-- | Convert UI layer to render LayerId
uiLayerToLayerId :: UILayer -> Int -> LayerId
uiLayerToLayerId layer zIndex = LayerId $ fromIntegral $ case layer of
    LayerHUD     -> uiLayerBase + 0
    LayerMenu    -> uiLayerBase + 1
    LayerModal   -> uiLayerBase + 10 + zIndex
    LayerTooltip -> uiLayerBase + 100 + zIndex
    LayerDebug   -> uiLayerBase + 200 + zIndex

-- | Look up the bindless slot index for a texture handle
lookupTextureSlot :: BindlessTextureSystem -> TextureHandle -> Float
lookupTextureSlot bindless texHandle =
    case Map.lookup texHandle (btsHandleMap bindless) of
        Just bth -> fromIntegral $ fromBindlessHandle bth
        Nothing  -> 0.0

-- | Render all visible UI pages
renderUIPages :: EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderUIPages = do
    env <- ask
    mgr <- liftIO $ readIORef (uiManagerRef env)
    
    gs <- gets graphicsState
    let maybeBindless = textureSystem gs
    
    case maybeBindless of
        Nothing -> do
            logInfo "No bindless texture system available for UI rendering"
            pure (V.empty, Map.empty)
        Just bindless -> do
            -- Get font cache for text rendering
            fontCache <- liftIO $ readIORef (fontCacheRef env)
            
            let visiblePages = getVisiblePages mgr
                defaultTex = upmDefaultBoxTex mgr
            
            results <- forM visiblePages $ \page -> 
                renderPage mgr bindless fontCache defaultTex page
            
            let allBatches = V.concat $ map fst results
                allLayered = foldr (Map.unionWith (<>)) Map.empty (map snd results)
            
            pure (allBatches, allLayered)

-- | Render a single page
renderPage :: UIPageManager -> BindlessTextureSystem -> FontCache 
           -> Maybe TextureHandle -> UIPage 
           -> EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderPage mgr bindless fontCache defaultTex page = do
    let layerId = uiLayerToLayerId (upLayer page) (upZIndex page)
        rootElems = upRootElements page
    
    results <- forM rootElems $ \elemHandle ->
        renderElement mgr bindless fontCache defaultTex layerId elemHandle
    
    let allBatches = V.concat $ map fst results
        allLayered = foldr (Map.unionWith (<>)) Map.empty (map snd results)
    
    pure (allBatches, allLayered)

-- | Render an element and all its children
renderElement :: UIPageManager -> BindlessTextureSystem -> FontCache
              -> Maybe TextureHandle -> LayerId -> ElementHandle 
              -> EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderElement mgr bindless fontCache defaultTex layerId handle = do
    case Map.lookup handle (upmElements mgr) of
        Nothing -> pure (V.empty, Map.empty)
        Just elem
            | not (ueVisible elem) -> pure (V.empty, Map.empty)
            | otherwise -> do
                let (absX, absY) = case getElementAbsolutePosition handle mgr of
                        Just pos -> pos
                        Nothing  -> (0, 0)
                
                (selfBatch, selfItem) <- renderElementData bindless fontCache 
                                           defaultTex layerId elem absX absY
                
                let sortedChildren = sortOn (getChildZIndex mgr) (ueChildren elem)
                childResults <- forM sortedChildren $ \childHandle ->
                    renderElement mgr bindless fontCache defaultTex layerId childHandle
                
                let childBatches = V.concat $ map fst childResults
                    childLayered = foldr (Map.unionWith (<>)) Map.empty (map snd childResults)
                
                let allBatches = case selfBatch of
                        Nothing -> childBatches
                        Just batch -> V.singleton batch <> childBatches
                    
                    allLayered = case selfItem of
                        Nothing -> childLayered
                        Just item -> Map.unionWith (<>) 
                                       (Map.singleton layerId (V.singleton item)) 
                                       childLayered
                
                pure (allBatches, allLayered)

-- | Get z-index for sorting
getChildZIndex :: UIPageManager -> ElementHandle -> Int
getChildZIndex mgr handle = 
    case Map.lookup handle (upmElements mgr) of
        Nothing -> 0
        Just elem -> ueZIndex elem

-- | Render element's visual data
renderElementData :: BindlessTextureSystem -> FontCache -> Maybe TextureHandle 
                  -> LayerId -> UIElement -> Float -> Float 
                  -> EngineM ε σ (Maybe RenderBatch, Maybe RenderItem)
renderElementData bindless fontCache defaultTex layerId elem absX absY = 
    case ueRenderData elem of
        RenderNone -> pure (Nothing, Nothing)
        
        RenderBox style -> 
            case defaultTex of
                Nothing -> do
                    logInfo "UI box has no default texture set"
                    pure (Nothing, Nothing)
                Just tex -> do
                    let (w, h) = ueSize elem
                        color = ubsColor style
                        atlasId = lookupTextureSlot bindless tex
                        vertices = makeQuadVertices absX absY w h color atlasId
                        batch = RenderBatch
                            { rbTexture  = tex
                            , rbLayer    = layerId
                            , rbVertices = vertices
                            , rbObjects  = V.empty
                            , rbDirty    = True
                            }
                    pure (Just batch, Just (SpriteItem batch))
        
        RenderText style -> do
            let fontHandle = utsFont style
            case Map.lookup fontHandle (fcFonts fontCache) of
                Nothing -> do
                    logInfo $ "UI text font not found: " <> show fontHandle
                    pure (Nothing, Nothing)
                Just atlas -> do
                    let text = utsText style
                        (cr, cg, cb, ca) = utsColor style
                        color = (cr, cg, cb, ca)
                        instances = layoutTextUI atlas absX absY text color
                    
                    if V.null instances
                        then pure (Nothing, Nothing)
                        else do
                            let textBatch = TextRenderBatch
                                    { trbFont      = fontHandle
                                    , trbLayer     = layerId
                                    , trbInstances = instances
                                    , trbObjects   = V.empty
                                    }
                            -- Text doesn't go in sprite batches, only in layered items
                            pure (Nothing, Just (TextItem textBatch))
        
        RenderSprite style -> do
            let (w, h) = ueSize elem
                color = ussColor style
                tex = ussTexture style
                atlasId = lookupTextureSlot bindless tex
                vertices = makeQuadVertices absX absY w h color atlasId
                batch = RenderBatch
                    { rbTexture  = tex
                    , rbLayer    = layerId
                    , rbVertices = vertices
                    , rbObjects  = V.empty
                    , rbDirty    = True
                    }
            pure (Just batch, Just (SpriteItem batch))

-- | Generate quad vertices for a UI element
makeQuadVertices :: Float -> Float -> Float -> Float 
                 -> (Float, Float, Float, Float) 
                 -> Float
                 -> V.Vector Vertex
makeQuadVertices x y w h (cr, cg, cb, ca) atlasId =
    let x0 = x
        y0 = y
        x1 = x + w
        y1 = y + h
        
        u0 = 0.0
        v0 = 0.0
        u1 = 1.0
        v1 = 1.0
        
        col = Vec4 cr cg cb ca
        
        v1' = Vertex (Vec2 x0 y0) (Vec2 u0 v0) col atlasId
        v2' = Vertex (Vec2 x1 y0) (Vec2 u1 v0) col atlasId
        v3' = Vertex (Vec2 x0 y1) (Vec2 u0 v1) col atlasId
        v4' = Vertex (Vec2 x1 y0) (Vec2 u1 v0) col atlasId
        v5' = Vertex (Vec2 x1 y1) (Vec2 u1 v1) col atlasId
        v6' = Vertex (Vec2 x0 y1) (Vec2 u0 v1) col atlasId
    in V.fromList [v1', v2', v3', v4', v5', v6']

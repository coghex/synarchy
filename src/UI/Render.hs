{-# LANGUAGE Strict #-}
module UI.Render
  ( renderUIPages
  , uiLayerToLayerId
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.List (sortOn)
import Data.IORef (readIORef)
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))
import Engine.Core.Monad
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logInfoM, logWarnM)
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Graphics.Font.Data (FontCache(..), fcFonts)
import Engine.Graphics.Font.Draw (layoutTextUI)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..), fromBindlessHandle)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types.Batch (RenderBatch(..), RenderItem(..), TextRenderBatch(..))
import UI.Types
import UI.Manager (getVisiblePages, getElementAbsolutePosition, getBoxTextureSet)

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
            logWarnM CatUI "No bindless texture system available for UI rendering"
            pure (V.empty, Map.empty)
        Just bindless -> do
            fontCache <- liftIO $ readIORef (fontCacheRef env)
            
            let visiblePages = getVisiblePages mgr
            
            results <- forM visiblePages $ \page -> 
                renderPage mgr bindless fontCache page
            
            let allBatches = V.concat $ map fst results
                allLayered = foldr (Map.unionWith (<>)) Map.empty (map snd results)
            
            pure (allBatches, allLayered)

-- | Render a single page
renderPage :: UIPageManager -> BindlessTextureSystem -> FontCache -> UIPage 
           -> EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderPage mgr bindless fontCache page = do
    let layerId = uiLayerToLayerId (upLayer page) (upZIndex page)
        rootElems = upRootElements page
    
    results <- forM rootElems $ \elemHandle ->
        renderElement mgr bindless fontCache layerId elemHandle
    
    let allBatches = V.concat $ map fst results
        allLayered = foldr (Map.unionWith (<>)) Map.empty (map snd results)
    
    pure (allBatches, allLayered)

-- | Render an element and all its children
renderElement :: UIPageManager -> BindlessTextureSystem -> FontCache
              -> LayerId -> ElementHandle 
              -> EngineM ε σ (V.Vector RenderBatch, Map.Map LayerId (V.Vector RenderItem))
renderElement mgr bindless fontCache layerId handle = do
    case Map.lookup handle (upmElements mgr) of
        Nothing -> pure (V.empty, Map.empty)
        Just elem
            | not (ueVisible elem) -> pure (V.empty, Map.empty)
            | otherwise -> do
                let (absX, absY) = case getElementAbsolutePosition handle mgr of
                        Just pos -> pos
                        Nothing  -> (0, 0)
                
                (selfBatches, selfItems) <- renderElementData mgr bindless fontCache 
                                              layerId elem absX absY
                
                let sortedChildren = sortOn (getChildZIndex mgr) (ueChildren elem)
                childResults <- forM sortedChildren $ \childHandle ->
                    renderElement mgr bindless fontCache layerId childHandle
                
                let childBatches = V.concat $ map fst childResults
                    childLayered = foldr (Map.unionWith (<>)) Map.empty (map snd childResults)
                
                let allBatches = selfBatches <> childBatches
                    allLayered = if V.null selfItems
                                 then childLayered
                                 else Map.unionWith (<>) 
                                        (Map.singleton layerId selfItems) 
                                        childLayered
                
                pure (allBatches, allLayered)

-- | Get z-index for sorting
getChildZIndex :: UIPageManager -> ElementHandle -> Int
getChildZIndex mgr handle = 
    case Map.lookup handle (upmElements mgr) of
        Nothing -> 0
        Just elem -> ueZIndex elem

-- | Render element's visual data
renderElementData :: UIPageManager -> BindlessTextureSystem -> FontCache 
                  -> LayerId -> UIElement -> Float -> Float 
                  -> EngineM ε σ (V.Vector RenderBatch, V.Vector RenderItem)
renderElementData mgr bindless fontCache layerId elem absX absY = 
    case ueRenderData elem of
        RenderNone -> pure (V.empty, V.empty)
        
        RenderBox style -> do
            case getBoxTextureSet (ubsTextures style) mgr of
                Nothing -> do
                    logWarnM CatUI "UI box texture set not found"
                    pure (V.empty, V.empty)
                Just texSet -> do
                    let (w, h) = ueSize elem
                        tileSize = ubsTileSize style
                        color = ubsColor style
                        batches = makeBoxBatches bindless texSet absX absY w h tileSize color layerId
                        items = V.map SpriteItem batches
                    pure (batches, items)
        
        RenderText style -> do
            let fontHandle = utsFont style
            case Map.lookup fontHandle (fcFonts fontCache) of
                Nothing -> do
                    logWarnM CatUI $ "UI text font not found: " <> (T.pack (show fontHandle))
                    pure (V.empty, V.empty)
                Just atlas -> do
                    let text = utsText style
                        (cr, cg, cb, ca) = utsColor style
                        color = (cr, cg, cb, ca)
                        instances = layoutTextUI atlas absX absY text color
                    
                    if V.null instances
                        then pure (V.empty, V.empty)
                        else do
                            let textBatch = TextRenderBatch
                                    { trbFont      = fontHandle
                                    , trbLayer     = layerId
                                    , trbInstances = instances
                                    , trbObjects   = V.empty
                                    }
                            pure (V.empty, V.singleton (TextItem textBatch))
        
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
            pure (V.singleton batch, V.singleton (SpriteItem batch))

-- | Generate 9 render batches for a box
makeBoxBatches :: BindlessTextureSystem -> BoxTextureSet 
               -> Float -> Float -> Float -> Float -> Float 
               -> (Float, Float, Float, Float) -> LayerId
               -> V.Vector RenderBatch
makeBoxBatches bindless texSet x y w h tileSize color layerId =
    let ts = tileSize
        midW = max 0 (w - ts * 2)
        midH = max 0 (h - ts * 2)
        
        -- Positions for each tile
        -- Top row
        nwX = x
        nwY = y
        nX  = x + ts
        nY  = y
        neX = x + ts + midW
        neY = y
        
        -- Middle row
        wX  = x
        wY  = y + ts
        cX  = x + ts
        cY  = y + ts
        eX  = x + ts + midW
        eY  = y + ts
        
        -- Bottom row
        swX = x
        swY = y + ts + midH
        sX  = x + ts
        sY  = y + ts + midH
        seX = x + ts + midW
        seY = y + ts + midH
        
        -- Create batch for each tile
        makeBatch tex px py pw ph = 
            let atlasId = lookupTextureSlot bindless tex
                vertices = makeQuadVertices px py pw ph color atlasId
            in RenderBatch
                { rbTexture  = tex
                , rbLayer    = layerId
                , rbVertices = vertices
                , rbObjects  = V.empty
                , rbDirty    = True
                }
        
    in V.fromList
        [ makeBatch (btsNW texSet) nwX nwY ts ts
        , makeBatch (btsN texSet)  nX  nY  midW ts
        , makeBatch (btsNE texSet) neX neY ts ts
        , makeBatch (btsW texSet)  wX  wY  ts midH
        , makeBatch (btsCenter texSet) cX cY midW midH
        , makeBatch (btsE texSet)  eX  eY  ts midH
        , makeBatch (btsSW texSet) swX swY ts ts
        , makeBatch (btsS texSet)  sX  sY  midW ts
        , makeBatch (btsSE texSet) seX seY ts ts
        ]

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

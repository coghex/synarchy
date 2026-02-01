{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.UI
  ( -- Page functions
    uiNewPageFn
  , uiDeletePageFn
  , uiShowPageFn
  , uiHidePageFn
    -- Element creation
  , uiNewElementFn
  , uiNewBoxFn
  , uiNewTextFn
  , uiNewSpriteFn
    -- Hierarchy
  , uiAddToPageFn
  , uiAddChildFn
  , uiRemoveElementFn
  , uiDeleteElementFn
    -- Properties
  , uiSetPositionFn
  , uiSetSizeFn
  , uiSetVisibleFn
  , uiSetClickableFn
  , uiSetZIndexFn
  , uiSetColorFn
  , uiSetTextFn
  , uiLoadBoxTexturesFn
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))
import UI.Types
import UI.Manager

-----------------------------------------------------------
-- Page Functions
-----------------------------------------------------------

-- | UI.newPage(name, layer) -> pageHandle
uiNewPageFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiNewPageFn env = do
    nameArg <- Lua.tostring 1
    layerArg <- Lua.tostring 2
    
    case (nameArg, layerArg) of
        (Just nameBS, Just layerBS) -> do
            let name = TE.decodeUtf8 nameBS
                layer = parseLayer (TE.decodeUtf8 layerBS)
            
            handle <- Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                let (h, newMgr) = createPage name layer mgr
                in (newMgr, h)
            
            Lua.pushinteger (fromIntegral $ unPageHandle handle)
        _ -> Lua.pushnil
    
    return 1

-- | UI.deletePage(pageHandle)
uiDeletePageFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiDeletePageFn env = do
    handleArg <- Lua.tointeger 1
    case handleArg of
        Just n -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (deletePage (PageHandle $ fromIntegral n) mgr, ())
        Nothing -> pure ()
    return 0

-- | UI.showPage(pageHandle)
uiShowPageFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiShowPageFn env = do
    handleArg <- Lua.tointeger 1
    case handleArg of
        Just n -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (showPage (PageHandle $ fromIntegral n) mgr, ())
        Nothing -> pure ()
    return 0

-- | UI.hidePage(pageHandle)
uiHidePageFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiHidePageFn env = do
    handleArg <- Lua.tointeger 1
    case handleArg of
        Just n -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (hidePage (PageHandle $ fromIntegral n) mgr, ())
        Nothing -> pure ()
    return 0

-----------------------------------------------------------
-- Element Creation
-----------------------------------------------------------

-- | UI.newElement(name, width, height, pageHandle) -> elementHandle
uiNewElementFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiNewElementFn env = do
    nameArg <- Lua.tostring 1
    widthArg <- Lua.tonumber 2
    heightArg <- Lua.tonumber 3
    pageArg <- Lua.tointeger 4
    
    case (nameArg, widthArg, heightArg, pageArg) of
        (Just nameBS, Just w, Just h, Just p) -> do
            let name = TE.decodeUtf8 nameBS
                pageHandle = PageHandle (fromIntegral p)
            
            handle <- Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                let (elemH, newMgr) = createElement name (realToFrac w) (realToFrac h) pageHandle mgr
                in (newMgr, elemH)
            
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ -> Lua.pushnil
    
    return 1

-- | UI.newBox(name, width, height, boxTexHandle, tileSize, r, g, b, a, pageHandle) -> elementHandle
uiNewBoxFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiNewBoxFn env = do
    nameArg <- Lua.tostring 1
    widthArg <- Lua.tonumber 2
    heightArg <- Lua.tonumber 3
    boxTexArg <- Lua.tointeger 4
    tileSizeArg <- Lua.tonumber 5
    rArg <- Lua.tonumber 6
    gArg <- Lua.tonumber 7
    bArg <- Lua.tonumber 8
    aArg <- Lua.tonumber 9
    pageArg <- Lua.tointeger 10
    
    case (nameArg, widthArg, heightArg, boxTexArg, tileSizeArg, rArg, gArg, bArg, aArg, pageArg) of
        (Just nameBS, Just w, Just h, Just bt, Just ts, Just r, Just g, Just b, Just a, Just p) -> do
            let name = TE.decodeUtf8 nameBS
                boxTexHandle = BoxTextureHandle (fromIntegral bt)
                tileSize = realToFrac ts
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)
            
            handle <- Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                let (elemH, newMgr) = createBox name (realToFrac w) (realToFrac h) 
                                        boxTexHandle tileSize color pageHandle mgr
                in (newMgr, elemH)
            
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ -> Lua.pushnil
    
    return 1

-- | UI.loadBoxTextures(texCenter, texN, texS, texE, texW, texNE, texNW, texSE, texSW) -> boxTextureHandle
uiLoadBoxTexturesFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiLoadBoxTexturesFn env = do
    centerArg <- Lua.tointeger 1
    nArg <- Lua.tointeger 2
    sArg <- Lua.tointeger 3
    eArg <- Lua.tointeger 4
    wArg <- Lua.tointeger 5
    neArg <- Lua.tointeger 6
    nwArg <- Lua.tointeger 7
    seArg <- Lua.tointeger 8
    swArg <- Lua.tointeger 9
    
    case (centerArg, nArg, sArg, eArg, wArg, neArg, nwArg, seArg, swArg) of
        (Just c, Just n, Just s, Just e, Just w, Just ne, Just nw, Just se, Just sw) -> do
            let texSet = BoxTextureSet
                    { btsCenter = TextureHandle (fromIntegral c)
                    , btsN      = TextureHandle (fromIntegral n)
                    , btsS      = TextureHandle (fromIntegral s)
                    , btsE      = TextureHandle (fromIntegral e)
                    , btsW      = TextureHandle (fromIntegral w)
                    , btsNE     = TextureHandle (fromIntegral ne)
                    , btsNW     = TextureHandle (fromIntegral nw)
                    , btsSE     = TextureHandle (fromIntegral se)
                    , btsSW     = TextureHandle (fromIntegral sw)
                    }
            
            handle <- Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                let (h, newMgr) = registerBoxTextures texSet mgr
                in (newMgr, h)
            
            Lua.pushinteger (fromIntegral $ unBoxTextureHandle handle)
        _ -> Lua.pushnil
    
    return 1

-- | UI.newText(name, text, fontHandle, r, g, b, a, pageHandle) -> elementHandle
uiNewTextFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiNewTextFn env = do
    nameArg <- Lua.tostring 1
    textArg <- Lua.tostring 2
    fontArg <- Lua.tointeger 3
    rArg <- Lua.tonumber 4
    gArg <- Lua.tonumber 5
    bArg <- Lua.tonumber 6
    aArg <- Lua.tonumber 7
    pageArg <- Lua.tointeger 8
    
    case (nameArg, textArg, fontArg, rArg, gArg, bArg, aArg, pageArg) of
        (Just nameBS, Just txtBS, Just f, Just r, Just g, Just b, Just a, Just p) -> do
            let name = TE.decodeUtf8 nameBS
                text = TE.decodeUtf8 txtBS
                fontHandle = FontHandle (fromIntegral f)
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)
            
            handle <- Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                let (elemH, newMgr) = createText name text fontHandle color pageHandle mgr
                in (newMgr, elemH)
            
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ -> Lua.pushnil
    
    return 1

-- | UI.newSprite(name, width, height, textureHandle, r, g, b, a, pageHandle) -> elementHandle
uiNewSpriteFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiNewSpriteFn env = do
    nameArg <- Lua.tostring 1
    widthArg <- Lua.tonumber 2
    heightArg <- Lua.tonumber 3
    texArg <- Lua.tointeger 4
    rArg <- Lua.tonumber 5
    gArg <- Lua.tonumber 6
    bArg <- Lua.tonumber 7
    aArg <- Lua.tonumber 8
    pageArg <- Lua.tointeger 9
    
    case (nameArg, widthArg, heightArg, texArg, rArg, gArg, bArg, aArg, pageArg) of
        (Just nameBS, Just w, Just h, Just t, Just r, Just g, Just b, Just a, Just p) -> do
            let name = TE.decodeUtf8 nameBS
                texHandle = TextureHandle (fromIntegral t)
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)
            
            handle <- Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                let (elemH, newMgr) = createSprite name (realToFrac w) (realToFrac h) texHandle color pageHandle mgr
                in (newMgr, elemH)
            
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ -> Lua.pushnil
    
    return 1

-----------------------------------------------------------
-- Hierarchy
-----------------------------------------------------------

-- | UI.addToPage(pageHandle, elementHandle, x, y)
uiAddToPageFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiAddToPageFn env = do
    pageArg <- Lua.tointeger 1
    elemArg <- Lua.tointeger 2
    xArg <- Lua.tonumber 3
    yArg <- Lua.tonumber 4
    
    case (pageArg, elemArg, xArg, yArg) of
        (Just p, Just e, Just x, Just y) -> do
            let pageHandle = PageHandle (fromIntegral p)
                elemHandle = ElementHandle (fromIntegral e)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                (addElementToPage pageHandle elemHandle (realToFrac x) (realToFrac y) mgr, ())
        _ -> pure ()
    
    return 0

-- | UI.addChild(parentHandle, childHandle, x, y)
uiAddChildFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiAddChildFn env = do
    parentArg <- Lua.tointeger 1
    childArg <- Lua.tointeger 2
    xArg <- Lua.tonumber 3
    yArg <- Lua.tonumber 4
    
    case (parentArg, childArg, xArg, yArg) of
        (Just p, Just c, Just x, Just y) -> do
            let parentHandle = ElementHandle (fromIntegral p)
                childHandle = ElementHandle (fromIntegral c)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                (addChildElement parentHandle childHandle (realToFrac x) (realToFrac y) mgr, ())
        _ -> pure ()
    
    return 0

-- | UI.removeElement(elementHandle)
uiRemoveElementFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiRemoveElementFn env = do
    elemArg <- Lua.tointeger 1
    case elemArg of
        Just e -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (removeElement (ElementHandle $ fromIntegral e) mgr, ())
        Nothing -> pure ()
    return 0

-- | UI.deleteElement(elementHandle)
uiDeleteElementFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiDeleteElementFn env = do
    elemArg <- Lua.tointeger 1
    case elemArg of
        Just e -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (deleteElement (ElementHandle $ fromIntegral e) mgr, ())
        Nothing -> pure ()
    return 0

-----------------------------------------------------------
-- Properties
-----------------------------------------------------------

-- | UI.setPosition(elementHandle, x, y)
uiSetPositionFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiSetPositionFn env = do
    elemArg <- Lua.tointeger 1
    xArg <- Lua.tonumber 2
    yArg <- Lua.tonumber 3
    
    case (elemArg, xArg, yArg) of
        (Just e, Just x, Just y) -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (setElementPosition (ElementHandle $ fromIntegral e) (realToFrac x) (realToFrac y) mgr, ())
        _ -> pure ()
    
    return 0

-- | UI.setSize(elementHandle, width, height)
uiSetSizeFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiSetSizeFn env = do
    elemArg <- Lua.tointeger 1
    wArg <- Lua.tonumber 2
    hArg <- Lua.tonumber 3
    
    case (elemArg, wArg, hArg) of
        (Just e, Just w, Just h) -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (setElementSize (ElementHandle $ fromIntegral e) (realToFrac w) (realToFrac h) mgr, ())
        _ -> pure ()
    
    return 0

-- | UI.setVisible(elementHandle, visible)
uiSetVisibleFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiSetVisibleFn env = do
    elemArg <- Lua.tointeger 1
    visArg <- Lua.toboolean 2
    
    case elemArg of
        Just e -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (setElementVisible (ElementHandle $ fromIntegral e) visArg mgr, ())
        Nothing -> pure ()
    
    return 0

-- | UI.setClickable(elementHandle, clickable)
uiSetClickableFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiSetClickableFn env = do
    elemArg <- Lua.tointeger 1
    clickArg <- Lua.toboolean 2
    
    case elemArg of
        Just e -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (setElementClickable (ElementHandle $ fromIntegral e) clickArg mgr, ())
        Nothing -> pure ()
    
    return 0

-- | UI.setZIndex(elementHandle, z)
uiSetZIndexFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiSetZIndexFn env = do
    elemArg <- Lua.tointeger 1
    zArg <- Lua.tointeger 2
    
    case (elemArg, zArg) of
        (Just e, Just z) -> Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
            (setElementZIndex (ElementHandle $ fromIntegral e) (fromIntegral z) mgr, ())
        _ -> pure ()
    
    return 0

-- | UI.setColor(elementHandle, r, g, b, a)
uiSetColorFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiSetColorFn env = do
    elemArg <- Lua.tointeger 1
    rArg <- Lua.tonumber 2
    gArg <- Lua.tonumber 3
    bArg <- Lua.tonumber 4
    aArg <- Lua.tonumber 5
    
    case (elemArg, rArg, gArg, bArg, aArg) of
        (Just e, Just r, Just g, Just b, Just a) -> do
            let elemHandle = ElementHandle (fromIntegral e)
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                case Map.lookup elemHandle (upmElements mgr) of
                    Nothing -> (mgr, ())
                    Just elem -> case ueRenderData elem of
                        RenderBox _    -> (setBoxColor elemHandle color mgr, ())
                        RenderSprite _ -> (setSpriteColor elemHandle color mgr, ())
                        RenderText _   -> (setTextColor elemHandle color mgr, ())
                        RenderNone     -> (mgr, ())
        _ -> pure ()
    
    return 0

-- | UI.setText(elementHandle, text)
uiSetTextFn :: EngineEnv -> Lua.LuaE Lua.Exception Lua.NumResults
uiSetTextFn env = do
    elemArg <- Lua.tointeger 1
    textArg <- Lua.tostring 2
    
    case (elemArg, textArg) of
        (Just e, Just txtBS) -> do
            let elemHandle = ElementHandle (fromIntegral e)
                text = TE.decodeUtf8 txtBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr ->
                (setText elemHandle text mgr, ())
        _ -> pure ()
    
    return 0

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

parseLayer :: Text -> UILayer
parseLayer t = case T.toLower t of
    "hud"     -> LayerHUD
    "menu"    -> LayerMenu
    "modal"   -> LayerModal
    "tooltip" -> LayerTooltip
    "debug"   -> LayerDebug
    _         -> LayerMenu

{-# LANGUAGE Strict, UnicodeSyntax #-}
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
  , uiFindElementAtFn
  , uiGetElementOnClickFn
  , uiFindHoverTargetFn
    -- Focus operations
  , uiSetFocusFn
  , uiClearFocusFn
  , uiGetFocusFn
  , uiHasFocusFn
    -- Text buffer operations
  , uiEnableTextInputFn
  , uiGetTextFn
  , uiSetTextInputFn
  , uiGetCursorFn
  , uiSetCursorFn
  , uiInsertCharFn
  , uiDeleteBackwardFn
  , uiDeleteForwardFn
  , uiCursorLeftFn
  , uiCursorRightFn
  , uiCursorHomeFn
  , uiCursorEndFn
    -- Properties
  , uiSetPositionFn
  , uiSetSizeFn
  , uiSetVisibleFn
  , uiSetClickableFn
  , uiSetOnClickFn
  , uiSetZIndexFn
  , uiSetColorFn
  , uiSetTextFn
  , uiSetSpriteTextureFn
  , uiSetBoxTexturesFn
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
import qualified UI.TextBuffer as TB

-----------------------------------------------------------
-- Page management
-----------------------------------------------------------

uiNewPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewPageFn env = do
    nameArg ← Lua.tostring 1
    layerArg ← Lua.tostring 2
    case (nameArg, layerArg) of
        (Just nameBS, Just layerBS) → do
            let name = TE.decodeUtf8 nameBS
                layer = parseLayer (TE.decodeUtf8 layerBS)
            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (h, newMgr) = createPage name layer mgr
                in (newMgr, h)
            Lua.pushinteger (fromIntegral $ unPageHandle handle)
        _ → Lua.pushnil
    return 1

uiDeletePageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeletePageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (deletePage (PageHandle $ fromIntegral n) mgr, ())
        Nothing → pure ()
    return 0

uiShowPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiShowPageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (showPage (PageHandle $ fromIntegral n) mgr, ())
        Nothing → pure ()
    return 0

uiHidePageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiHidePageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (hidePage (PageHandle $ fromIntegral n) mgr, ())
        Nothing → pure ()
    return 0

-----------------------------------------------------------
-- Element creation
-----------------------------------------------------------

uiNewElementFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewElementFn env = do
    nameArg ← Lua.tostring 1
    widthArg ← Lua.tonumber 2
    heightArg ← Lua.tonumber 3
    pageArg ← Lua.tointeger 4
    case (nameArg, widthArg, heightArg, pageArg) of
        (Just nameBS, Just w, Just h, Just p) → do
            let name = TE.decodeUtf8 nameBS
                pageHandle = PageHandle (fromIntegral p)
            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createElement name (realToFrac w) (realToFrac h) pageHandle mgr
                in (newMgr, elemH)
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil
    return 1

uiNewBoxFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewBoxFn env = do
    nameArg ← Lua.tostring 1
    widthArg ← Lua.tonumber 2
    heightArg ← Lua.tonumber 3
    boxTexArg ← Lua.tointeger 4
    tileSizeArg ← Lua.tonumber 5
    rArg ← Lua.tonumber 6
    gArg ← Lua.tonumber 7
    bArg ← Lua.tonumber 8
    aArg ← Lua.tonumber 9
    overflowArg ← Lua.tonumber 10
    pageArg ← Lua.tointeger 11
    case (nameArg, widthArg, heightArg, boxTexArg, tileSizeArg, rArg, gArg, bArg, aArg, overflowArg, pageArg) of
        (Just nameBS, Just w, Just h, Just bt, Just ts, Just r, Just g, Just b, Just a, Just ovf, Just p) → do
            let name = TE.decodeUtf8 nameBS
                boxTexHandle = BoxTextureHandle (fromIntegral bt)
                tileSize = realToFrac ts
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)
                overflow = realToFrac ovf
            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createBox name (realToFrac w) (realToFrac h) 
                                        boxTexHandle tileSize color overflow pageHandle mgr
                in (newMgr, elemH)
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil
    return 1

uiNewTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewTextFn env = do
    nameArg ← Lua.tostring 1
    textArg ← Lua.tostring 2
    fontArg ← Lua.tointeger 3
    sizeArg ← Lua.tonumber 4
    rArg ← Lua.tonumber 5
    gArg ← Lua.tonumber 6
    bArg ← Lua.tonumber 7
    aArg ← Lua.tonumber 8
    pageArg ← Lua.tointeger 9
    case (nameArg, textArg, fontArg, sizeArg, rArg, gArg, bArg, aArg, pageArg) of
        (Just nameBS, Just txtBS, Just f, Just s, Just r, Just g, Just b, Just a, Just p) → do
            let name = TE.decodeUtf8 nameBS
                text = TE.decodeUtf8 txtBS
                fontHandle = FontHandle (fromIntegral f)
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)
                size = realToFrac s
            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createText name text fontHandle size color pageHandle mgr
                in (newMgr, elemH)
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil
    return 1

uiNewSpriteFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewSpriteFn env = do
    nameArg ← Lua.tostring 1
    widthArg ← Lua.tonumber 2
    heightArg ← Lua.tonumber 3
    texArg ← Lua.tointeger 4
    rArg ← Lua.tonumber 5
    gArg ← Lua.tonumber 6
    bArg ← Lua.tonumber 7
    aArg ← Lua.tonumber 8
    pageArg ← Lua.tointeger 9
    case (nameArg, widthArg, heightArg, texArg, rArg, gArg, bArg, aArg, pageArg) of
        (Just nameBS, Just w, Just h, Just t, Just r, Just g, Just b, Just a, Just p) → do
            let name = TE.decodeUtf8 nameBS
                texHandle = TextureHandle (fromIntegral t)
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
                pageHandle = PageHandle (fromIntegral p)
            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (elemH, newMgr) = createSprite name (realToFrac w) (realToFrac h) texHandle color pageHandle mgr
                in (newMgr, elemH)
            Lua.pushinteger (fromIntegral $ unElementHandle handle)
        _ → Lua.pushnil
    return 1

-----------------------------------------------------------
-- Hierarchy management
-----------------------------------------------------------

uiAddToPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiAddToPageFn env = do
    pageArg ← Lua.tointeger 1
    elemArg ← Lua.tointeger 2
    xArg ← Lua.tonumber 3
    yArg ← Lua.tonumber 4
    case (pageArg, elemArg, xArg, yArg) of
        (Just p, Just e, Just x, Just y) → do
            let pageHandle = PageHandle (fromIntegral p)
                elemHandle = ElementHandle (fromIntegral e)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (addElementToPage pageHandle elemHandle (realToFrac x) (realToFrac y) mgr, ())
        _ → pure ()
    return 0

uiAddChildFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiAddChildFn env = do
    parentArg ← Lua.tointeger 1
    childArg ← Lua.tointeger 2
    xArg ← Lua.tonumber 3
    yArg ← Lua.tonumber 4
    case (parentArg, childArg, xArg, yArg) of
        (Just p, Just c, Just x, Just y) → do
            let parentHandle = ElementHandle (fromIntegral p)
                childHandle = ElementHandle (fromIntegral c)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (addChildElement parentHandle childHandle (realToFrac x) (realToFrac y) mgr, ())
        _ → pure ()
    return 0

uiRemoveElementFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiRemoveElementFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (removeElement (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

uiDeleteElementFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeleteElementFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (deleteElement (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

uiFindElementAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiFindElementAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just x, Just y) → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case findElementAt (realToFrac x, realToFrac y) mgr of
                Just (ElementHandle h) → Lua.pushinteger (fromIntegral h)
                Nothing → Lua.pushnil
        _ → Lua.pushnil
    return 1

uiGetElementOnClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetElementOnClickFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case Map.lookup (ElementHandle $ fromIntegral e) (upmElements mgr) of
                Just elem → case ueOnClick elem of
                    Just cb → Lua.pushstring (TE.encodeUtf8 cb)
                    Nothing → Lua.pushnil
                Nothing → Lua.pushnil
        Nothing → Lua.pushnil
    return 1

uiFindHoverTargetFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiFindHoverTargetFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just x, Just y) → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case findElementAt (realToFrac x, realToFrac y) mgr of
                Just hitElem → case findClickableAncestor hitElem mgr of
                    Just (ElementHandle h, cb) → do
                        Lua.pushinteger (fromIntegral h)
                        Lua.pushstring (TE.encodeUtf8 cb)
                    Nothing → do
                        Lua.pushnil
                        Lua.pushnil
                Nothing → do
                    Lua.pushnil
                    Lua.pushnil
        _ → do
            Lua.pushnil
            Lua.pushnil
    return 2

-----------------------------------------------------------
-- Focus operations
-----------------------------------------------------------

uiSetFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetFocusFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementFocus (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

uiClearFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiClearFocusFn env = do
    Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
        (clearElementFocus mgr, ())
    return 0

uiGetFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetFocusFn env = do
    mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
    case getElementFocus mgr of
        Just (ElementHandle h) → Lua.pushinteger (fromIntegral h)
        Nothing → Lua.pushnil
    return 1

uiHasFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiHasFocusFn env = do
    elemArg ← Lua.tointeger 1
    mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
    let isFocused = case (elemArg, getElementFocus mgr) of
            (Just e, Just (ElementHandle h)) → fromIntegral e ≡ h
            _ → False
    Lua.pushboolean isFocused
    return 1

-----------------------------------------------------------
-- Text buffer operations
-----------------------------------------------------------

uiEnableTextInputFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiEnableTextInputFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (enableTextInput (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

uiGetTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetTextFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case getTextBuffer (ElementHandle $ fromIntegral e) mgr of
                Just buf → Lua.pushstring (TE.encodeUtf8 $ tbContent buf)
                Nothing → Lua.pushnil
        Nothing → Lua.pushnil
    return 1

uiSetTextInputFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTextInputFn env = do
    elemArg ← Lua.tointeger 1
    textArg ← Lua.tostring 2
    case (elemArg, textArg) of
        (Just e, Just txtBS) → do
            let txt = TE.decodeUtf8 txtBS
                elemHandle = ElementHandle (fromIntegral e)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let newBuffer = TextBuffer { tbContent = txt, tbCursor = T.length txt }
                in (setTextBuffer elemHandle newBuffer mgr, ())
        _ → pure ()
    return 0

uiGetCursorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetCursorFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case getTextBuffer (ElementHandle $ fromIntegral e) mgr of
                Just buf → Lua.pushinteger (fromIntegral $ tbCursor buf)
                Nothing → Lua.pushnil
        Nothing → Lua.pushnil
    return 1

uiSetCursorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetCursorFn env = do
    elemArg ← Lua.tointeger 1
    posArg ← Lua.tointeger 2
    case (elemArg, posArg) of
        (Just e, Just pos) → do
            let elemHandle = ElementHandle (fromIntegral e)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (modifyTextBuffer elemHandle (\buf → 
                    buf { tbCursor = max 0 (min (T.length $ tbContent buf) (fromIntegral pos)) }
                ) mgr, ())
        _ → pure ()
    return 0

uiInsertCharFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiInsertCharFn env = do
    elemArg ← Lua.tointeger 1
    charArg ← Lua.tostring 2
    case (elemArg, charArg) of
        (Just e, Just charBS) → do
            let elemHandle = ElementHandle (fromIntegral e)
                charText = TE.decodeUtf8 charBS
            case T.uncons charText of
                Just (c, _) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                    (modifyTextBuffer elemHandle (TB.insertChar c) mgr, ())
                Nothing → pure ()
        _ → pure ()
    return 0

uiDeleteBackwardFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeleteBackwardFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.deleteBackward mgr, ())
        Nothing → pure ()
    return 0

uiDeleteForwardFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeleteForwardFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.deleteForward mgr, ())
        Nothing → pure ()
    return 0

uiCursorLeftFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorLeftFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorLeft mgr, ())
        Nothing → pure ()
    return 0

uiCursorRightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorRightFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorRight mgr, ())
        Nothing → pure ()
    return 0

uiCursorHomeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorHomeFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorHome mgr, ())
        Nothing → pure ()
    return 0

uiCursorEndFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiCursorEndFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (modifyTextBuffer (ElementHandle $ fromIntegral e) TB.cursorEnd mgr, ())
        Nothing → pure ()
    return 0

-----------------------------------------------------------
-- Property setters
-----------------------------------------------------------

uiSetPositionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetPositionFn env = do
    elemArg ← Lua.tointeger 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (elemArg, xArg, yArg) of
        (Just e, Just x, Just y) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementPosition (ElementHandle $ fromIntegral e) (realToFrac x) (realToFrac y) mgr, ())
        _ → pure ()
    return 0

uiSetSizeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetSizeFn env = do
    elemArg ← Lua.tointeger 1
    wArg ← Lua.tonumber 2
    hArg ← Lua.tonumber 3
    case (elemArg, wArg, hArg) of
        (Just e, Just w, Just h) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementSize (ElementHandle $ fromIntegral e) (realToFrac w) (realToFrac h) mgr, ())
        _ → pure ()
    return 0

uiSetVisibleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetVisibleFn env = do
    elemArg ← Lua.tointeger 1
    visArg ← Lua.toboolean 2
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementVisible (ElementHandle $ fromIntegral e) visArg mgr, ())
        Nothing → pure ()
    return 0

uiSetClickableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetClickableFn env = do
    elemArg ← Lua.tointeger 1
    clickArg ← Lua.toboolean 2
    case elemArg of
        Just e → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementClickable (ElementHandle $ fromIntegral e) clickArg mgr, ())
        Nothing → pure ()
    return 0

uiSetOnClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetOnClickFn env = do
    elemArg ← Lua.tointeger 1
    callbackArg ← Lua.tostring 2
    case (elemArg, callbackArg) of
        (Just e, Just cbBS) → do
            let callback = TE.decodeUtf8 cbBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setElementOnClick (ElementHandle $ fromIntegral e) callback mgr, ())
        _ → pure ()
    return 0

uiSetZIndexFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetZIndexFn env = do
    elemArg ← Lua.tointeger 1
    zArg ← Lua.tointeger 2
    case (elemArg, zArg) of
        (Just e, Just z) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementZIndex (ElementHandle $ fromIntegral e) (fromIntegral z) mgr, ())
        _ → pure ()
    return 0

uiSetColorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetColorFn env = do
    elemArg ← Lua.tointeger 1
    rArg ← Lua.tonumber 2
    gArg ← Lua.tonumber 3
    bArg ← Lua.tonumber 4
    aArg ← Lua.tonumber 5
    case (elemArg, rArg, gArg, bArg, aArg) of
        (Just e, Just r, Just g, Just b, Just a) → do
            let elemHandle = ElementHandle (fromIntegral e)
                color = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                case Map.lookup elemHandle (upmElements mgr) of
                    Nothing → (mgr, ())
                    Just elem → case ueRenderData elem of
                        RenderBox _    → (setBoxColor elemHandle color mgr, ())
                        RenderSprite _ → (setSpriteColor elemHandle color mgr, ())
                        RenderText _   → (setTextColor elemHandle color mgr, ())
                        RenderNone     → (mgr, ())
        _ → pure ()
    return 0

uiSetTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTextFn env = do
    elemArg ← Lua.tointeger 1
    textArg ← Lua.tostring 2
    case (elemArg, textArg) of
        (Just e, Just txtBS) → do
            let elemHandle = ElementHandle (fromIntegral e)
                text = TE.decodeUtf8 txtBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setText elemHandle text mgr, ())
        _ → pure ()
    return 0

uiSetSpriteTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetSpriteTextureFn env = do
    elemArg ← Lua.tointeger 1
    texArg ← Lua.tointeger 2
    case (elemArg, texArg) of
        (Just e, Just t) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setSpriteTexture (ElementHandle $ fromIntegral e) (TextureHandle $ fromIntegral t) mgr, ())
        _ → pure ()
    return 0

-----------------------------------------------------------
-- Texture operations
-----------------------------------------------------------

uiSetBoxTexturesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetBoxTexturesFn env = do
    elemArg ← Lua.tointeger 1
    texArg ← Lua.tointeger 2
    case (elemArg, texArg) of
        (Just e, Just t) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setBoxTextures (ElementHandle $ fromIntegral e) (BoxTextureHandle $ fromIntegral t) mgr, ())
        _ → pure ()
    return 0

uiLoadBoxTexturesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiLoadBoxTexturesFn env = do
    centerArg ← Lua.tointeger 1
    nArg ← Lua.tointeger 2
    sArg ← Lua.tointeger 3
    eArg ← Lua.tointeger 4
    wArg ← Lua.tointeger 5
    neArg ← Lua.tointeger 6
    nwArg ← Lua.tointeger 7
    seArg ← Lua.tointeger 8
    swArg ← Lua.tointeger 9
    case (centerArg, nArg, sArg, eArg, wArg, neArg, nwArg, seArg, swArg) of
        (Just c, Just n, Just s, Just e, Just w, Just ne, Just nw, Just se, Just sw) → do
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
            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (h, newMgr) = registerBoxTextures texSet mgr
                in (newMgr, h)
            Lua.pushinteger (fromIntegral $ unBoxTextureHandle handle)
        _ → Lua.pushnil
    return 1

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

parseLayer ∷ Text → UILayer
parseLayer t = case T.toLower t of
    "hud"     → LayerHUD
    "menu"    → LayerMenu
    "modal"   → LayerModal
    "tooltip" → LayerTooltip
    "debug"   → LayerDebug
    _         → LayerMenu

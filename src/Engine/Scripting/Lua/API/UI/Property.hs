{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for reading and writing element properties:
--   position, size, visibility, clickability, colour, z-index,
--   text content, and texture assignment.
module Engine.Scripting.Lua.API.UI.Property
  ( uiSetPositionFn
  , uiSetSizeFn
  , uiSetVisibleFn
  , uiIsPageVisibleFn
  , uiGetElementInfoFn
  , uiGetVisibleElementsFn
  , uiSetClickableFn
  , uiSetOnClickFn
  , uiSetOnRightClickFn
  , uiSetZIndexFn
  , uiSetColorFn
  , uiSetTextFn
  , uiSetSpriteTextureFn
  , uiSetBoxTexturesFn
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import UI.Types
import UI.Manager

-- | UI.setPosition(elementHandle, x, y)
uiSetPositionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetPositionFn env = do
    elemArg ← Lua.tointeger 1
    xArg    ← Lua.tonumber  2
    yArg    ← Lua.tonumber  3

    case (elemArg, xArg, yArg) of
        (Just e, Just x, Just y) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementPosition (ElementHandle $ fromIntegral e) (realToFrac x) (realToFrac y) mgr, ())
        _ → pure ()

    return 0

-- | UI.setSize(elementHandle, width, height)
uiSetSizeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetSizeFn env = do
    elemArg ← Lua.tointeger 1
    wArg    ← Lua.tonumber  2
    hArg    ← Lua.tonumber  3

    case (elemArg, wArg, hArg) of
        (Just e, Just w, Just h) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementSize (ElementHandle $ fromIntegral e) (realToFrac w) (realToFrac h) mgr, ())
        _ → pure ()

    return 0

-- | UI.setVisible(elementHandle, visible)
uiSetVisibleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetVisibleFn env = do
    elemArg ← Lua.tointeger 1
    visArg  ← Lua.toboolean 2

    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementVisible (ElementHandle $ fromIntegral e) visArg mgr, ())
        Nothing → pure ()

    return 0

-- | UI.isPageVisible(pageHandle) -> boolean
uiIsPageVisibleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiIsPageVisibleFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → do
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case getPage (PageHandle $ fromIntegral n) mgr of
                Just page → Lua.pushboolean (upVisible page)
                Nothing   → Lua.pushboolean False
        Nothing → Lua.pushboolean False
    return 1

-- | Push a table with one element's info fields onto the Lua stack:
--   { handle, x, y (absolute framebuffer-pixel position), width,
--     height, visible, clickable, interactive (has an onClick or
--     onRightClick callback), zIndex, name, text, page (page name),
--     pageVisible, hovered, focused }. Shared by 'uiGetElementInfoFn'
--   (one element by handle) and 'uiGetVisibleElementsFn' (every
--   element on every visible page) so both report identical fields.
--
--   visible is 'isEffectivelyVisible' (this element AND every ancestor
--   up to the page root), matching what the renderer/hit-tester
--   actually treat as "on screen" — a visible child of a hidden parent
--   (e.g. a collapsed panel) is not on screen either. pageVisible is a
--   separate, coarser signal: an element can be visible=true while
--   sitting on a page that's currently hidden/inactive (UI.showPage/
--   hidePage), so a caller that wants "actually rendered right now"
--   needs both.
--
--   text is a best-effort visible caption (see 'elementText') — the
--   element's own text if it IS a text element, else its first
--   text-rendering child. Present for screens that build clickable
--   elements out of raw UI.newBox/UI.newText instead of a
--   scripts/ui/*.lua widget module (e.g. the main menu), where there's
--   no Lua-side cache of the label to fall back on.
pushElementInfoTable ∷ ElementHandle → UIElement → UIPageManager → Lua.LuaE Lua.Exception ()
pushElementInfoTable handle el mgr = do
    let (ax, ay) = fromMaybe (0, 0) (getElementAbsolutePosition handle mgr)
        (w, h)   = ueSize el
        pageName = maybe "" upName (Map.lookup (uePage el) (upmPages mgr))
        pageVisible = uePage el `Set.member` upmVisiblePages mgr
        isHovered = upmHovered mgr ≡ Just handle
        isFocused = upmGlobalFocus mgr ≡ Just handle
        isInteractive = isJust (ueOnClick el) ∨ isJust (ueOnRightClick el)
        visible = isEffectivelyVisible handle mgr
        mText = elementText el mgr
    Lua.newtable
    Lua.pushinteger (fromIntegral (unElementHandle handle))
    Lua.setfield (Lua.nth 2) "handle"
    Lua.pushnumber (realToFrac ax)
    Lua.setfield (Lua.nth 2) "x"
    Lua.pushnumber (realToFrac ay)
    Lua.setfield (Lua.nth 2) "y"
    Lua.pushnumber (realToFrac w)
    Lua.setfield (Lua.nth 2) "width"
    Lua.pushnumber (realToFrac h)
    Lua.setfield (Lua.nth 2) "height"
    Lua.pushboolean visible
    Lua.setfield (Lua.nth 2) "visible"
    Lua.pushboolean (ueClickable el)
    Lua.setfield (Lua.nth 2) "clickable"
    Lua.pushboolean isInteractive
    Lua.setfield (Lua.nth 2) "interactive"
    Lua.pushinteger (fromIntegral (ueZIndex el))
    Lua.setfield (Lua.nth 2) "zIndex"
    Lua.pushstring (TE.encodeUtf8 (ueName el))
    Lua.setfield (Lua.nth 2) "name"
    case mText of
        Just t  → Lua.pushstring (TE.encodeUtf8 t)
        Nothing → Lua.pushnil
    Lua.setfield (Lua.nth 2) "text"
    Lua.pushstring (TE.encodeUtf8 pageName)
    Lua.setfield (Lua.nth 2) "page"
    Lua.pushboolean pageVisible
    Lua.setfield (Lua.nth 2) "pageVisible"
    Lua.pushboolean isHovered
    Lua.setfield (Lua.nth 2) "hovered"
    Lua.pushboolean isFocused
    Lua.setfield (Lua.nth 2) "focused"

-- | UI.getElementInfo(elementHandle) -> table or nil (see
--   'pushElementInfoTable' for the field list) — the authoritative
--   engine-side state for one element, for introspection callers
--   (e.g. ui.dumpWidgets) that can't trust a widget module's own
--   cached copy to stay in sync with UI.setVisible/setClickable/
--   setPosition calls made after construction.
uiGetElementInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetElementInfoFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Nothing → Lua.pushnil >> return 1
        Just e  → do
            let handle = ElementHandle (fromIntegral e)
            mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
            case getElement handle mgr of
                Nothing → Lua.pushnil >> return 1
                Just el → pushElementInfoTable handle el mgr >> return 1

-- | UI.getVisibleElements() -> array of element info tables (see
--   'pushElementInfoTable'), one per element on every currently-
--   visible page. There is no single authoritative widget registry
--   (see scripts/ui/registry.lua) — some screens (e.g. the main menu)
--   build their clickable elements with raw UI.newBox/UI.setOnClick
--   calls instead of going through a scripts/ui/*.lua widget module,
--   so ui.dumpWidgets() (F3, #645) falls back to this bulk read of
--   the engine's own element tree to catch those too.
uiGetVisibleElementsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetVisibleElementsFn env = do
    mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
    let els = concatMap (\p → getPageElements (upHandle p) mgr) (getVisiblePages mgr)
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] els) $ \(i, el) → do
        pushElementInfoTable (ueHandle el) el mgr
        Lua.rawseti (Lua.nth 2) (fromIntegral i)
    return 1

-- | UI.setClickable(elementHandle, clickable)
uiSetClickableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetClickableFn env = do
    elemArg  ← Lua.tointeger 1
    clickArg ← Lua.toboolean 2

    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementClickable (ElementHandle $ fromIntegral e) clickArg mgr, ())
        Nothing → pure ()

    return 0

-- | UI.setOnClick(elementHandle, callbackName)
uiSetOnClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetOnClickFn env = do
    elemArg     ← Lua.tointeger 1
    callbackArg ← Lua.tostring  2

    case (elemArg, callbackArg) of
        (Just e, Just cbBS) → do
            let callback = TE.decodeUtf8 cbBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setElementOnClick (ElementHandle $ fromIntegral e) callback mgr, ())
        _ → pure ()

    return 0

-- | UI.setOnRightClick(elementHandle, callbackName)
uiSetOnRightClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetOnRightClickFn env = do
    elemArg     ← Lua.tointeger 1
    callbackArg ← Lua.tostring  2

    case (elemArg, callbackArg) of
        (Just e, Just cbBS) → do
            let callback = TE.decodeUtf8 cbBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setElementOnRightClick (ElementHandle $ fromIntegral e) callback mgr, ())
        _ → pure ()

    return 0

-- | UI.setZIndex(elementHandle, z)
uiSetZIndexFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetZIndexFn env = do
    elemArg ← Lua.tointeger 1
    zArg    ← Lua.tointeger 2

    case (elemArg, zArg) of
        (Just e, Just z) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementZIndex (ElementHandle $ fromIntegral e) (fromIntegral z) mgr, ())
        _ → pure ()

    return 0

-- | UI.setColor(elementHandle, r, g, b, a)
--
--   Dispatches to the correct colour setter based on the element's
--   render-data variant (box, sprite, or text).
uiSetColorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetColorFn env = do
    elemArg ← Lua.tointeger 1
    rArg    ← Lua.tonumber  2
    gArg    ← Lua.tonumber  3
    bArg    ← Lua.tonumber  4
    aArg    ← Lua.tonumber  5

    case (elemArg, rArg, gArg, bArg, aArg) of
        (Just e, Just r, Just g, Just b, Just a) → do
            let elemHandle = ElementHandle (fromIntegral e)
                color      = (realToFrac r, realToFrac g, realToFrac b, realToFrac a)
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                case Map.lookup elemHandle (upmElements mgr) of
                    Nothing   → (mgr, ())
                    Just elem → case ueRenderData elem of
                        RenderBox    _ → (setBoxColor    elemHandle color mgr, ())
                        RenderSprite _ → (setSpriteColor elemHandle color mgr, ())
                        RenderText   _ → (setTextColor   elemHandle color mgr, ())
                        RenderNone     → (mgr, ())
        _ → pure ()

    return 0

-- | UI.setText(elementHandle, text)
uiSetTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetTextFn env = do
    elemArg ← Lua.tointeger 1
    textArg ← Lua.tostring  2

    case (elemArg, textArg) of
        (Just e, Just txtBS) → do
            let elemHandle = ElementHandle (fromIntegral e)
                text       = TE.decodeUtf8 txtBS
            Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                (setText elemHandle text mgr, ())
        _ → pure ()

    return 0

-- | UI.setSpriteTexture(elementHandle, textureHandle)
uiSetSpriteTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetSpriteTextureFn env = do
    elemArg ← Lua.tointeger 1
    texArg  ← Lua.tointeger 2

    case (elemArg, texArg) of
        (Just e, Just t) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setSpriteTexture (ElementHandle $ fromIntegral e) (TextureHandle $ fromIntegral t) mgr, ())
        _ → pure ()

    return 0

-- | UI.setBoxTextures(elementHandle, boxTextureHandle)
uiSetBoxTexturesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetBoxTexturesFn env = do
    elemArg ← Lua.tointeger 1
    texArg  ← Lua.tointeger 2

    case (elemArg, texArg) of
        (Just e, Just t) → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setBoxTextures (ElementHandle $ fromIntegral e) (BoxTextureHandle $ fromIntegral t) mgr, ())
        _ → pure ()

    return 0

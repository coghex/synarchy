{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for page lifecycle: creation, deletion, show/hide.
module Engine.Scripting.Lua.API.UI.Page
  ( uiNewPageFn
  , uiDeletePageFn
  , uiShowPageFn
  , uiHidePageFn
  , uiSetPageInputExclusiveFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.API.UI.Focus (applyAndNotifyControlFocus)
import UI.Types
import UI.Manager

-- | UI.newPage(name, layer) -> pageHandle
uiNewPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewPageFn env = do
    nameArg ← Lua.tostring 1
    layerArg ← Lua.tostring 2

    case (nameArg, layerArg) of
        (Just nameBS, Just layerBS) → do
            let name  = TE.decodeUtf8Lenient nameBS
                layer = parseLayer (TE.decodeUtf8Lenient layerBS)

            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (h, newMgr) = createPage name layer mgr
                in (newMgr, h)

            Lua.pushinteger (fromIntegral $ unPageHandle handle)
        _ → Lua.pushnil

    return 1

-- | UI.deletePage(pageHandle) — #745 review round 6: deletePage
--   recursively deletes every element it owns (deleteElementTree),
--   which already clears upmControlFocus for whichever handle it
--   matches; report it the same way uiHidePageFn/the Hierarchy
--   delete/detach bindings do.
uiDeletePageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeletePageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ applyAndNotifyControlFocus env $
            deletePage (PageHandle $ fromIntegral n)
        Nothing → pure ()
    return 0

-- | UI.showPage(pageHandle)
uiShowPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiShowPageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (showPage (PageHandle $ fromIntegral n) mgr, ())
        Nothing → pure ()
    return 0

-- | UI.hidePage(pageHandle) — #745 review round 4: hidePage proactively
--   clears upmControlFocus (see UI.Manager.Page) for a control focused
--   on the hidden page, same as it already does for text focus. That's
--   a pure mutation with no notification of its own, and unlike a
--   direct UI.clearControlFocus() call the calling script may have no
--   idea it happened — report it here so a Lua focus-ring consumer
--   (button.lua) doesn't keep pointing at a page that's gone.
uiHidePageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiHidePageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ applyAndNotifyControlFocus env $
            hidePage (PageHandle $ fromIntegral n)
        Nothing → pure ()
    return 0

-- | UI.setPageInputExclusive(pageHandle, exclusive) — #742: override a
--   page's default modal-boundary classification (@upLayer ≡ LayerModal@
--   at creation). Used by e.g. scripts/popup.lua to keep its
--   notification-card page pass-through despite sitting on the modal
--   layer for visual stacking only.
uiSetPageInputExclusiveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetPageInputExclusiveFn env = do
    handleArg    ← Lua.tointeger 1
    exclusiveArg ← Lua.toboolean 2
    case handleArg of
        Just n → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setPageInputExclusive (PageHandle $ fromIntegral n) exclusiveArg mgr, ())
        Nothing → pure ()
    return 0

-- | Parse a layer name string into a 'UILayer'.  Defaults to 'LayerMenu'
--   for unrecognised input.
parseLayer ∷ Text → UILayer
parseLayer t = case T.toLower t of
    "hud"     → LayerHUD
    -- "overlay" previously fell through to LayerMenu, so the HUD
    -- chrome shared a band with real menus and stacking between them
    -- was accidental (decided by element zIndexes + page order).
    "overlay" → LayerOverlay
    "menu"    → LayerMenu
    "modal"   → LayerModal
    "tooltip" → LayerTooltip
    "debug"   → LayerDebug
    _         → LayerMenu

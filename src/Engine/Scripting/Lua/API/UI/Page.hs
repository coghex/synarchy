{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for page lifecycle: creation, deletion, show/hide.
module Engine.Scripting.Lua.API.UI.Page
  ( uiNewPageFn
  , uiDeletePageFn
  , uiShowPageFn
  , uiHidePageFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import UI.Types
import UI.Manager

-----------------------------------------------------------
-- Page Functions
-----------------------------------------------------------

-- | UI.newPage(name, layer) -> pageHandle
uiNewPageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiNewPageFn env = do
    nameArg ← Lua.tostring 1
    layerArg ← Lua.tostring 2

    case (nameArg, layerArg) of
        (Just nameBS, Just layerBS) → do
            let name  = TE.decodeUtf8 nameBS
                layer = parseLayer (TE.decodeUtf8 layerBS)

            handle ← Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
                let (h, newMgr) = createPage name layer mgr
                in (newMgr, h)

            Lua.pushinteger (fromIntegral $ unPageHandle handle)
        _ → Lua.pushnil

    return 1

-- | UI.deletePage(pageHandle)
uiDeletePageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiDeletePageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (deletePage (PageHandle $ fromIntegral n) mgr, ())
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

-- | UI.hidePage(pageHandle)
uiHidePageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiHidePageFn env = do
    handleArg ← Lua.tointeger 1
    case handleArg of
        Just n → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (hidePage (PageHandle $ fromIntegral n) mgr, ())
        Nothing → pure ()
    return 0

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- | Parse a layer name string into a 'UILayer'.  Defaults to 'LayerMenu'
--   for unrecognised input.
parseLayer ∷ Text → UILayer
parseLayer t = case T.toLower t of
    "hud"     → LayerHUD
    "menu"    → LayerMenu
    "modal"   → LayerModal
    "tooltip" → LayerTooltip
    "debug"   → LayerDebug
    _         → LayerMenu

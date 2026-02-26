{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua bindings for keyboard/input focus management.
module Engine.Scripting.Lua.API.UI.Focus
  ( uiSetFocusFn
  , uiClearFocusFn
  , uiGetFocusFn
  , uiHasFocusFn
  ) where

import UPrelude
import qualified HsLua as Lua
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import UI.Types
import UI.Manager

-----------------------------------------------------------
-- Focus Operations
-----------------------------------------------------------

-- | UI.setFocus(elementHandle)
uiSetFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiSetFocusFn env = do
    elemArg ← Lua.tointeger 1
    case elemArg of
        Just e  → Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
            (setElementFocus (ElementHandle $ fromIntegral e) mgr, ())
        Nothing → pure ()
    return 0

-- | UI.clearFocus()
uiClearFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiClearFocusFn env = do
    Lua.liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
        (clearElementFocus mgr, ())
    return 0

-- | UI.getFocus() -> elementHandle or nil
uiGetFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiGetFocusFn env = do
    mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
    case getElementFocus mgr of
        Just (ElementHandle h) → Lua.pushinteger (fromIntegral h)
        Nothing                → Lua.pushnil
    return 1

-- | UI.hasFocus(elementHandle) -> boolean
uiHasFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
uiHasFocusFn env = do
    elemArg ← Lua.tointeger 1
    mgr ← Lua.liftIO $ readIORef (uiManagerRef env)
    let isFocused = case (elemArg, getElementFocus mgr) of
            (Just e, Just (ElementHandle h)) → fromIntegral e ≡ h
            _                                → False
    Lua.pushboolean isFocused
    return 1

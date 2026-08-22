-- | Lua bindings for SHELL\/CONSOLE text-input focus — the
--   @engine.registerFocusable@, @engine.requestFocus@,
--   @engine.releaseFocus@, @engine.getFocusId@ family, which drives
--   "UI.ShellFocus"'s 'UI.ShellFocus.FocusManager' through
--   @uicFocusManagerRef@. Its only consumer is @scripts\/shell.lua@'s
--   debug-console input line.
--
--   This is NOT the game-UI focus binding: element TEXT focus and
--   keyboard CONTROL focus are exposed as the @UI.*@ family in
--   "Engine.Scripting.Lua.API.UI.Focus" over 'UI.Types.UIPageManager'.
module Engine.Scripting.Lua.API.ShellFocus
  ( registerFocusableFn
  , requestFocusFn
  , releaseFocusFn
  , getFocusIdFn
  ) where

import UPrelude
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Ui (UiCapability(..), toUiCapability)
import UI.ShellFocus (FocusId(..), registerFocusTarget, setFocus, clearFocus, fmCurrentFocus)
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')

registerFocusableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
registerFocusableFn env = do
  acceptsText ← Lua.toboolean 1
  tabIndex ← Lua.tointeger 2
  case tabIndex of
    Just (Lua.Integer idx) → do
      fid ← Lua.liftIO $ atomicModifyIORef' (uicFocusManagerRef (toUiCapability env)) $ \fm →
        let (newFid, newFm) = registerFocusTarget acceptsText (fromIntegral idx) fm
        in (newFm, newFid)
      let (FocusId n) = fid
      Lua.pushinteger (Lua.Integer $ fromIntegral n)
    Nothing → Lua.pushnil
  return 1

requestFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
requestFocusFn env = do
  mFid ← Lua.tointeger 1
  case mFid of
    Just (Lua.Integer n) → Lua.liftIO $
      atomicModifyIORef' (uicFocusManagerRef (toUiCapability env)) $ \fm →
        (setFocus (FocusId $ fromIntegral n) fm, ())
    Nothing → return ()
  return 0

releaseFocusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
releaseFocusFn env = do
  Lua.liftIO $ atomicModifyIORef' (uicFocusManagerRef (toUiCapability env)) $ \fm →
      (clearFocus fm, ())
  return 0

getFocusIdFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getFocusIdFn env = do
  fm ← Lua.liftIO $ readIORef (uicFocusManagerRef (toUiCapability env))
  case fmCurrentFocus fm of
    Just (FocusId n) → Lua.pushinteger (Lua.Integer $ fromIntegral n)
    Nothing          → Lua.pushnil
  return 1

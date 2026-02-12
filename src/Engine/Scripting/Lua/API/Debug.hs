module Engine.Scripting.Lua.API.Debug
  ( showDebugFn
  , hideDebugFn
  , toggleDebugFn
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua

-- | engine.showDebug()
showDebugFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
showDebugFn backendState = do
    Lua.liftIO $ do
        let (_, etlq) = lbsMsgQueues backendState
        Q.writeQueue etlq LuaDebugShow
    return 0

-- | engine.hideDebug()
hideDebugFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
hideDebugFn backendState = do
    Lua.liftIO $ do
        let (_, etlq) = lbsMsgQueues backendState
        Q.writeQueue etlq LuaDebugHide
    return 0

-- | engine.toggleDebug()
toggleDebugFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
toggleDebugFn backendState = do
    Lua.liftIO $ do
        let (_, etlq) = lbsMsgQueues backendState
        Q.writeQueue etlq LuaDebugToggle
    return 0

module Engine.Scripting.Lua.API.Debug
  ( setFluidSurfaceFn
  , showDebugFn
  , hideDebugFn
  , toggleDebugFn
  ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import World.Command.Types (WorldCommand(..))
import World.Page.Types (WorldPageId(..))
import World.Fluid.Types (FluidType(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua

showDebugFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
showDebugFn backendState = do
    Lua.liftIO $ do
        let (_, etlq) = lbsMsgQueues backendState
        Q.writeQueue etlq LuaDebugShow
    return 0

hideDebugFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
hideDebugFn backendState = do
    Lua.liftIO $ do
        let (_, etlq) = lbsMsgQueues backendState
        Q.writeQueue etlq LuaDebugHide
    return 0

toggleDebugFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
toggleDebugFn backendState = do
    Lua.liftIO $ do
        let (_, etlq) = lbsMsgQueues backendState
        Q.writeQueue etlq LuaDebugToggle
    return 0

-- | debug.setFluidSurface(page, x, y, kind, exactUnits) -> queued.
--   A fixture authoring hook, not a public exact-height query. Unknown kinds
--   and missing/noninteger arguments are rejected. Mutation is world-owned.
setFluidSurfaceFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
setFluidSurfaceFn wsc = do
    page ← Lua.tostring 1
    gx ← Lua.tointeger 2
    gy ← Lua.tointeger 3
    kind ← Lua.tostring 4
    surface ← Lua.tointeger 5
    let fluid = case kind of
            Just "lake" → Just Lake
            Just "river" → Just River
            Just "ocean" → Just Ocean
            Just "lava" → Just Lava
            _ → Nothing
    case (page, gx, gy, fluid, surface) of
        (Just pid, Just x, Just y, Just ft, Just units) → do
            Lua.liftIO $ Q.writeQueue (wsWorldQueue wsc) $
                WorldDebugSetFluidSurface (WorldPageId (TE.decodeUtf8Lenient pid))
                    (fromIntegral x) (fromIntegral y) ft (fromIntegral units)
            Lua.pushboolean True
        _ → Lua.pushboolean False
    return 1

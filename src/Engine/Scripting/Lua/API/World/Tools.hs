{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World.Tools
    ( worldSetToolModeFn
    , worldGetToolModeFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.State (activeWorldStateFrom)
import World.Types

worldSetToolModeFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetToolModeFn wsc = do
    pageIdArg ← Lua.tostring 1
    toolModeArg ← Lua.tostring 2
    case (pageIdArg, toolModeArg) of
        (Just pageIdBS, Just toolModeBS) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                toolMode = TE.decodeUtf8Lenient toolModeBS
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetToolMode pageId $ textToToolMode toolMode
        _ → pure ()
    return 0

-- | world.getToolMode() → "info" | "default" | "mine" | "build"
--   | "construct" | "chop" | "till" | nil
--   Returns the current tool mode for the first visible world, or nil
--   if no world is active. Reads `wsToolModeRef` directly so callers
--   see the world thread's view of the tool state.
worldGetToolModeFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetToolModeFn wsc = do
    mWs ← Lua.liftIO $ activeWorldStateFrom (wsWorldManagerRef wsc)
    case mWs of
        Just ws → do
            tm ← Lua.liftIO $ readIORef (wsToolModeRef ws)
            let s = case tm of
                    InfoTool      → "info"
                    DefaultTool   → "default"
                    MineTool      → "mine"
                    BuildTool     → "build"
                    ConstructTool → "construct"
                    ChopTool      → "chop"
                    TillTool      → "till"
                    PlantTool     → "plant"
            Lua.pushstring s
            return 1
        Nothing → do
            Lua.pushnil
            return 1

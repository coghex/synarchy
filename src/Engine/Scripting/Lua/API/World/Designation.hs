{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World.Designation
    ( worldSetMineAnchorFn
    , worldClearMineAnchorFn
    , worldDesignateMineFn
    , worldSetMineDesignateTextureFn
    , worldGetMineDesignationCountFn
    , worldNearestMineDesignationFn
    , worldGetMineDesignationAtFn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import World.Mine.Types (MineDesignation(..))

-- * Mine designation tool

-- | world.setMineAnchor(pageId, gx, gy) — anchor the designation
--   rectangle at the given tile (mine tool first click).
worldSetMineAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMineAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetMineAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | world.clearMineAnchor(pageId) — cancel the pending rectangle.
worldClearMineAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldClearMineAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldClearMineAnchor pageId
        _ → pure ()
    return 0

-- | world.designateMine(pageId, x1, y1, x2, y2) — commit the
--   rectangle (corners in either order; mine tool second click).
worldDesignateMineFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDesignateMineFn env = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldDesignateMine pageId (round x1) (round y1)
                                          (round x2) (round y2)
        _ → pure ()
    return 0

-- | world.setMineDesignateTexture(pageId, texHandle) — marker texture
--   for committed designations.
worldSetMineDesignateTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMineDesignateTextureFn env = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetMineDesignateTexture pageId texHandle
        _ → pure ()
    return 0

-- | world.nearestMineDesignation(pageId, x, y) → gx, gy, dist | nil
--   Nearest designated tile to (x, y) by Euclidean distance — the
--   "distance to the nearest dig job" term in the dig utility. Linear
--   scan of the designation map (synchronous read).
worldNearestMineDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldNearestMineDesignationFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                ux = realToFrac x ∷ Float
                uy = realToFrac y ∷ Float
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    let dist2 (gx, gy) =
                            let dx = fromIntegral gx - ux
                                dy = fromIntegral gy - uy
                            in dx * dx + dy * dy
                        best = foldl' (\acc k → case acc of
                                  Nothing → Just (k, dist2 k)
                                  Just (_, d) | dist2 k < d → Just (k, dist2 k)
                                  _ → acc)
                                Nothing (HM.keys m)
                    case best of
                        Just ((gx, gy), d2) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            return 3
                        Nothing → do
                            Lua.pushnil
                            return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.getMineDesignationAt(pageId, gx, gy)
--     → z, cNW, cNE, cSE, cSW | nil
--   Designation state at a tile, including corner dig progress (the
--   AI's "how far along is this tile" query).
worldGetMineDesignationAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetMineDesignationAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    case HM.lookup (round gxN, round gyN) m of
                        Nothing → Lua.pushnil >> return 1
                        Just md → do
                            let (a, b, c, d) = mdCorners md
                            Lua.pushinteger (fromIntegral (mdZ md))
                            Lua.pushnumber (Lua.Number (realToFrac a))
                            Lua.pushnumber (Lua.Number (realToFrac b))
                            Lua.pushnumber (Lua.Number (realToFrac c))
                            Lua.pushnumber (Lua.Number (realToFrac d))
                            return 5
        _ → Lua.pushnil >> return 1

-- | world.getMineDesignationCount(pageId) → n — number of designated
--   tiles. Reads the ref directly (synchronous; for HUD readouts and
--   headless tests).
worldGetMineDesignationCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetMineDesignationCountFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    Lua.pushinteger (fromIntegral (HM.size m))
                    return 1
                Nothing → do
                    Lua.pushinteger 0
                    return 1
        _ → do
            Lua.pushinteger 0
            return 1

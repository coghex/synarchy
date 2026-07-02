{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua API for the chop-designation tool (issue #97) — the @chop.*@
--   namespace. Mirrors the construction-designation API (#95): the tool
--   drives setAnchor / clearAnchor / designate, the chop AI
--   (scripts/unit_ai.lua) drives nearestDesignation / getDesignationAt /
--   cancelDesignation (claims are Lua-side like dig jobs, so there is no
--   engine-side job status), and the HUD sets the marker texture.
module Engine.Scripting.Lua.API.Chop
    ( chopSetAnchorFn
    , chopClearAnchorFn
    , chopDesignateFn
    , chopCancelDesignationFn
    , chopGetDesignationAtFn
    , chopGetDesignationCountFn
    , chopNearestDesignationFn
    , chopSetDesignateTextureFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Page.Types (WorldPageId(..))
import World.Command.Types (WorldCommand(..))
import World.Chop.Types

-- | chop.setAnchor(pageId, gx, gy) — first-click anchor.
chopSetAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopSetAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetChopAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | chop.clearAnchor(pageId) — cancel the pending rectangle.
chopClearAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopClearAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldClearChopAnchor pageId
        _ → pure ()
    return 0

-- | chop.designate(pageId, x1, y1, x2, y2 [, tag]) — commit the
--   rectangle. Only tiles holding a currently-harvestable flora species
--   carrying @tag@ (default "wood") are designated — sweeping a forest
--   marks the trees, not the ground between them.
chopDesignateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopDesignateFn env = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    tagArg ← Lua.tostring 6
    case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                tag = maybe "wood" TE.decodeUtf8 tagArg
            Q.writeQueue (worldQueue env) $
                WorldDesignateChop pageId
                    (round x1) (round y1) (round x2) (round y2) tag
        _ → pure ()
    return 0

-- | chop.cancelDesignation(gx, gy) — remove the designation at a tile
--   on the active world. Both the player-cancel path and the chop AI's
--   completion call this (best-effort, returns nothing).
chopCancelDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopCancelDesignationFn env = do
    gxArg ← Lua.tonumber 1
    gyArg ← Lua.tonumber 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mPage ← Lua.liftIO $ activeWorldPage env
            case mPage of
                Just (pageId, _) → Lua.liftIO $
                    Q.writeQueue (worldQueue env) $
                        WorldCancelChop pageId (round gx) (round gy)
                Nothing → pure ()
        _ → pure ()
    return 0

-- | chop.getDesignationAt(pageId, gx, gy) → {x, y, z} | nil.
chopGetDesignationAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopGetDesignationAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                gx = round gxN ∷ Int
                gy = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
                    case HM.lookup (gx, gy) m of
                        Just cd → do
                            Lua.newtable
                            Lua.pushinteger (fromIntegral gx)
                            Lua.setfield (Lua.nth 2) "x"
                            Lua.pushinteger (fromIntegral gy)
                            Lua.setfield (Lua.nth 2) "y"
                            Lua.pushinteger (fromIntegral (chZ cd))
                            Lua.setfield (Lua.nth 2) "z"
                            return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | chop.getDesignationCount(pageId) → n.
chopGetDesignationCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopGetDesignationCountFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
                    Lua.pushinteger (fromIntegral (HM.size m))
                    return 1
                Nothing → Lua.pushinteger 0 >> return 1
        _ → Lua.pushinteger 0 >> return 1

-- | chop.nearestDesignation(pageId, x, y) → gx, gy, dist | nil.
--   Nearest designated tree by Euclidean distance — the chop AI's
--   "distance to nearest chop job" term. Mirrors nearestMineDesignation.
chopNearestDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopNearestDesignationFn env = do
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
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
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
                        Nothing → Lua.pushnil >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | chop.setDesignateTexture(pageId, texHandle) — marker texture for
--   committed chop designations.
chopSetDesignateTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopSetDesignateTextureFn env = do
    pageIdArg ← Lua.tostring 1
    handleArg ← Lua.tointeger 2
    case (pageIdArg, handleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetChopDesignateTexture pageId texHandle
        _ → pure ()
    return 0

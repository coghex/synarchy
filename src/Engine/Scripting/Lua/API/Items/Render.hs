{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Ground-item render-facing queries: window-space hit testing, the
--   world-view ground-item selection (white outline + info panel), and
--   a headless-safe debug dump of the actual ground-item render pass.
--   Split from Engine.Scripting.Lua.API.Items (#577) — item def
--   loading lives in Items.Defs, ground-item CRUD in Items.Ground.
module Engine.Scripting.Lua.API.Items.Render
    ( itemHitTestAtFn
    , itemSelectFn
    , itemDeselectFn
    , itemGetSelectedFn
    , itemDebugQuadsFn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldState)
import World.Cursor.Types (CursorState(..))
import qualified Data.Vector as V
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Render.GroundItemQuads (hitTestGroundItemAt
                                    , renderGroundItemQuads)
import World.Types (WorldState(..))
import Item.Ground (GroundItems(..))

-- | item.hitTestAt(px, py) → gid | nil — topmost ground item whose
--   sprite contains the window-pixel point (unit.hitTestAt analog).
itemHitTestAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemHitTestAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → do
            mWs ← Lua.liftIO $ activeWorldState env
            case mWs of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    mGid ← Lua.liftIO $
                        hitTestGroundItemAt env ws (realToFrac x)
                                                   (realToFrac y)
                    case mGid of
                        Just gid → Lua.pushinteger (fromIntegral gid)
                        Nothing  → Lua.pushnil
                    return 1
        _ → Lua.pushnil >> return 1

-- | item.select(gid) / item.deselect() / item.getSelected() — the
--   world-view ground-item selection (white outline + info panel).
itemSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemSelectFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            mWs ← Lua.liftIO $ activeWorldState env
            case mWs of
                Just ws → Lua.liftIO $
                    atomicModifyIORef' (wsCursorRef ws) $ \cs →
                        (cs { selectedGroundItem =
                                Just (fromIntegral n) }, ())
                Nothing → pure ()
        _ → pure ()
    return 0

itemDeselectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemDeselectFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just ws → Lua.liftIO $
            atomicModifyIORef' (wsCursorRef ws) $ \cs →
                (cs { selectedGroundItem = Nothing }, ())
        Nothing → pure ()
    return 0

itemGetSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGetSelectedFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Nothing → Lua.pushnil >> return 1
        Just ws → do
            cs ← Lua.liftIO $ readIORef (wsCursorRef ws)
            case selectedGroundItem cs of
                Just gid → Lua.pushinteger (fromIntegral gid)
                Nothing  → Lua.pushnil
            return 1

-- | item.debugQuads() — run the actual ground-item render pass and
--   report what it produced: how many ground items exist, how many
--   quads survived geometry + culling, the camera snapshot the pass
--   saw, and the first few quads' screen rects. Splits "items aren't
--   spawning" from "geometry/culling drops them" from "they reach the
--   GPU but the shader hides them" without needing a debugger on the
--   render loop. Headless-safe (no GPU touched — slot lookups just
--   return 0 without a texture system).
itemDebugQuadsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemDebugQuadsFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Nothing → Lua.pushnil >> return 1
        Just ws → do
            (quads, gis, cam) ← Lua.liftIO $ do
                q ← renderGroundItemQuads env ws 1.0
                g ← readIORef (wsGroundItemsRef ws)
                c ← readIORef (cameraRef env)
                pure (q, g, c)
            let (camX, camY) = camPosition cam
            Lua.newtable
            Lua.pushinteger (fromIntegral (HM.size (gisItems gis)))
            Lua.setfield (Lua.nth 2) "groundItems"
            Lua.pushinteger (fromIntegral (V.length quads))
            Lua.setfield (Lua.nth 2) "quads"
            Lua.pushnumber (Lua.Number (realToFrac camX))
            Lua.setfield (Lua.nth 2) "camX"
            Lua.pushnumber (Lua.Number (realToFrac camY))
            Lua.setfield (Lua.nth 2) "camY"
            Lua.pushinteger (fromIntegral (camZSlice cam))
            Lua.setfield (Lua.nth 2) "zSlice"
            Lua.pushnumber (Lua.Number (realToFrac (camZoom cam)))
            Lua.setfield (Lua.nth 2) "zoom"
            Lua.newtable
            forM_ (zip [1 ∷ Int ..]
                       (take 8 (V.toList quads))) $ \(i, sq) → do
                let Vec2 qx qy = pos (sqV0 sq)
                    Vec2 q2x q2y = pos (sqV2 sq)
                Lua.newtable
                Lua.pushnumber (Lua.Number (realToFrac qx))
                Lua.setfield (Lua.nth 2) "x0"
                Lua.pushnumber (Lua.Number (realToFrac qy))
                Lua.setfield (Lua.nth 2) "y0"
                Lua.pushnumber (Lua.Number (realToFrac q2x))
                Lua.setfield (Lua.nth 2) "x1"
                Lua.pushnumber (Lua.Number (realToFrac q2y))
                Lua.setfield (Lua.nth 2) "y1"
                Lua.pushnumber (Lua.Number
                    (realToFrac (atlasId (sqV0 sq))))
                Lua.setfield (Lua.nth 2) "texSlot"
                Lua.pushnumber (Lua.Number
                    (realToFrac (faceMapId (sqV0 sq))))
                Lua.setfield (Lua.nth 2) "fmSlot"
                Lua.pushnumber (Lua.Number
                    (realToFrac (sqSortKey sq)))
                Lua.setfield (Lua.nth 2) "sortKey"
                Lua.rawseti (Lua.nth 2) (fromIntegral i)
            Lua.setfield (Lua.nth 2) "sample"
            return 1

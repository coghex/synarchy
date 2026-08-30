{-# LANGUAGE Strict #-}
-- | Lua API for the till-designation tool (issue #333) — the @till.*@
--   namespace. Mirrors the chop-designation API (#97): the tool drives
--   setAnchor / clearAnchor / designate, the till AI
--   (scripts/unit_ai.lua) drives nearestDesignation / getDesignationAt /
--   cancelDesignation (claims are Lua-side like dig/chop jobs, so there
--   is no engine-side job status), and the HUD sets the marker texture.
--   Completion goes through the generic @world.setVegAt@ primitive, not
--   a till-specific one (mirrors how chop completion calls
--   world.harvestFlora before chop.cancelDesignation).
module Engine.Scripting.Lua.API.Till
    ( tillSetAnchorFn
    , tillClearAnchorFn
    , tillDesignateFn
    , tillCancelDesignationFn
    , tillGetDesignationAtFn
    , tillGetDesignationCountFn
    , tillNearestDesignationFn
    , tillSetDesignateTextureFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.State (activeWorldPageFrom)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldManager(..), WorldState(..), pageWrapWorldSize)
import World.Page.Types (WorldPageId(..))
import World.Command.Types (WorldCommand(..))
import World.Generate.Coordinates (canonicalTile, seamTileDist2)
import World.Till.Types

-- | till.setAnchor(pageId, gx, gy) — first-click anchor.
tillSetAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillSetAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetTillAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | till.clearAnchor(pageId) — cancel the pending rectangle.
tillClearAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillClearAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $ WorldClearTillAnchor pageId
        _ → pure ()
    return 0

-- | till.designate(pageId, x1, y1, x2, y2) — commit the rectangle. Only
--   tillable tiles at the anchor's surface z (level ground, no fluid,
--   no flora, not already tilled) are designated.
tillDesignateFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillDesignateFn wsc = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldDesignateTill pageId
                    (round x1) (round y1) (round x2) (round y2)
        _ → pure ()
    return 0

-- | till.cancelDesignation(gx, gy) — remove the designation at a tile
--   on the active world. Both the player-cancel path and the till AI's
--   completion call this (best-effort, returns nothing).
tillCancelDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillCancelDesignationFn wsc = do
    gxArg ← Lua.tonumber 1
    gyArg ← Lua.tonumber 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mPage ← Lua.liftIO $ activeWorldPageFrom (wsWorldManagerRef wsc)
            case mPage of
                Just (pageId, _) → Lua.liftIO $
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldCancelTill pageId (round gx) (round gy)
                Nothing → pure ()
        _ → pure ()
    return 0

-- | till.getDesignationAt(pageId, gx, gy) → {x, y, z} | nil. Accepts any
--   u-alias of the tile and reports the CANONICAL stored coords (#1175);
--   identity away from the seam.
tillGetDesignationAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillGetDesignationAtFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsTillDesignationsRef ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let (gx, gy) = canonicalTile worldSize (round gxN) (round gyN)
                    case HM.lookup (gx, gy) m of
                        Just td → do
                            Lua.newtable
                            Lua.pushinteger (fromIntegral gx)
                            Lua.setfield (Lua.nth 2) "x"
                            Lua.pushinteger (fromIntegral gy)
                            Lua.setfield (Lua.nth 2) "y"
                            Lua.pushinteger (fromIntegral (tlZ td))
                            Lua.setfield (Lua.nth 2) "z"
                            return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | till.getDesignationCount(pageId) → n.
tillGetDesignationCountFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillGetDesignationCountFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsTillDesignationsRef ws)
                    Lua.pushinteger (fromIntegral (HM.size m))
                    return 1
                Nothing → Lua.pushinteger 0 >> return 1
        _ → Lua.pushinteger 0 >> return 1

-- | till.nearestDesignation(pageId, x, y) → gx, gy, dist | nil.
--   Nearest designated tile by Euclidean distance — the till AI's
--   "distance to nearest till job" term. Mirrors chop.nearestDesignation,
--   including its seam-aware compare and canonical result (#1175).
tillNearestDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillNearestDesignationFn wsc = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                ux = realToFrac x ∷ Float
                uy = realToFrac y ∷ Float
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsTillDesignationsRef ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let dist2 = seamTileDist2 worldSize (ux, uy)
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

-- | till.setDesignateTexture(pageId, texHandle) — marker texture for
--   committed till designations.
tillSetDesignateTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
tillSetDesignateTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    handleArg ← Lua.tointeger 2
    case (pageIdArg, handleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetTillDesignateTexture pageId texHandle
        _ → pure ()
    return 0

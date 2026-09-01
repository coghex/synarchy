{-# LANGUAGE Strict #-}
-- | Lua API for the chop-designation tool (issue #97) — the @chop.*@
--   namespace. Mirrors the construction-designation API (#95): the tool
--   drives setAnchor / clearAnchor / designate, the chop AI
--   (scripts/unit_ai_chop.lua) drives nearestDesignation /
--   getDesignationForInstance / cancelDesignation (claims are Lua-side
--   like dig jobs, so there is no engine-side job status), and the HUD
--   sets the marker texture.
--
--   #1854: a designation names one PLANT, not one tile — two wood-tagged
--   trees can share a tile. Every verb here that identifies a
--   designation therefore reports the plant's stable
--   'World.Flora.Identity.FloraInstanceId' alongside its coords, and the
--   AI addresses its claimed tree by that id. The tile-shaped verbs stay
--   for the PLAYER's tile-granularity gestures (a cancel click clears
--   what the player pointed at) and for the "is there still work here?"
--   question.
module Engine.Scripting.Lua.API.Chop
    ( chopSetAnchorFn
    , chopClearAnchorFn
    , chopDesignateFn
    , chopCancelDesignationFn
    , chopGetDesignationAtFn
    , chopGetDesignationForInstanceFn
    , chopGetDesignationCountFn
    , chopNearestDesignationFn
    , chopSetDesignateTextureFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Data.List (minimumBy)
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.State (activeWorldPageFrom)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldManager(..), WorldState(..), pageWrapWorldSize)
import World.Page.Types (WorldPageId(..))
import World.Command.Types (WorldCommand(..))
import World.Flora.Identity
    (FloraInstanceId(..), unFloraInstanceId)
import World.Generate.Coordinates (canonicalTile, seamTileDist2)
import World.Chop.Types

-- | chop.setAnchor(pageId, gx, gy) — first-click anchor.
chopSetAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopSetAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetChopAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | chop.clearAnchor(pageId) — cancel the pending rectangle.
chopClearAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopClearAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $ WorldClearChopAnchor pageId
        _ → pure ()
    return 0

-- | chop.designate(pageId, x1, y1, x2, y2 [, tag]) — commit the
--   rectangle. Only tiles holding a currently-harvestable flora species
--   carrying @tag@ (default "wood") are designated — sweeping a forest
--   marks the trees, not the ground between them.
chopDesignateFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopDesignateFn wsc = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    tagArg ← Lua.tostring 6
    case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                tag = maybe "wood" TE.decodeUtf8Lenient tagArg
            Q.writeQueue (wsWorldQueue wsc) $
                WorldDesignateChop pageId
                    (round x1) (round y1) (round x2) (round y2) tag
        _ → pure ()
    return 0

-- | chop.cancelDesignation(gx, gy [, instanceId]) — remove a
--   designation on the active world (best-effort, returns nothing).
--
--   #1854: WITH an instance id exactly that plant's designation goes —
--   the chop AI's completion passes the id it claimed, so felling one
--   of two designated trees on a tile leaves the other designated.
--   WITHOUT one this is the player's tile-granularity cancel and clears
--   every designation standing there.
chopCancelDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopCancelDesignationFn wsc = do
    gxArg ← Lua.tonumber 1
    gyArg ← Lua.tonumber 2
    iidArg ← Lua.tointeger 3
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mPage ← Lua.liftIO $ activeWorldPageFrom (wsWorldManagerRef wsc)
            case mPage of
                Just (pageId, _) → Lua.liftIO $
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldCancelChop pageId (round gx) (round gy)
                            (toInstanceId <$> iidArg)
                Nothing → pure ()
        _ → pure ()
    return 0

-- | A Lua integer back to a 'FloraInstanceId'. The whole id space fits
--   in a positive Int64 by construction ("World.Flora.Identity"), so
--   this is lossless in both directions.
toInstanceId ∷ Lua.Integer → FloraInstanceId
toInstanceId = FloraInstanceId . fromIntegral

pushInstanceId ∷ FloraInstanceId → Lua.LuaE Lua.Exception ()
pushInstanceId = Lua.pushinteger . fromIntegral . unFloraInstanceId

-- | chop.getDesignationAt(pageId, gx, gy) → {x, y, z, instanceId} | nil.
--   Accepts any u-alias of the tile and reports the CANONICAL stored
--   coords (#1175); identity away from the seam.
--
--   #1854: designations are per-PLANT, so a tile can carry several. This
--   answers with the LOWEST-id one, which is deterministic (never
--   hashmap order) and is what makes "is there still work on this tile?"
--   a stable question. A caller that holds a specific plant's id — the
--   chop AI, once it has claimed a tree — must ask
--   'chopGetDesignationForInstanceFn' instead, or it will follow another
--   acolyte's tree.
chopGetDesignationAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopGetDesignationAtFn wsc = do
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
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let tile = canonicalTile worldSize (round gxN) (round gyN)
                        here = [ (iid, cd) | (iid, cd) ← HM.toList m
                                           , chopDesignationTile cd ≡ tile ]
                    case listToMaybe (L.sortOn fst here) of
                        Just (iid, cd) → pushDesignation iid cd >> return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | chop.getDesignationForInstance(pageId, instanceId)
--   → {x, y, z, instanceId} | nil.
--
--   The EXACT question (#1854): is THIS plant still designated? The chop
--   AI's lock-in and its mid-swing cancellation check both ask it, so a
--   felling acolyte notices its own tree being cancelled and is not kept
--   working by an unrelated designation that happens to share the tile.
chopGetDesignationForInstanceFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopGetDesignationForInstanceFn wsc = do
    pageIdArg ← Lua.tostring 1
    iidArg ← Lua.tointeger 2
    case (pageIdArg, iidArg) of
        (Just pageIdBS, Just iidN) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                iid = toInstanceId iidN
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
                    case HM.lookup iid m of
                        Just cd → pushDesignation iid cd >> return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

pushDesignation
    ∷ FloraInstanceId → ChopDesignation → Lua.LuaE Lua.Exception ()
pushDesignation iid cd = do
    Lua.newtable
    Lua.pushinteger (fromIntegral (chGX cd))
    Lua.setfield (Lua.nth 2) "x"
    Lua.pushinteger (fromIntegral (chGY cd))
    Lua.setfield (Lua.nth 2) "y"
    Lua.pushinteger (fromIntegral (chZ cd))
    Lua.setfield (Lua.nth 2) "z"
    pushInstanceId iid
    Lua.setfield (Lua.nth 2) "instanceId"

-- | chop.getDesignationCount(pageId) → n.
chopGetDesignationCountFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopGetDesignationCountFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
                    Lua.pushinteger (fromIntegral (HM.size m))
                    return 1
                Nothing → Lua.pushinteger 0 >> return 1
        _ → Lua.pushinteger 0 >> return 1

-- | chop.nearestDesignation(pageId, x, y) → gx, gy, dist, instanceId | nil.
--   Nearest designated tree by Euclidean distance — the chop AI's
--   "distance to nearest chop job" term. Mirrors nearestMineDesignation,
--   including its seam-aware compare and canonical result (#1175).
--
--   #1854: the fourth return is the winning PLANT's stable id, appended
--   so existing three-value callers are unaffected. Ties break on that
--   id, which is deterministic — two equidistant trees on one tile used
--   to be indistinguishable, and picking by hashmap order would have
--   made the AI's choice vary run to run.
chopNearestDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopNearestDesignationFn wsc = do
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
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let dist2 = seamTileDist2 worldSize (ux, uy)
                        scored = [ (dist2 (chopDesignationTile cd), iid, cd)
                                 | (iid, cd) ← HM.toList m ]
                    case scored of
                        [] → Lua.pushnil >> return 1
                        _  → do
                            let (d2, iid, cd) = minimumBy
                                    (comparing (\(d, i, _) → (d, i))) scored
                            Lua.pushinteger (fromIntegral (chGX cd))
                            Lua.pushinteger (fromIntegral (chGY cd))
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            pushInstanceId iid
                            return 4
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | chop.setDesignateTexture(pageId, texHandle) — marker texture for
--   committed chop designations.
chopSetDesignateTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopSetDesignateTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    handleArg ← Lua.tointeger 2
    case (pageIdArg, handleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetChopDesignateTexture pageId texHandle
        _ → pure ()
    return 0

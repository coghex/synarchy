{-# LANGUAGE Strict #-}
-- | Lua API for the chop-designation tool (issue #97) — the @chop.*@
--   namespace.
--
--   #1854: a designation names one PLANT, not one tile — two wood-tagged
--   trees can share a tile. Every verb here that identifies a
--   designation therefore reports the plant's stable
--   'World.Flora.Identity.FloraInstanceId' alongside its coords, and the
--   AI addresses its claimed tree by that id. The tile-shaped READ verbs
--   stay for the \"is there still work here?\" question and for a
--   restored job that knows its tile but not its plant.
--
--   #1856 replaced the PLAYER's two-click tile rectangle with a
--   screen-space press-drag:
--
--     * @designateAt@ \/ @designateInRect@ add, @eraseAt@ \/
--       @eraseInRect@ erase, symmetric through the one selection
--       oracle ("World.Flora.HitTest");
--     * @designateInstances@ \/ @eraseInstances@ are the exact-identity
--       authority beneath them, for callers that already hold ids;
--     * @setAnchor@, @clearAnchor@ and the tile-rectangle @designate@
--       are GONE — the gesture has no world-side anchor and no tile
--       rectangle crosses the queue.
--
--   The chop AI still drives nearestDesignation \/
--   getDesignationForInstance \/ cancelDesignation (claims are Lua-side
--   like dig jobs, so there is no engine-side job status), and the HUD
--   sets the marker texture.
module Engine.Scripting.Lua.API.Chop
    ( chopDesignateAtFn
    , chopDesignateInRectFn
    , chopEraseAtFn
    , chopEraseInRectFn
    , chopDesignateInstancesFn
    , chopEraseInstancesFn
    , chopCancelDesignationFn
    , chopGetDesignationAtFn
    , chopGetDesignationsAtFn
    , chopGetDesignationForInstanceFn
    , chopGetDesignationCountFn
    , chopNearestDesignationFn
    , chopSetDesignateTextureFn
    ) where

import UPrelude
import Data.ByteString (ByteString)
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
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv, activeWorldPageFrom)
import World.Flora.HitTest
    (FloraHitView, FloraPick(..), FloraSelectMode(..)
    , floraHitView, pickFloraAt, pickFloraInRect)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldManager(..), WorldState(..), pageWrapWorldSize)
import World.Page.Types (WorldPageId(..))
import World.Command.Types (WorldCommand(..))
import World.Flora.Identity
    (FloraInstanceId, floraInstanceIdToLua, floraInstanceIdFromLua)
import World.Generate.Coordinates (canonicalTile, seamTileDist2)
import World.Chop.Types

-- * The gesture surface (#1856)
--
-- Chop is a screen-space press-drag, so every player-facing verb here
-- takes WINDOW pixels and resolves them through the shared selection
-- oracle ("World.Flora.HitTest"), which derives its geometry from the
-- values the renderer draws with. There is no tile rectangle and no
-- world-side anchor left: what crosses the queue is an exact list of
-- plant identities, and the world thread re-checks eligibility against
-- live state before writing anything.
--
-- Each verb answers with the number of plants it SELECTED (not the
-- number the world thread went on to accept, which it cannot know
-- synchronously) so a Lua caller can tell an empty gesture from a
-- productive one.

-- | chop.designateAt(pageId, pixX, pixY [, tag]) → n
--
--   Click-add: the topmost eligible tree whose rendered sprite contains
--   the pointer.
chopDesignateAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopDesignateAtFn env = gesturePoint env (SelectChoppable . fromMaybe "wood")
    (\wsc pageId tag picks →
        Q.writeQueue (wsWorldQueue wsc) $
            WorldDesignateChopInstances pageId (map fpInstanceId picks) tag)

-- | chop.designateInRect(pageId, x1, y1, x2, y2 [, tag]) → n
--
--   Drag-add: every eligible tree whose rendered ground-contact anchor
--   lies inside the drawn box. Either drag direction, closed bounds.
chopDesignateInRectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopDesignateInRectFn env = gestureRect env (SelectChoppable . fromMaybe "wood")
    (\wsc pageId tag picks →
        Q.writeQueue (wsWorldQueue wsc) $
            WorldDesignateChopInstances pageId (map fpInstanceId picks) tag)

-- | chop.eraseAt(pageId, pixX, pixY) → n
--
--   Click-erase, the exact mirror of 'chopDesignateAtFn'. Candidates
--   are the trees currently DESIGNATED, so a designation whose tree has
--   stopped being add-eligible stays clearable (D-12).
chopEraseAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopEraseAtFn env = gesturePoint env (const SelectDesignated)
    (\wsc pageId _tag picks →
        Q.writeQueue (wsWorldQueue wsc) $
            WorldEraseChopInstances pageId (map fpInstanceId picks))

-- | chop.eraseInRect(pageId, x1, y1, x2, y2) → n — drag-erase.
chopEraseInRectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
chopEraseInRectFn env = gestureRect env (const SelectDesignated)
    (\wsc pageId _tag picks →
        Q.writeQueue (wsWorldQueue wsc) $
            WorldEraseChopInstances pageId (map fpInstanceId picks))

-- | chop.designateInstances(pageId, {instanceId, ...} [, tag]) → n
--
--   The exact-identity authority underneath the gesture verbs, exposed
--   directly for callers that already hold ids — headless probes and
--   specs, which have no camera to project through. It applies the SAME
--   world-side eligibility re-check; @n@ is the number of well-formed
--   ids submitted.
chopDesignateInstancesFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopDesignateInstancesFn wsc = do
    pageIdArg ← Lua.tostring 1
    iids ← readInstanceIdArray 2
    tagArg ← Lua.tostring 3
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                tag = maybe "wood" TE.decodeUtf8Lenient tagArg
            Lua.liftIO $ Q.writeQueue (wsWorldQueue wsc) $
                WorldDesignateChopInstances pageId iids tag
            Lua.pushinteger (fromIntegral (length iids)) >> return 1
        _ → Lua.pushinteger 0 >> return 1

-- | chop.eraseInstances(pageId, {instanceId, ...}) → n — the erase half.
chopEraseInstancesFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopEraseInstancesFn wsc = do
    pageIdArg ← Lua.tostring 1
    iids ← readInstanceIdArray 2
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Lua.liftIO $ Q.writeQueue (wsWorldQueue wsc) $
                WorldEraseChopInstances pageId iids
            Lua.pushinteger (fromIntegral (length iids)) >> return 1
        _ → Lua.pushinteger 0 >> return 1

-- | Shared body of the two point gestures.
gesturePoint
    ∷ EngineEnv
    → (Maybe Text → FloraSelectMode)
    → (WorldSimCapability → WorldPageId → Text → [FloraPick] → IO ())
    → Lua.LuaE Lua.Exception Lua.NumResults
gesturePoint env mkMode commit = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    tagArg ← Lua.tostring 4
    withGesturePage env pageIdArg tagArg $ \view pageId tag → do
        let picks = maybeToList
                (pickFloraAt view (mkMode (Just tag)) (num xArg) (num yArg))
        commit (toWorldSimCapability env) pageId tag picks
        pure (length picks)

-- | Shared body of the two box gestures.
gestureRect
    ∷ EngineEnv
    → (Maybe Text → FloraSelectMode)
    → (WorldSimCapability → WorldPageId → Text → [FloraPick] → IO ())
    → Lua.LuaE Lua.Exception Lua.NumResults
gestureRect env mkMode commit = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    tagArg ← Lua.tostring 6
    withGesturePage env pageIdArg tagArg $ \view pageId tag → do
        let picks = pickFloraInRect view (mkMode (Just tag))
                        (num x1Arg) (num y1Arg) (num x2Arg) (num y2Arg)
        commit (toWorldSimCapability env) pageId tag picks
        pure (length picks)

-- | Resolve the named page, snapshot the oracle's view of it, and push
--   the selected count. A page that does not exist selects nothing.
withGesturePage
    ∷ EngineEnv
    → Maybe ByteString
    → Maybe ByteString
    → (FloraHitView → WorldPageId → Text → IO Int)
    → Lua.LuaE Lua.Exception Lua.NumResults
withGesturePage env pageIdArg tagArg act = case pageIdArg of
    Nothing → Lua.pushinteger 0 >> return 1
    Just pageIdBS → do
        let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            tag = maybe "wood" TE.decodeUtf8Lenient tagArg
        mgr ← Lua.liftIO $
            readIORef (wsWorldManagerRef (toWorldSimCapability env))
        case lookup pageId (wmWorlds mgr) of
            Nothing → Lua.pushinteger 0 >> return 1
            Just ws → do
                n ← Lua.liftIO $ do
                    view ← floraHitView env ws
                    act view pageId tag
                Lua.pushinteger (fromIntegral n) >> return 1

num ∷ Maybe Lua.Number → Float
num = maybe 0 realToFrac

-- | Read a Lua array of instance ids, dropping any entry that is not a
--   well-formed id. 'floraInstanceIdFromLua' refuses a number in
--   neither namespace, so a bad element names no plant rather than
--   silently matching one.
readInstanceIdArray ∷ Lua.StackIndex → Lua.LuaE Lua.Exception [FloraInstanceId]
readInstanceIdArray idx = do
    isTable ← Lua.istable idx
    if not isTable then pure [] else do
        n ← Lua.rawlen idx
        catMaybes ⊚ forM [1 .. n] (\i → do
            _ ← Lua.rawgeti idx (fromIntegral i)
            v ← Lua.tointeger (-1)
            Lua.pop 1
            pure (toInstanceId =≪ v))

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
                            (toInstanceId =≪ iidArg)
                Nothing → pure ()
        _ → pure ()
    return 0

-- | A Lua integer back to a 'FloraInstanceId'. Partial by design: a
--   number in neither namespace names no plant that could exist, and
--   "World.Flora.Identity" refuses to mint one rather than hand back an
--   id that silently matches nothing.
toInstanceId ∷ Lua.Integer → Maybe FloraInstanceId
toInstanceId = floraInstanceIdFromLua . fromIntegral

pushInstanceId ∷ FloraInstanceId → Lua.LuaE Lua.Exception ()
pushInstanceId = Lua.pushinteger . fromIntegral . floraInstanceIdToLua

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

-- | chop.getDesignationsAt(pageId, gx, gy)
--   → array of {x, y, z, instanceId} | nil
--
--   EVERY designation standing on the tile (#1854), ascending by
--   instance id — the deterministic order 'chopGetDesignationAtFn'
--   takes its single answer from. nil when the tile carries none.
--
--   The chop AI needs the whole list, not just the first: a job
--   restored from a save knows its TILE but not which of that tile's
--   plants it had claimed (the id is deliberately not persisted), so it
--   walks these and adopts the first one no OTHER acolyte is holding.
--   Answering only the lowest id would make two units restoring jobs on
--   one tile both take the same tree and leave its co-tenant's
--   designation orphaned.
chopGetDesignationsAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
chopGetDesignationsAtFn wsc = do
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
                        here = L.sortOn fst
                            [ (iid, cd) | (iid, cd) ← HM.toList m
                                        , chopDesignationTile cd ≡ tile ]
                    case here of
                        [] → Lua.pushnil >> return 1
                        _  → do
                            Lua.newtable
                            forM_ (zip [1 ∷ Int ..] here) $ \(i, (iid, cd)) → do
                                pushDesignation iid cd
                                Lua.rawseti (-2) (fromIntegral i)
                            return 1
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
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case (toInstanceId iidN, lookup pageId (wmWorlds mgr)) of
                (Just iid, Just ws) → do
                    m ← Lua.liftIO $ readIORef (wsChopDesignationsRef ws)
                    case HM.lookup iid m of
                        Just cd → pushDesignation iid cd >> return 1
                        Nothing → Lua.pushnil >> return 1
                _ → Lua.pushnil >> return 1
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

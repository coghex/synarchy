{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World.Designation
    ( worldSetMineAnchorFn
    , worldClearMineAnchorFn
    , worldDesignateMineFn
    , worldSetMineDesignateTextureFn
    , worldGetMineDesignationCountFn
    , worldNearestMineDesignationFn
    , worldNearestWorkableMineDesignationFn
    , worldGetMineDesignationAtFn
    ) where

import UPrelude
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.API.FreeDesignation
    (maxDistBound, readExcludedTiles)
import World.Types
import World.Generate.Coordinates (canonicalTile, seamTileDist2)
import World.Mine.DigInfo
    (digMaterialAt, firstWorkableDesignation, spoilBlockedFor)
import World.Mine.Types (MineDesignation(..))

-- * Mine designation tool

-- | world.setMineAnchor(pageId, gx, gy) — anchor the designation
--   rectangle at the given tile (mine tool first click).
worldSetMineAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMineAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetMineAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | world.clearMineAnchor(pageId) — cancel the pending rectangle.
worldClearMineAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldClearMineAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $ WorldClearMineAnchor pageId
        _ → pure ()
    return 0

-- | world.designateMine(pageId, x1, y1, x2, y2) — commit the
--   rectangle (corners in either order; mine tool second click).
worldDesignateMineFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldDesignateMineFn wsc = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldDesignateMine pageId (round x1) (round y1)
                                          (round x2) (round y2)
        _ → pure ()
    return 0

-- | world.setMineDesignateTexture(pageId, texHandle) — marker texture
--   for committed designations.
worldSetMineDesignateTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetMineDesignateTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    textureHandleArg ← Lua.tointeger 2
    case (pageIdArg, textureHandleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetMineDesignateTexture pageId texHandle
        _ → pure ()
    return 0

-- | world.nearestMineDesignation(pageId, x, y) → gx, gy, dist | nil
--   Nearest designated tile to (x, y) by Euclidean distance — the
--   "distance to the nearest dig job" term in the dig utility. Linear
--   scan of the designation map (synchronous read).
--
--   #1175: the compare is seam-aware (each key measured through its
--   nearest u-alias), and the coords returned are the CANONICAL stored
--   key — which the AI can hand straight back to any point verb. Both
--   are the plain Euclidean/identity case away from the seam.
worldNearestMineDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldNearestMineDesignationFn wsc = do
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
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
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
                        Nothing → do
                            Lua.pushnil
                            return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.nearestWorkableMineDesignation(pageId, x, y
--       [, maxDist [, excluded [, tools]]])
--     → gx, gy, dist, tool, speed | nil
--
--   The nearest designated tile this worker can ACTUALLY DIG (#2538) —
--   'worldNearestMineDesignationFn' answers "the nearest designation",
--   full stop, and a miner that scored only that reported no mining
--   work at all whenever the winner happened to be unusable, leaving
--   every farther workable designation untouched.
--
--   Eligibility is the existing rejection set, unchanged and applied
--   per candidate rather than once to the geometric winner:
--
--     * @excluded@ — the flat @{x1, y1, x2, y2, ...}@ array of tiles a
--       live OTHER worker holds a claim on, built by
--       @scripts/unit_ai_claims.lua@'s @claimedTiles@. Claims are
--       Lua-side, so the engine can only know them by being told.
--     * @maxDist@ — the caller's own scan range, inclusive, in tiles.
--     * Resident, in-range dig information: an unloaded chunk answers
--       nothing and is not workable, and selection NEVER queues a load
--       ('digMaterialAt' reads only what is already there).
--     * Spoil disposal: a tile whose spoil has nowhere to go would have
--       the dig command refuse every tick.
--     * @tools@ — @{ pick = true, shovel = true }@, the tool CLASSES
--       the worker is carrying. The chosen tool is the shovel unless a
--       carried pick is strictly faster on this material, which is the
--       rule @scripts/unit_ai_dig.lua@ applied before #2538 and now
--       applies only here, so there is exactly one of it.
--
--   A missing or non-table @excluded@\/@tools@ restricts nothing, and an
--   omitted @maxDist@ is unbounded — the same "a caller that cannot say
--   gets the unrestricted query" convention the sibling verbs use.
--
--   COST is bounded by construction, which matters because this runs
--   in every idle miner's thought tick. Candidates are ordered by
--   'seamTileDist2' ONCE and walked ascending, and the walk stops at
--   the first workable one. Range and the claim exclusion are settled
--   before the list is even built; 'World.Mine.DigInfo' then splits the
--   per-candidate work so 'firstWorkableDesignation' pays the cheap
--   resident read first and reaches the expensive spoil-capacity sweep
--   ONLY for a candidate the carried tools have already admitted. A
--   worker carrying neither class reads nothing at all.
--
--   Ties are broken on ascending canonical @(x, y)@, NOT hash order:
--   two equidistant workable designations must nominate the same one on
--   every run, or a clustered-designation regression is unreproducible.
--   That is 'nearestFreeDesignationOn' own tie-break, this being the
--   same tile-keyed identity.
--
--   Reported coordinates are the designation's stored CANONICAL key and
--   @dist@ is its seam-aware distance, so the tile the caller scores,
--   claims and walks to is one identity measured one way (#1175).
worldNearestWorkableMineDesignationFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldNearestWorkableMineDesignationFn wsc = do
    pageIdArg ← Lua.tostring 1
    xArg      ← Lua.tonumber 2
    yArg      ← Lua.tonumber 3
    maxArg    ← Lua.tonumber 4
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                ux = realToFrac x ∷ Float
                uy = realToFrac y ∷ Float
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    excluded  ← readExcludedTiles worldSize 5
                    tools     ← readToolset 6
                    desigs    ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    tileData  ← Lua.liftIO $ readIORef (wsTilesRef ws)
                    piles     ← Lua.liftIO $ readIORef (wsSpoilRef ws)
                    registry  ← Lua.liftIO $ readIORef
                                    (wsMaterialRegistryRef wsc)
                    let dist2 = seamTileDist2 worldSize (ux, uy)
                        maxD2 = maxDistBound maxArg
                        materialOf = digMaterialAt registry tileData desigs
                                                   worldSize
                        blocked = spoilBlockedFor registry tileData desigs
                                                  piles
                        candidates = sort
                            [ (d2, k)
                            | k ← HM.keys desigs
                            , not (HS.member k excluded)
                            , let d2 = dist2 k
                            , not (maybe False (d2 >) maxD2) ]
                    case firstWorkableDesignation tools materialOf blocked
                                                  candidates of
                        Nothing → Lua.pushnil >> return 1
                        Just ((gx, gy), d2, tool, speed) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            Lua.pushstring (TE.encodeUtf8 tool)
                            Lua.pushnumber (Lua.Number (realToFrac speed))
                            return 5
        _ → Lua.pushnil >> return 1

-- | Read the @{ pick = true, shovel = true }@ toolset table at @idx@.
--   A missing or non-table argument is no restriction — both classes
--   allowed — matching the convention 'readExcludedTiles' itself uses: a
--   caller which cannot say gets the unrestricted query rather than an
--   error mid-thought-tick.
readToolset ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Bool, Bool)
readToolset idx = do
    isTable ← Lua.istable idx
    if not isTable then pure (True, True) else do
        hasPick   ← flagField "pick"
        hasShovel ← flagField "shovel"
        pure (hasPick, hasShovel)
  where
    flagField name = do
        _ ← Lua.getfield idx name
        v ← Lua.toboolean (-1)
        Lua.pop 1
        pure v

-- | world.getMineDesignationAt(pageId, gx, gy)
--     → z, cNW, cNE, cSE, cSW | nil
--   Designation state at a tile, including corner dig progress (the
--   AI's "how far along is this tile" query). Accepts any u-alias of
--   the tile (#1175); identity away from the seam.
worldGetMineDesignationAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetMineDesignationAtFn wsc = do
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
                    m ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    case HM.lookup (canonicalTile worldSize (round gxN)
                                                            (round gyN)) m of
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
worldGetMineDesignationCountFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetMineDesignationCountFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
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

{-# LANGUAGE Strict #-}
-- | The claim-aware nearest-designation queries — the selector every
--   designation-driven action scores with. Two shapes live here:
--   'nearestFreeDesignationOn' for the TILE-keyed farming maps
--   (@till.nearestFreeDesignation@ \/ @plant.nearestFreeDesignation@,
--   issue #2534) and 'nearestFreeInstanceDesignationOn' for chop's
--   INSTANCE-keyed one (@chop.nearestFreeDesignation@, issue #2536).
--
--   @<ns>.nearestDesignation@ answers "the nearest designation", full
--   stop. Every one of these AIs needs "the nearest designation THIS
--   worker may claim": claims are Lua-side (see
--   @scripts/unit_ai_claims.lua@), so a designation another live worker
--   already holds is invisible to the engine, and a worker that scored
--   only the unconditional nearest reported no work at all whenever a
--   colleague held it — leaving farther free designations untouched.
--
--   The exclusion travels IN rather than the designation set travelling
--   OUT. Every action's @utility@ runs for every unit on every thought
--   tick and one drag can designate 128x128 tiles, so handing Lua the
--   page's whole designation set per scoring call would be a new
--   per-tick marshalling cost; the excluded set is bounded by the live
--   claim registry instead (at most one entry per working unit), and
--   the result stays the single winning designation the existing verbs
--   return.
--
--   Three properties the callers depend on, all inherited from #1175
--   and all enforced here rather than in Lua:
--
--     * Distances are compared by 'seamTileDist2' and results report
--       the designation's own CANONICAL tile, so a designation across
--       the wrap seam is ranked by physical distance and named by the
--       one key its claim is filed under.
--     * An exclusion is spelled in the SAME identity the map is keyed
--       by, and normalised the same way: a tile-keyed query
--       canonicalises the tiles handed to it, so a seam alias lands on
--       the very key the fold is testing and one physical designation
--       occupies exactly one claim slot; an instance-keyed one needs no
--       normalisation, a 'FloraInstanceId' having exactly one spelling.
--     * Every query is page-scoped by @pageId@, so a claim recorded on
--       another page can never exclude a designation here.
--
--   Ties are broken deterministically, NOT by hash order: two
--   equidistant free designations must pick the same one on every run,
--   or a regression over clustered designations is unreproducible. The
--   tie-break is each query's own identity — ascending canonical
--   @(x, y)@ for the tile-keyed maps, ascending instance id for the
--   instance-keyed one, which is what @chop.nearestDesignation@ has
--   always used (#1854) and therefore what preserving chop's existing
--   selection order means.
--
--   The two bodies are kept SEPARATE rather than folded into one
--   generic scan: they disagree on what a map entry is keyed by, what
--   identity the exclusion set names, what the tie-break compares and
--   how many values come back. What they genuinely share — the
--   inclusive distance bound's sentinel form — is 'maxDistBound'.
module Engine.Scripting.Lua.API.FreeDesignation
    ( nearestFreeDesignationOn
    , nearestFreeInstanceDesignationOn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified HsLua as Lua
import Data.IORef (IORef, readIORef)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import World.Types (WorldManager(..), WorldState, pageWrapWorldSize)
import World.Page.Types (WorldPageId(..))
import World.Generate.Coordinates (canonicalTile, seamTileDist2)
import World.Flora.Identity
    (FloraInstanceId, floraInstanceIdToLua, floraInstanceIdFromLua)

-- | @<ns>.nearestFreeDesignation(pageId, x, y [, maxDist [, excluded]])@
--   → @gx, gy, dist@ | @nil@.
--
--   @refOf@ selects the page's designation map; only its KEYS are read,
--   so till's and plant's differing record shapes never meet here.
--
--   @maxDist@ bounds the search to the caller's own scan range
--   (inclusive, in tiles); omitted or non-numeric means unbounded, and a
--   negative bound admits nothing. @excluded@ is the flat
--   @{x1, y1, x2, y2, ...}@ array of tiles the caller may not take —
--   @scripts/unit_ai_claims.lua@'s @claimedTiles@ builds it from the
--   live claim registry. A missing or non-table argument excludes
--   nothing, which is exactly the unconditional nearest query.
nearestFreeDesignationOn
    ∷ WorldSimCapability
    → (WorldState → IORef (HM.HashMap (Int, Int) α))
    → Lua.LuaE Lua.Exception Lua.NumResults
nearestFreeDesignationOn wsc refOf = do
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
                    m         ← Lua.liftIO $ readIORef (refOf ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    excluded  ← readExcludedTiles worldSize 5
                    let dist2 = seamTileDist2 worldSize (ux, uy)
                        maxD2 = maxDistBound maxArg
                        best = foldl' (pick dist2 excluded maxD2)
                                      Nothing (HM.keys m)
                    case best of
                        Just ((gx, gy), d2) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            return 3
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | One fold step: reject the excluded and the out-of-range, then keep
--   the strictly nearer candidate — or, at an exact tie, the one earlier
--   in canonical @(x, y)@ order.
pick ∷ ((Int, Int) → Float) → HS.HashSet (Int, Int) → Maybe Float
     → Maybe ((Int, Int), Float) → (Int, Int)
     → Maybe ((Int, Int), Float)
pick dist2 excluded maxD2 acc k
    | HS.member k excluded              = acc
    | maybe False (d >) maxD2           = acc
    | otherwise = case acc of
        Nothing → Just (k, d)
        Just (bk, bd)
            | d < bd            → Just (k, d)
            | d ≡ bd && k < bk  → Just (k, d)
            | otherwise         → acc
  where d = dist2 k

-- | Read the flat @{x1, y1, x2, y2, ...}@ exclusion array at @idx@ into
--   canonical tile keys. A trailing unpaired value, a non-numeric entry
--   and a non-table argument are all simply no exclusion: a caller that
--   cannot say which tiles are taken must get the unconditional nearest
--   rather than an error mid-thought-tick.
readExcludedTiles ∷ Int → Lua.StackIndex
                  → Lua.LuaE Lua.Exception (HS.HashSet (Int, Int))
readExcludedTiles worldSize idx = do
    isTable ← Lua.istable idx
    if not isTable then pure HS.empty else do
        n ← Lua.rawlen idx
        let readAt i = do
                _ ← Lua.rawgeti idx (fromIntegral i)
                v ← Lua.tonumber (-1)
                Lua.pop 1
                pure v
            go i acc
                | i + 1 > n = pure acc
                | otherwise = do
                    mx ← readAt i
                    my ← readAt (i + 1)
                    case (mx, my) of
                        (Just xv, Just yv) → go (i + 2) $ HS.insert
                            (canonicalTile worldSize (round xv) (round yv)) acc
                        _ → go (i + 2) acc
        go 1 HS.empty

-- | @chop.nearestFreeDesignation(pageId, x, y [, maxDist [, excluded]])@
--   → @gx, gy, dist, instanceId@ | @nil@.
--
--   The instance-keyed sibling of 'nearestFreeDesignationOn' (#2536).
--   A chop designation names one PLANT rather than one tile (#1854),
--   because two wood-tagged trees can share a tile — so @refOf@ selects
--   a map keyed by 'FloraInstanceId', @tileOf@ reads each designation's
--   own canonical tile out of the value, and the exclusion set names
--   INSTANCES. That is what keeps requirement-level exclusivity exact:
--   claiming one tree must not hide its co-tenant, which a tile-shaped
--   exclusion could not express.
--
--   @maxDist@ and @excluded@ carry the same contract as the tile query:
--   an inclusive bound in tiles (omitted or non-numeric is unbounded, a
--   negative bound admits nothing), and a flat
--   @{iid1, iid2, ...}@ array built by @scripts/unit_ai_claims.lua@'s
--   @claimedInstances@ from the live claim registry. A missing or
--   non-table argument excludes nothing, which is exactly the
--   unconditional nearest query.
--
--   Empty result is @nil@, matching @chop.nearestDesignation@ — NOT the
--   empty table @chop.getDesignationsAt@'s callers write @or {}@ for.
--   Distances use 'seamTileDist2' and the reported coordinates are the
--   designation's stored canonical ones, so a tree across the wrap seam
--   is ranked by physical distance and named by the one key its claim
--   is filed under.
nearestFreeInstanceDesignationOn
    ∷ WorldSimCapability
    → (WorldState → IORef (HM.HashMap FloraInstanceId α))
    → (α → (Int, Int))
    → Lua.LuaE Lua.Exception Lua.NumResults
nearestFreeInstanceDesignationOn wsc refOf tileOf = do
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
                    m         ← Lua.liftIO $ readIORef (refOf ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    excluded  ← readExcludedInstances 5
                    let dist2 = seamTileDist2 worldSize (ux, uy)
                        maxD2 = maxDistBound maxArg
                        best  = foldl' (pickInstance dist2 tileOf excluded maxD2)
                                       Nothing (HM.toList m)
                    case best of
                        Just (iid, (gx, gy), d2) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            Lua.pushinteger
                                (fromIntegral (floraInstanceIdToLua iid))
                            return 4
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | One fold step over an instance-keyed map: reject the excluded and
--   the out-of-range, then keep the strictly nearer candidate — or, at
--   an exact tie, the lower instance id, which is
--   @chop.nearestDesignation@'s own tie-break.
pickInstance
    ∷ ((Int, Int) → Float) → (α → (Int, Int))
    → HS.HashSet FloraInstanceId → Maybe Float
    → Maybe (FloraInstanceId, (Int, Int), Float)
    → (FloraInstanceId, α)
    → Maybe (FloraInstanceId, (Int, Int), Float)
pickInstance dist2 tileOf excluded maxD2 acc (iid, cd)
    | HS.member iid excluded  = acc
    | maybe False (d >) maxD2 = acc
    | otherwise = case acc of
        Nothing → Just (iid, t, d)
        Just (bi, _, bd)
            | d < bd              → Just (iid, t, d)
            | d ≡ bd ∧ iid < bi   → Just (iid, t, d)
            | otherwise           → acc
  where t = tileOf cd
        d = dist2 t

-- | Read the flat @{iid1, iid2, ...}@ exclusion array at @idx@ into
--   instance identities. An entry that is not a number, or is a number
--   'floraInstanceIdFromLua' refuses (it names no plant that could
--   exist), excludes nothing — as does a non-table argument: a caller
--   that cannot say which trees are taken must get the unconditional
--   nearest rather than an error mid-thought-tick.
readExcludedInstances
    ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (HS.HashSet FloraInstanceId)
readExcludedInstances idx = do
    isTable ← Lua.istable idx
    if not isTable then pure HS.empty else do
        n ← Lua.rawlen idx
        let go i acc
                | i > n = pure acc
                | otherwise = do
                    _ ← Lua.rawgeti idx (fromIntegral i)
                    v ← Lua.tointeger (-1)
                    Lua.pop 1
                    go (i + 1) $ case floraInstanceIdFromLua . fromIntegral ⊚ v of
                        Just (Just iid) → HS.insert iid acc
                        _               → acc
        go 1 HS.empty

-- | The caller's inclusive distance bound, squared for comparison
--   against 'seamTileDist2' output. Compared squared, so a NEGATIVE
--   bound must be carried as a sentinel no squared distance can reach
--   rather than squared into a positive one — @-1@ admits nothing,
--   which is what a caller asking for a negative range means. 'Nothing'
--   (the argument omitted, or not a number) is unbounded.
maxDistBound ∷ Maybe Lua.Number → Maybe Float
maxDistBound = fmap (\d → let f = realToFrac d ∷ Float
                          in if f < 0 then -1 else f * f)

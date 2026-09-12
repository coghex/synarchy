{-# LANGUAGE Strict #-}
-- | The claim-aware nearest-designation query shared by the two farming
--   namespaces (issue #2534) — @till.nearestFreeDesignation@ and
--   @plant.nearestFreeDesignation@.
--
--   @<ns>.nearestDesignation@ answers "the nearest designated tile",
--   full stop. The farming AI needs "the nearest designated tile THIS
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
--   the result stays the single winning tile the existing verbs return.
--
--   Two properties the callers depend on, both inherited from #1175 and
--   both enforced here rather than in Lua:
--
--     * Results and exclusions are CANONICAL tile coordinates, compared
--       by 'seamTileDist2'. That is what makes one physical designation
--       occupy exactly one claim slot however the caller spells it: a
--       seam alias handed in as an exclusion canonicalises onto the same
--       key the fold is testing.
--     * The query is page-scoped by @pageId@, so a claim recorded on
--       another page can never exclude a tile here.
--
--   Ties are broken by ascending canonical @(x, y)@, NOT by hash order:
--   two equidistant free designations must pick the same one on every
--   run, or a regression over clustered designations is unreproducible.
module Engine.Scripting.Lua.API.FreeDesignation
    ( nearestFreeDesignationOn
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
                        -- Compared squared, so a negative bound must be
                        -- carried as a sentinel no squared distance can
                        -- reach rather than squared into a positive one.
                        maxD2 = (\d → let f = realToFrac d ∷ Float
                                      in if f < 0 then -1 else f * f) ⊚ maxArg
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

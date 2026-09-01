{-# LANGUAGE Strict #-}
-- | Read-only Forage query surface (#94/#332/#334): report flora
--   state on a tile, search for the nearest harvestable flora, and
--   look up planted crop-plot / food-item data. No world mutation —
--   see Engine.Scripting.Lua.API.Forage.Harvest and .Crop for the
--   verbs that change state.
module Engine.Scripting.Lua.API.Forage.Query
    ( worldGetFloraAtFn
    , worldGetFloraGrowthAtFn
    , worldFindHarvestableFloraFn
    , itemGetFoodFn
    , worldGetCropPlotAtFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.State (EngineEnv, activeWorldStateFrom)
import World.Types
import World.Flora.Growth (FloraGrowth(..), floraGrowth, harvestOpen,
                           growthPhaseTag, activeStageTag,
                           lifePhaseText, annualStageText)
import World.Flora.CropPlot (CropPlot(..), cropPlotElapsedDays,
                             cropPlotInstance)
import World.Generate.Coordinates (globalToChunk, chunkToGlobal,
                                  chunkInSeamRegion, seamTileDist2)
import Item.Types (ItemDef(..), ItemFood(..), lookupItemDef)
import Engine.Scripting.Lua.API.Forage.Lookup
    (canonicalPageTile, floraAt, growthClock)

-- | Push a 'FloraInstanceId' as a Lua integer. The whole id space fits
--   in a positive Int64 by construction ("World.Flora.Identity"), so
--   this round-trips losslessly through Lua, JSON and the debug
--   console.
pushInstanceId ∷ FloraInstanceId → Lua.LuaE Lua.Exception ()
pushInstanceId = Lua.pushinteger . fromIntegral . floraInstanceIdToLua

-- | world.getFloraAt(gx, gy) → {id, instanceId, chopDesignated,
--   harvestable, regrowthRemaining, tags} | nil
--
--   nil when the tile has no flora (or its chunk isn't loaded). When
--   several instances share the tile, an instance a bare harvest would
--   take wins the report, then any harvestable species (a berry bush
--   over the decorative dandelion beside it). @harvestable@ mirrors
--   the query/action contract of a BARE world.harvestFlora: true only
--   for a harvestable SPECIES with no live regrowth timer whose growth
--   state is inside the #332 harvest window (in season, not dead, not
--   a juvenile). @regrowthRemaining@ is the timer in game-seconds (0
--   when none). @tags@ is the species' harvest-tag array (#97 — "wood"
--   marks a choppable tree; empty for non-harvestable flora).
--
--   #1854 added two ADDITIVE fields describing the REPORTED PLANT:
--   @instanceId@, its stable opaque identity, and @chopDesignated@,
--   whether that plant in particular is slated for felling. @id@ still
--   means the SPECIES name — repurposing it would break every existing
--   caller — and the whole report, @regrowthRemaining@ included, is now
--   that one plant's own state rather than its tile's, so a berry bush
--   picked beside an oak no longer reports the oak as depleted.
--
--   Designation flows must NOT read @harvestable@ (it is the
--   forage-facing signal): the chop AI keys its claim on the plant's own
--   @regrowthRemaining@ + @tags@, so a designated tree stays choppable
--   as a sprout or standing dead. Per-instance gated state is
--   world.getFloraGrowthAt.
--
--   Accepts any u-alias of the tile (#1707) and reports the state the
--   CANONICAL coord reports — species, @harvestable@, @regrowthRemaining@
--   and @tags@ alike. Identity inland.
worldGetFloraAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetFloraAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let rawGX = fromIntegral gx'
                rawGY = fromIntegral gy'
            mResult ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        -- #1707: the regrowth-timer map is canonical-keyed
                        -- like every other tile-keyed forage map, so the
                        -- timer must be read under the SAME name 'floraAt'
                        -- resolved the instances under — canonicalising
                        -- only one of the two reports a seam tile's flora
                        -- with the wrong (default) timer. Identity inland.
                        (gx, gy) ← canonicalPageTile ws rawGX rawGY
                        insts ← floraAt (toWorldSimCapability env) ws gx gy
                        harvests ← readIORef (wsFloraHarvestsRef ws)
                        (doy, absDay) ← growthClock ws
                        let open (i, sp) =
                                harvestOpen sp doy (floraGrowth sp absDay i)
                            harvestables =
                                [ p | p@(_, sp) ← insts
                                    , isJust (fsHarvest sp) ]
                            harvestFirst =
                                filter open harvestables
                                <> harvestables <> insts
                        pure $ case harvestFirst of
                            [] → Nothing
                            (p@(i, sp):_) →
                                -- #1854: the timer belongs to the
                                -- REPORTED plant, not to its tile, so a
                                -- berry bush picked beside an oak no
                                -- longer reports the oak as depleted.
                                let timer = HM.lookupDefault 0
                                                (fiInstanceId i) harvests
                                in Just ( fsName sp
                                        , isJust (fsHarvest sp) ∧ timer ≤ 0
                                            ∧ open p
                                        , timer
                                        , maybe [] fhTags (fsHarvest sp)
                                        , fiInstanceId i
                                        , fiChopDesignated i )
            case mResult of
                Nothing → Lua.pushnil
                Just (name, harvestable, timer, tags, iid, designated) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 name)
                    Lua.setfield (-2) "id"
                    Lua.pushboolean harvestable
                    Lua.setfield (-2) "harvestable"
                    Lua.pushnumber (Lua.Number (realToFrac timer))
                    Lua.setfield (-2) "regrowthRemaining"
                    -- #1854, ADDITIVE: the reported plant's stable
                    -- identity and its own chop-designated flag. The
                    -- existing @id@ field keeps meaning the SPECIES
                    -- name — repurposing it would break every caller.
                    pushInstanceId iid
                    Lua.setfield (-2) "instanceId"
                    Lua.pushboolean designated
                    Lua.setfield (-2) "chopDesignated"
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] tags) $ \(i, tg) → do
                        Lua.pushstring (TE.encodeUtf8 tg)
                        Lua.rawseti (-2) (fromIntegral i)
                    Lua.setfield (-2) "tags"
            return 1
        _ → Lua.pushnil >> return 1

-- | world.getFloraGrowthAt(gx, gy) → array of {id, instanceId, age,
--   health, phase, stage, generation, dead, harvestable,
--   regrowthRemaining, chopDesignated} | nil
--
--   The growth-state inspection window (#332): one entry per flora
--   instance on the tile, with the DERIVED state — effective age in
--   game-days, life-phase / annual-stage names (or nil where the
--   species defines none), placement health, reseed generation, and
--   whether a harvest would yield right now. nil when the tile has no
--   flora or its chunk isn't loaded. Poke the state by moving the
--   clock: world.setDate / world.setTime / world.setTimeScale.
--
--   #1854: each entry additionally carries its plant's stable
--   @instanceId@ and its own @chopDesignated@ flag, and its
--   @regrowthRemaining@ is that plant's OWN timer — under the old
--   tile-keyed map every harvestable co-tenant reported the same one.
--
--   Alias-accepting on the same terms as world.getFloraAt (#1707).
worldGetFloraGrowthAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetFloraGrowthAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let rawGX = fromIntegral gx'
                rawGY = fromIntegral gy'
            entries ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
                case mWs of
                    Nothing → pure []
                    Just ws → do
                        -- #1707: alias parity covers the WHOLE reported
                        -- state, timer included — see world.getFloraAt.
                        (gx, gy) ← canonicalPageTile ws rawGX rawGY
                        insts ← floraAt (toWorldSimCapability env) ws gx gy
                        harvests ← readIORef (wsFloraHarvestsRef ws)
                        (doy, absDay) ← growthClock ws
                        -- #1854: one timer per PLANT, so each entry
                        -- reports its own rather than the tile's.
                        pure
                            [ ( fsName sp, g
                              , fiHealth i
                              , lifePhaseText <$> growthPhaseTag sp g
                              , annualStageText <$> activeStageTag sp doy
                              , isJust (fsHarvest sp) ∧ timer ≤ 0
                                  ∧ harvestOpen sp doy g
                              , timer
                              , fiInstanceId i
                              , fiChopDesignated i )
                            | (i, sp) ← insts
                            , let g = floraGrowth sp absDay i
                                  timer = HM.lookupDefault 0
                                              (fiInstanceId i) harvests
                            ]
            case entries of
                [] → Lua.pushnil
                _  → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] entries) $
                        \(n, (name, g, health, mPhase, mStage
                             , harvestable, timer, iid, designated)) → do
                            Lua.newtable
                            Lua.pushstring (TE.encodeUtf8 name)
                            Lua.setfield (-2) "id"
                            Lua.pushnumber (Lua.Number (realToFrac (fgAge g)))
                            Lua.setfield (-2) "age"
                            Lua.pushnumber (Lua.Number (realToFrac health))
                            Lua.setfield (-2) "health"
                            case mPhase of
                                Just t → do
                                    Lua.pushstring (TE.encodeUtf8 t)
                                    Lua.setfield (-2) "phase"
                                Nothing → pure ()
                            case mStage of
                                Just t → do
                                    Lua.pushstring (TE.encodeUtf8 t)
                                    Lua.setfield (-2) "stage"
                                Nothing → pure ()
                            Lua.pushinteger (fromIntegral (fgGeneration g))
                            Lua.setfield (-2) "generation"
                            Lua.pushboolean (fgDead g)
                            Lua.setfield (-2) "dead"
                            Lua.pushboolean harvestable
                            Lua.setfield (-2) "harvestable"
                            Lua.pushnumber (Lua.Number (realToFrac timer))
                            Lua.setfield (-2) "regrowthRemaining"
                            -- #1854, ADDITIVE — see world.getFloraAt.
                            pushInstanceId iid
                            Lua.setfield (-2) "instanceId"
                            Lua.pushboolean designated
                            Lua.setfield (-2) "chopDesignated"
                            Lua.rawseti (-2) (fromIntegral n)
            return 1
        _ → Lua.pushnil >> return 1

-- | world.findHarvestableFlora(gx, gy, radius [, tag])
--   → {gx, gy, id, dist} | nil
--
--   Nearest currently-harvestable flora tile within @radius@ tiles
--   (Euclidean, clamped to 64 like getAreaFluid), scanning only LOADED
--   chunks. Skips tiles with a live regrowth timer.
--
--   With @tag@ (#97): only species whose harvest tags include it — the
--   chop tool/probe pass "wood" to find trees, in any growth state (a
--   standing-dead tree is still worth designating). WITHOUT a tag the
--   call is the foraging AI's food search, so only species whose yield
--   contains at least one EDIBLE item count (a bare call must not send
--   a starving unit to fell an oak for inedible logs), further gated on
--   the #332 growth window — off-season berry bushes don't distract
--   the forager from the clover that still yields. nil when nothing
--   matching is in range.
--
--   Seam-aware (#1707): the search origin may be any u-alias, a tile is
--   in range when it is PHYSICALLY within @radius@ (its nearest alias,
--   via 'seamTileDist2'), and the scan reaches a chunk whose stored key
--   is an alias of one the raw box names ('chunkInSeamRegion') — the
--   wild-flora and crop-plot branches share both, so they rank on one
--   geometry. @gx@/@gy@ come back CANONICAL (ready for
--   world.harvestFlora) and @dist@ is that same physical distance, which
--   is what the auto-harvest action divides into its utility. Ties keep
--   their historical order: distance, then canonical gx, gy, species
--   name. Identity inland and on a non-wrapping page.
--
--   #1854: a wild-flora winner additionally carries its plant's stable
--   @instanceId@ (ready for world.harvestFloraInstance), and the
--   regrowth skip is now per PLANT — one picked berry bush no longer
--   hides the oak sharing its tile. A CROP PLOT winner carries no
--   @instanceId@ at all: a plot is tile-keyed by construction and has no
--   instance identity to give, and the reserved non-identity value is
--   never handed out as if it were a usable id.
worldFindHarvestableFloraFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldFindHarvestableFloraFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mRad ← Lua.tointeger 3
    mTag ← Lua.tostring 4
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let rawGX = fromIntegral gx' ∷ Int
                rawGY = fromIntegral gy' ∷ Int
                radius = min 64 (max 1 (maybe 24 fromIntegral mRad)) ∷ Int
                tagFilter = TE.decodeUtf8Lenient <$> mTag
            mBest ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        tileData ← readIORef (wsTilesRef ws)
                        cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
                        harvests ← readIORef (wsFloraHarvestsRef ws)
                        cropPlots ← readIORef (wsCropPlotsRef ws)
                        (doy, absDay) ← growthClock ws
                        itemMgr ← readReadOnlyRef
                            (crvItemManagerRef (toContentRegistriesViewCapability env))
                        worldSize ← pageWrapWorldSize ws
                        -- Resolve the ORIGIN into the stored frame before
                        -- stepping the box outward from it: a caller may
                        -- pass any u-alias, and an alias origin would put
                        -- the far side of the seam two alias steps from
                        -- the box, past the one step 'chunkInSeamRegion'
                        -- covers. Canonical here, canonical everywhere.
                        (gx, gy) ← canonicalPageTile ws rawGX rawGY
                        let (cLo, _) = globalToChunk (gx - radius) (gy - radius)
                            (cHi, _) = globalToChunk (gx + radius) (gy + radius)
                            ChunkCoord cx0 cy0 = cLo
                            ChunkCoord cx1 cy1 = cHi
                            r2 = fromIntegral (radius * radius) ∷ Float
                            -- #1707: the box is stepped outward in the
                            -- origin's own frame, so near the seam it
                            -- steps PAST the canonical u range and names
                            -- ALIASES of the keys the tile store holds —
                            -- a raw lookupChunk per box key therefore
                            -- misses the wrapped neighbour entirely.
                            -- Containment counts those aliases (the same
                            -- test construction.getPendingJobs uses), and
                            -- every distance below is measured through the
                            -- tile's nearest alias so the radius cutoff
                            -- and the ordering agree with the geometry a
                            -- worker actually walks. Identity inland and
                            -- on a non-wrapping page.
                            dist2 = seamTileDist2 worldSize
                                        (fromIntegral gx, fromIntegral gy)
                            inRange coord = chunkInSeamRegion worldSize
                                                (cx0, cy0) (cx1, cy1) coord
                            edibleYield fh = or
                                [ isJust (idFood def)
                                | (yName, _, _) ← fhYield fh
                                , Just def ← [lookupItemDef yName itemMgr]
                                ]
                            wanted fh = case tagFilter of
                                Just tg → tg `elem` fhTags fh
                                Nothing → edibleYield fh
                            -- Scanning the STORED keys (and testing each
                            -- for alias containment) rather than looking
                            -- up each raw box key is what makes the
                            -- wrapped neighbour reachable; the coords
                            -- derived from a stored key are canonical, so
                            -- the harvest-timer skip and the reported
                            -- gx/gy stay in the frame every other verb
                            -- accepts.
                            candidates =
                                [ (d2, tgx, tgy, fsName sp, fiInstanceId i)
                                | (coord, lc) ← HM.toList (wtdChunks tileData)
                                , inRange coord
                                , i ← fcdInstances (lcFlora lc)
                                , Just sp ← [lookupSpecies (fiSpecies i) cat]
                                , Just fh ← [fsHarvest sp]
                                , wanted fh
                                -- #332: the growth window gates the
                                -- bare food search only (see above).
                                , case tagFilter of
                                    Just _  → True
                                    Nothing → harvestOpen sp doy
                                                  (floraGrowth sp absDay i)
                                , let (tgx, tgy) = chunkToGlobal coord
                                        (fromIntegral (fiTileX i))
                                        (fromIntegral (fiTileY i))
                                      d2 = dist2 (tgx, tgy)
                                , d2 ≤ r2
                                -- #1854: skip a plant whose OWN timer is
                                -- live, not every plant on a tile where
                                -- something was picked.
                                , not (HM.member (fiInstanceId i) harvests)
                                ]
                            -- Planted groundcover crop plots (#334) are a
                            -- world-level flat map, not chunk-embedded
                            -- FloraInstances, so they need their own scan
                            -- (never covered by the fcdInstances sweep
                            -- above). Mirrors worldHarvestFloraFn's plot
                            -- branch: BARE calls only (a tag is a
                            -- designation flow — chop's "wood" — and a
                            -- plot is never a designation target), no
                            -- regrowth-timer check (harvesting a plot
                            -- clears it outright instead of starting one).
                            cropCandidates = case tagFilter of
                                Just _  → []
                                Nothing →
                                    [ (d2, tgx, tgy, fsName sp
                                      , floraInstanceIdNone)
                                    | ((tgx, tgy), cp) ← HM.toList cropPlots
                                    , Just sp ← [lookupSpecies (cpSpecies cp) cat]
                                    , Just fh ← [fsHarvest sp]
                                    , wanted fh
                                    , let elapsed = cropPlotElapsedDays absDay cp
                                          g = floraGrowth sp elapsed
                                                  (cropPlotInstance cp)
                                    -- elapsed (days since planting) is the
                                    -- plot's own AGE clock (#334 — a plot's
                                    -- growth/phase timeline starts fresh at
                                    -- planting, not at the calendar epoch),
                                    -- but the fruiting-window annual-cycle
                                    -- gate must read the REAL calendar day
                                    -- (doy) — a future fruiting-stage
                                    -- groundcover species must ripen in
                                    -- season, not on an elapsed-day clock
                                    -- that drifts away from the calendar.
                                    , harvestOpen sp doy g
                                    -- Seam-aware like the wild-flora
                                    -- branch above (#1707): the two must
                                    -- agree, or a bare call ranks a plot
                                    -- and a wild plant on different
                                    -- geometries.
                                    , let d2 = dist2 (tgx, tgy)
                                    , d2 ≤ r2
                                    ]
                        pure $ case candidates ⧺ cropCandidates of
                            [] → Nothing
                            cs → Just (minimum cs)
            case mBest of
                Nothing → Lua.pushnil
                Just (d2, tgx, tgy, name, iid) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral tgx)
                    Lua.setfield (-2) "gx"
                    Lua.pushinteger (fromIntegral tgy)
                    Lua.setfield (-2) "gy"
                    Lua.pushstring (TE.encodeUtf8 name)
                    Lua.setfield (-2) "id"
                    Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                    Lua.setfield (-2) "dist"
                    -- #1854, ADDITIVE: the winning plant's stable
                    -- identity, ABSENT for a crop plot (which has no
                    -- instance identity to give — the reserved
                    -- non-identity value is never exposed as a usable
                    -- id). @id@ still means the species name.
                    unless (isFloraInstanceIdNone iid) $ do
                        pushInstanceId iid
                        Lua.setfield (-2) "instanceId"
            return 1
        _ → Lua.pushnil >> return 1

-- | item.getFood(defName) → {calories, caloriesPerKg} | nil
--
--   Food data straight off the item def, nil for non-food (or unknown)
--   items. Lets the AI recognise edible GROUND items (unit.getInventory
--   already exposes food data for carried ones).
itemGetFoodFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGetFoodFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Nothing → Lua.pushnil >> return 1
        Just nameBS → do
            let name = TE.decodeUtf8Lenient nameBS
            mFood ← Lua.liftIO $ do
                itemMgr ← readReadOnlyRef
                    (crvItemManagerRef (toContentRegistriesViewCapability env))
                pure (lookupItemDef name itemMgr ⌦ idFood)
            case mFood of
                Nothing → Lua.pushnil
                Just f → do
                    Lua.newtable
                    Lua.pushnumber (Lua.Number (realToFrac (ifCalories f)))
                    Lua.setfield (-2) "calories"
                    Lua.pushnumber (Lua.Number (realToFrac (ifCaloriesPerKg f)))
                    Lua.setfield (-2) "caloriesPerKg"
            return 1

-- | world.getCropPlotAt(gx, gy) → {id, age, health, phase, stage,
--   generation, dead, harvestable} | nil
--
--   The #332 growth-state inspection window for a planted groundcover
--   crop plot (#334) — mirrors world.getFloraGrowthAt's shape for wild/
--   row flora, but for a single tile-keyed plot rather than an array of
--   instances, and with age measured in days ELAPSED SINCE PLANTING
--   (World.Flora.CropPlot) rather than an absolute placement baseline.
--   nil when the tile has no planted crop or names an unregistered
--   species.
worldGetCropPlotAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetCropPlotAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
            mResult ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        plots ← readIORef (wsCropPlotsRef ws)
                        case HM.lookup (gx, gy) plots of
                            Nothing → pure Nothing
                            Just cp → do
                                cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
                                (doy, absDay) ← growthClock ws
                                pure $ do
                                    sp ← lookupSpecies (cpSpecies cp) cat
                                    let elapsed = cropPlotElapsedDays absDay cp
                                        g = floraGrowth sp elapsed
                                                (cropPlotInstance cp)
                                    -- elapsed is the plot's own age clock
                                    -- (fine for fgAge/phase); the annual
                                    -- cycle stage and fruiting-window gate
                                    -- read the real calendar day (doy), to
                                    -- agree with world.harvestFlora /
                                    -- findHarvestableFlora above.
                                    Just ( fsName sp, g, cpHealth cp
                                         , lifePhaseText ⊚ growthPhaseTag sp g
                                         , annualStageText ⊚
                                               activeStageTag sp doy
                                         , isJust (fsHarvest sp)
                                             ∧ harvestOpen sp doy g )
            case mResult of
                Nothing → Lua.pushnil
                Just (name, g, health, mPhase, mStage, harvestable) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 name)
                    Lua.setfield (-2) "id"
                    Lua.pushnumber (Lua.Number (realToFrac (fgAge g)))
                    Lua.setfield (-2) "age"
                    Lua.pushnumber (Lua.Number (realToFrac health))
                    Lua.setfield (-2) "health"
                    case mPhase of
                        Just t → do
                            Lua.pushstring (TE.encodeUtf8 t)
                            Lua.setfield (-2) "phase"
                        Nothing → pure ()
                    case mStage of
                        Just t → do
                            Lua.pushstring (TE.encodeUtf8 t)
                            Lua.setfield (-2) "stage"
                        Nothing → pure ()
                    Lua.pushinteger (fromIntegral (fgGeneration g))
                    Lua.setfield (-2) "generation"
                    Lua.pushboolean (fgDead g)
                    Lua.setfield (-2) "dead"
                    Lua.pushboolean harvestable
                    Lua.setfield (-2) "harvestable"
            return 1
        _ → Lua.pushnil >> return 1

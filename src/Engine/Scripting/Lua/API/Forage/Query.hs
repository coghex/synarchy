{-# LANGUAGE Strict, UnicodeSyntax #-}
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
import Engine.Core.State (EngineEnv(..), activeWorldState)
import World.Types
import World.Flora.Growth (FloraGrowth(..), floraGrowth, harvestOpen,
                           growthPhaseTag, activeStageTag,
                           lifePhaseText, annualStageText)
import World.Flora.CropPlot (CropPlot(..), cropPlotElapsedDays,
                             cropPlotInstance)
import World.Generate.Coordinates (globalToChunk, chunkToGlobal)
import Item.Types (ItemDef(..), ItemFood(..), lookupItemDef)
import Engine.Scripting.Lua.API.Forage.Lookup (floraAt, growthClock)

-- | world.getFloraAt(gx, gy) → {id, harvestable, regrowthRemaining,
--   tags} | nil
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
--   Designation flows must NOT read @harvestable@ (it is the
--   forage-facing signal): the chop AI's claim check keys on
--   @regrowthRemaining@ + @tags@, so a designated tree stays choppable
--   as a sprout or standing dead. Per-instance gated state is
--   world.getFloraGrowthAt.
worldGetFloraAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetFloraAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
            mResult ← Lua.liftIO $ do
                mWs ← activeWorldState env
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        insts ← floraAt env ws gx gy
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
                            (p@(_, sp):_) →
                                let timer = HM.lookupDefault 0 (gx, gy) harvests
                                in Just ( fsName sp
                                        , isJust (fsHarvest sp) ∧ timer ≤ 0
                                            ∧ open p
                                        , timer
                                        , maybe [] fhTags (fsHarvest sp) )
            case mResult of
                Nothing → Lua.pushnil
                Just (name, harvestable, timer, tags) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 name)
                    Lua.setfield (-2) "id"
                    Lua.pushboolean harvestable
                    Lua.setfield (-2) "harvestable"
                    Lua.pushnumber (Lua.Number (realToFrac timer))
                    Lua.setfield (-2) "regrowthRemaining"
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] tags) $ \(i, tg) → do
                        Lua.pushstring (TE.encodeUtf8 tg)
                        Lua.rawseti (-2) (fromIntegral i)
                    Lua.setfield (-2) "tags"
            return 1
        _ → Lua.pushnil >> return 1

-- | world.getFloraGrowthAt(gx, gy) → array of {id, age, health, phase,
--   stage, generation, dead, harvestable, regrowthRemaining} | nil
--
--   The growth-state inspection window (#332): one entry per flora
--   instance on the tile, with the DERIVED state — effective age in
--   game-days, life-phase / annual-stage names (or nil where the
--   species defines none), placement health, reseed generation, and
--   whether a harvest would yield right now. nil when the tile has no
--   flora or its chunk isn't loaded. Poke the state by moving the
--   clock: world.setDate / world.setTime / world.setTimeScale.
worldGetFloraGrowthAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetFloraGrowthAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
            entries ← Lua.liftIO $ do
                mWs ← activeWorldState env
                case mWs of
                    Nothing → pure []
                    Just ws → do
                        insts ← floraAt env ws gx gy
                        harvests ← readIORef (wsFloraHarvestsRef ws)
                        (doy, absDay) ← growthClock ws
                        let timer = HM.lookupDefault 0 (gx, gy) harvests
                        pure
                            [ ( fsName sp, g
                              , fiHealth i
                              , lifePhaseText <$> growthPhaseTag sp g
                              , annualStageText <$> activeStageTag sp doy
                              , isJust (fsHarvest sp) ∧ timer ≤ 0
                                  ∧ harvestOpen sp doy g
                              , timer )
                            | (i, sp) ← insts
                            , let g = floraGrowth sp absDay i
                            ]
            case entries of
                [] → Lua.pushnil
                _  → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] entries) $
                        \(n, (name, g, health, mPhase, mStage
                             , harvestable, timer)) → do
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
worldFindHarvestableFloraFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldFindHarvestableFloraFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mRad ← Lua.tointeger 3
    mTag ← Lua.tostring 4
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx' ∷ Int
                gy = fromIntegral gy' ∷ Int
                radius = min 64 (max 1 (maybe 24 fromIntegral mRad)) ∷ Int
                tagFilter = TE.decodeUtf8 <$> mTag
            mBest ← Lua.liftIO $ do
                mWs ← activeWorldState env
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        tileData ← readIORef (wsTilesRef ws)
                        cat ← readIORef (floraCatalogRef env)
                        harvests ← readIORef (wsFloraHarvestsRef ws)
                        cropPlots ← readIORef (wsCropPlotsRef ws)
                        (doy, absDay) ← growthClock ws
                        itemMgr ← readIORef (itemManagerRef env)
                        let (cLo, _) = globalToChunk (gx - radius) (gy - radius)
                            (cHi, _) = globalToChunk (gx + radius) (gy + radius)
                            ChunkCoord cx0 cy0 = cLo
                            ChunkCoord cx1 cy1 = cHi
                            r2 = radius * radius
                            edibleYield fh = or
                                [ isJust (idFood def)
                                | (yName, _, _) ← fhYield fh
                                , Just def ← [lookupItemDef yName itemMgr]
                                ]
                            wanted fh = case tagFilter of
                                Just tg → tg `elem` fhTags fh
                                Nothing → edibleYield fh
                            candidates =
                                [ (d2, tgx, tgy, fsName sp)
                                | cx ← [cx0 .. cx1], cy ← [cy0 .. cy1]
                                , Just lc ← [lookupChunk (ChunkCoord cx cy) tileData]
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
                                , let (tgx, tgy) = chunkToGlobal (ChunkCoord cx cy)
                                        (fromIntegral (fiTileX i))
                                        (fromIntegral (fiTileY i))
                                      d2 = (tgx - gx) * (tgx - gx)
                                         + (tgy - gy) * (tgy - gy)
                                , d2 ≤ r2
                                , not (HM.member (tgx, tgy) harvests)
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
                                    [ (d2, tgx, tgy, fsName sp)
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
                                    , let d2 = (tgx - gx) * (tgx - gx)
                                             + (tgy - gy) * (tgy - gy)
                                    , d2 ≤ r2
                                    ]
                        pure $ case candidates ⧺ cropCandidates of
                            [] → Nothing
                            cs → Just (minimum cs)
            case mBest of
                Nothing → Lua.pushnil
                Just (d2, tgx, tgy, name) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral tgx)
                    Lua.setfield (-2) "gx"
                    Lua.pushinteger (fromIntegral tgy)
                    Lua.setfield (-2) "gy"
                    Lua.pushstring (TE.encodeUtf8 name)
                    Lua.setfield (-2) "id"
                    Lua.pushnumber (Lua.Number
                        (realToFrac (sqrt (fromIntegral d2 ∷ Float))))
                    Lua.setfield (-2) "dist"
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
            let name = TE.decodeUtf8 nameBS
            mFood ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                pure (lookupItemDef name itemMgr >>= idFood)
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
                mWs ← activeWorldState env
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        plots ← readIORef (wsCropPlotsRef ws)
                        case HM.lookup (gx, gy) plots of
                            Nothing → pure Nothing
                            Just cp → do
                                cat ← readIORef (floraCatalogRef env)
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

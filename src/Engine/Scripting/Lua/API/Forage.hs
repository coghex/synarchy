{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Interactive-flora Lua API (#94): query and harvest the flora the
--   worldgen placed, so the foraging AI (scripts/unit_ai.lua) can feed
--   hungry units from the land.
--
--   Harvest STATE lives in the world-level 'wsFloraHarvestsRef' map
--   (tile → regrowth game-seconds), not in the chunk — see
--   World.Flora.Harvest for why. All three functions run directly on
--   the Lua thread against the active world's refs, the same pattern
--   as the WorldQuery reads and item.spawnGround.
module Engine.Scripting.Lua.API.Forage
    ( worldGetFloraAtFn
    , worldGetFloraGrowthAtFn
    , worldHarvestFloraFn
    , worldFindHarvestableFloraFn
    , itemGetFoodFn
    , worldPlantCropAtFn
    , worldGetCropPlotAtFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, atomicModifyIORef')
import System.Random (randomR)
import Engine.Core.State (EngineEnv(..), activeWorldState,
                          freshItemInstanceId)
import World.Types
import World.Vegetation (isTilledSoil)
import World.Flora.Growth (FloraGrowth(..), floraGrowth, harvestOpen,
                           growthPhaseTag, activeStageTag,
                           lifePhaseText, annualStageText)
import World.Flora.CropPlot (CropPlot(..), newCropPlot,
                             cropPlotElapsedDays, cropPlotInstance)
import World.Generate.Coordinates (globalToChunk, chunkToGlobal)
import Item.Types (ItemDef(..), ItemInstance(..), ItemContainer(..),
                   ItemFood(..), lookupItemDef)
import Item.Ground (spawnGroundItem)
import Item.Roll (rollItemSpec, rollItemWeight)

-- | Every flora instance on tile (gx, gy) of the active world's loaded
--   chunks, joined with its species. Empty when the chunk isn't loaded.
floraAt ∷ EngineEnv → WorldState → Int → Int
        → IO [(FloraInstance, FloraSpecies)]
floraAt env ws gx gy = do
    tileData ← readIORef (wsTilesRef ws)
    cat ← readIORef (floraCatalogRef env)
    let (coord, (lx, ly)) = globalToChunk gx gy
    pure $ case lookupChunk coord tileData of
        Nothing → []
        Just lc →
            [ (i, sp)
            | i ← fcdInstances (lcFlora lc)
            , fromIntegral (fiTileX i) ≡ lx
            , fromIntegral (fiTileY i) ≡ ly
            , Just sp ← [lookupSpecies (fiSpecies i) cat]
            ]

-- | The world's growth clock (#332): (day-of-year, absolute day),
--   converted through the page's calendar.
growthClock ∷ WorldState → IO (Int, Int)
growthClock ws = do
    paramsM ← readIORef (wsGenParamsRef ws)
    date ← readIORef (wsDateRef ws)
    let calendar = maybe defaultCalendarConfig wgpCalender paramsM
    pure ( worldDateToDayOfYear calendar date
         , worldAbsoluteDay calendar date )

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

-- | world.harvestFlora(gx, gy [, tag]) → array of {id, gid} | nil
--
--   Harvests the tile's (first) harvestable-species instance: rolls each
--   yield entry's count, spawns the items as ground items scattered
--   around the tile, starts the regrowth timer, and invalidates the quad
--   cache so the depleted texture shows. One table entry per spawned
--   ITEM — @gid@ is the ground-item id, ready for item.pickupGround.
--   With @tag@ (#97) only a species carrying that harvest tag is
--   harvested — the chop AI passes "wood" so a shared tile can't trade
--   its berry bush for the designated tree. A BARE call is a forage,
--   additionally gated on the #332 growth window (skips dead plants,
--   juveniles, and fruiting species out of season); tagged calls
--   address designation flows and skip the window — a standing-dead
--   tree still chops. nil when the tile has nothing (matching)
--   harvestable — the codebase signals failure with nil rather than
--   raising.
worldHarvestFloraFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHarvestFloraFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mTag ← Lua.tostring 3
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                tagFilter = TE.decodeUtf8 <$> mTag
            mSpawned ← Lua.liftIO $ do
                mWs ← activeWorldState env
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        -- Planted crop plot (#334): a BARE call only —
                        -- like chop's tagged flow, a plot isn't a
                        -- designation target, so a tag skips it
                        -- straight to the wild-flora path below. A
                        -- plot never coexists with wild FloraInstances
                        -- on the same tile (tilled soil excludes
                        -- natural flora placement), so no precedence
                        -- question between the two arises.
                        mPlot ← if isJust tagFilter then pure Nothing
                                else HM.lookup (gx, gy) ⊚
                                         readIORef (wsCropPlotsRef ws)
                        cat ← readIORef (floraCatalogRef env)
                        (doy, absDay) ← growthClock ws
                        let mPlotHarvest = do
                                cp ← mPlot
                                sp ← lookupSpecies (cpSpecies cp) cat
                                fh ← fsHarvest sp
                                let elapsed = cropPlotElapsedDays absDay cp
                                    g = floraGrowth sp elapsed (cropPlotInstance cp)
                                if harvestOpen sp elapsed g
                                    then Just fh else Nothing
                        case mPlotHarvest of
                            Just fh → do
                                spawned ← spawnYields env ws gx gy (fhYield fh)
                                -- Annual, one-shot: harvesting clears
                                -- the plot instead of starting a
                                -- regrowth timer — the tile reverts to
                                -- bare tilled soil until replanted.
                                atomicModifyIORef' (wsCropPlotsRef ws) $
                                    \ps → (HM.delete (gx, gy) ps, ())
                                bumpQuadCacheGen ws
                                pure (Just spawned)
                            Nothing | isJust mPlot → pure Nothing
                            Nothing → do
                              insts ← floraAt env ws gx gy
                              harvests ← readIORef (wsFloraHarvestsRef ws)
                              let live = HM.lookupDefault 0 (gx, gy) harvests
                                  -- #332: only the BARE (forage) call
                                  -- checks the growth window; a tagged
                                  -- call is a designation flow (chop
                                  -- "wood") and takes the plant in any
                                  -- growth state.
                                  windowOk i sp = case tagFilter of
                                      Just _  → True
                                      Nothing → harvestOpen sp doy
                                                    (floraGrowth sp absDay i)
                                  mFh = listToMaybe
                                      [ fh2 | (i, sp) ← insts
                                           , Just fh2 ← [fsHarvest sp]
                                           , maybe True (`elem` fhTags fh2) tagFilter
                                           , windowOk i sp ]
                              case mFh of
                                  Just fh | live ≤ 0 → do
                                      spawned ← spawnYields env ws gx gy (fhYield fh)
                                      atomicModifyIORef' (wsFloraHarvestsRef ws) $
                                          \hs → (HM.insert (gx, gy) (fhRegrowth fh) hs, ())
                                      bumpQuadCacheGen ws
                                      pure (Just spawned)
                                  _ → pure Nothing
            case mSpawned of
                Nothing → Lua.pushnil
                Just spawned → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] spawned) $ \(i, (name, gid)) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.setfield (-2) "id"
                        Lua.pushinteger (fromIntegral gid)
                        Lua.setfield (-2) "gid"
                        Lua.rawseti (-2) (fromIntegral i)
            return 1
        _ → Lua.pushnil >> return 1

-- | Roll and spawn one harvest's yields as ground items scattered a
--   little around the tile center. Unknown item names are skipped (the
--   YAML referenced an item that doesn't exist — same silent-skip as
--   starting_inventory). Returns (defName, groundId) per spawned item.
spawnYields ∷ EngineEnv → WorldState → Int → Int → [(Text, Int, Int)]
            → IO [(Text, Int)]
spawnYields env ws gx gy yields = do
    itemMgr ← readIORef (itemManagerRef env)
    fmap concat $ forM yields $ \(name, lo, hi) →
        case lookupItemDef name itemMgr of
            Nothing → pure []
            Just def → do
                count ← atomicModifyIORef' (statRNGRef env) $ \g →
                    let (v, g') = randomR (lo, hi) g in (g', v)
                forM [1 .. max 0 count] $ \_ → do
                    qual ← rollItemSpec (idQualitySpec def) (statRNGRef env)
                    cond ← rollItemSpec (idConditionSpec def) (statRNGRef env)
                    wght ← rollItemWeight def (statRNGRef env)
                    iid ← freshItemInstanceId env
                    (ju, jv) ← atomicModifyIORef' (statRNGRef env) $ \g →
                        let (u, g')  = randomR (-0.3, 0.3 ∷ Float) g
                            (v, g'') = randomR (-0.3, 0.3 ∷ Float) g'
                        in (g'', (u, v))
                    let fill = case idContainer def of
                            Just c  → max 0 (min (icCapacity c) (icDefaultFill c))
                            Nothing → 0
                        inst = ItemInstance
                            { iiDefName     = name
                            , iiCurrentFill = fill
                            , iiQuality     = qual
                            , iiCondition   = cond
                            , iiWeight      = wght
                            , iiSharpness   = 100.0
                            , iiContents    = []
                            , iiInstanceId  = iid
                            , iiTemp        = Nothing
                            }
                    gid ← atomicModifyIORef' (wsGroundItemsRef ws) $
                        spawnGroundItem inst
                            (fromIntegral gx + 0.5 + ju)
                            (fromIntegral gy + 0.5 + jv)
                    pure (name, gid)

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
                        pure $ case candidates of
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

-- | Find a registered flora species by its YAML @name@. Catalogs are
--   small (tens of species), so a linear scan needs no index.
findSpeciesByName ∷ Text → FloraCatalog → Maybe (FloraId, FloraSpecies)
findSpeciesByName name cat =
    listToMaybe [ (FloraId k, sp)
                | (k, sp) ← HM.toList (fcSpecies cat), fsName sp ≡ name ]

-- | The one YAML worldGen category tag (World.Flora.Placement) that
--   marks a species as the ctVeg-tile-fill groundcover form (#334) —
--   the only form world.plantCropAt (a CropPlot) is valid for. A
--   row_crop species is an ordinary FloraInstance placed by the
--   worldgen pipeline; planting one as a CropPlot would render it as a
--   tile-fill (World.Render.Quads), the WRONG form for its species.
groundcoverCropCategory ∷ Text
groundcoverCropCategory = "groundcover_crop"

-- | world.plantCropAt(gx, gy, speciesName) → true | nil
--
--   Foundation-level planting primitive (#334): plants a groundcover
--   crop plot at (gx, gy) at full health, dated to the CURRENT world
--   day (the #332 runtime's age-0 baseline for this plot — see
--   World.Flora.CropPlot). Refuses (nil) unless the tile is plantable
--   (tilled soil, #333 — same gate as world.isPlantable), speciesName
--   names a REGISTERED GROUNDCOVER species (worldGen category
--   "groundcover_crop" — a row_crop species, e.g. tomato_plant, is
--   refused: it's an ordinary FloraInstance, not a CropPlot, see
--   groundcoverCropCategory), and the tile's chunk is loaded.
--
--   This is a low-level verb with no player-facing tool/AI/UI yet —
--   #335 (planting tool + suitability) and #336 (farm AI) build the
--   real planting flow on top of it, the same foundation-first shape
--   as #300's unit.repairItem preceding the repair AI/UI. A planted
--   plot renders as the tile's veg-fill (World.Render.Quads), NOT a
--   floating sprite.
worldPlantCropAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldPlantCropAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mName ← Lua.tostring 3
    case (mGx, mGy, mName) of
        (Just gx', Just gy', Just nameBS) → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                name = TE.decodeUtf8 nameBS
            ok ← Lua.liftIO $ do
                mWs ← activeWorldState env
                case mWs of
                    Nothing → pure False
                    Just ws → do
                        tileData ← readIORef (wsTilesRef ws)
                        let (coord, (lx, ly)) = globalToChunk gx gy
                            idx = ly * chunkSize + lx
                        case lookupChunk coord tileData of
                            Nothing → pure False
                            Just lc → do
                                let col = lcTiles lc V.! idx
                                    z    = lcSurfaceMap lc VU.! idx
                                    i    = z - ctStartZ col
                                    vg   = if i ≥ 0 ∧ i < VU.length (ctVeg col)
                                           then ctVeg col VU.! i else 0
                                if not (isTilledSoil vg) then pure False
                                else do
                                    cat ← readIORef (floraCatalogRef env)
                                    case findSpeciesByName name cat of
                                        Nothing → pure False
                                        Just (fid, _) →
                                            case HM.lookup (unFloraId fid)
                                                     (fcWorldGen cat) of
                                                Just wg
                                                    | fwCategory wg
                                                        ≡ groundcoverCropCategory → do
                                                    (_, absDay) ← growthClock ws
                                                    let plot = newCropPlot fid absDay 1.0
                                                    atomicModifyIORef' (wsCropPlotsRef ws) $
                                                        \ps → (HM.insert (gx, gy) plot ps, ())
                                                    bumpQuadCacheGen ws
                                                    pure True
                                                _ → pure False
            Lua.pushboolean ok
            return 1
        _ → Lua.pushnil >> return 1

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
                                (_, absDay) ← growthClock ws
                                pure $ do
                                    sp ← lookupSpecies (cpSpecies cp) cat
                                    let elapsed = cropPlotElapsedDays absDay cp
                                        g = floraGrowth sp elapsed
                                                (cropPlotInstance cp)
                                    Just ( fsName sp, g, cpHealth cp
                                         , lifePhaseText ⊚ growthPhaseTag sp g
                                         , annualStageText ⊚
                                               activeStageTag sp elapsed
                                         , isJust (fsHarvest sp)
                                             ∧ harvestOpen sp elapsed g )
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

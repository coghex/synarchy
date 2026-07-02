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
    , worldHarvestFloraFn
    , worldFindHarvestableFloraFn
    , itemGetFoodFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, atomicModifyIORef')
import System.Random (randomR)
import Engine.Core.State (EngineEnv(..), activeWorldState,
                          freshItemInstanceId)
import World.Types
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

-- | world.getFloraAt(gx, gy) → {id, harvestable, regrowthRemaining,
--   tags} | nil
--
--   nil when the tile has no flora (or its chunk isn't loaded). When
--   several instances share the tile, a harvestable species wins the
--   report (a berry bush over the decorative dandelion beside it).
--   @harvestable@ is true only for a harvestable SPECIES with no live
--   regrowth timer; @regrowthRemaining@ is the timer in game-seconds
--   (0 when none). @tags@ is the species' harvest-tag array (#97 —
--   "wood" marks a choppable tree; empty for non-harvestable flora).
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
                        let harvestFirst =
                                [ p | p@(_, sp) ← insts
                                    , isJust (fsHarvest sp) ] <> insts
                        pure $ case harvestFirst of
                            [] → Nothing
                            ((_, sp):_) →
                                let timer = HM.lookupDefault 0 (gx, gy) harvests
                                in Just ( fsName sp
                                        , isJust (fsHarvest sp) ∧ timer ≤ 0
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

-- | world.harvestFlora(gx, gy [, tag]) → array of {id, gid} | nil
--
--   Harvests the tile's (first) harvestable-species instance: rolls each
--   yield entry's count, spawns the items as ground items scattered
--   around the tile, starts the regrowth timer, and invalidates the quad
--   cache so the depleted texture shows. One table entry per spawned
--   ITEM — @gid@ is the ground-item id, ready for item.pickupGround.
--   With @tag@ (#97) only a species carrying that harvest tag is
--   harvested — the chop AI passes "wood" so a shared tile can't trade
--   its berry bush for the designated tree. nil when the tile has
--   nothing (matching) harvestable — the codebase signals failure with
--   nil rather than raising.
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
                        insts ← floraAt env ws gx gy
                        harvests ← readIORef (wsFloraHarvestsRef ws)
                        let live = HM.lookupDefault 0 (gx, gy) harvests
                            mFh = listToMaybe
                                [ fh | (_, sp) ← insts
                                     , Just fh ← [fsHarvest sp]
                                     , maybe True (`elem` fhTags fh) tagFilter ]
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
--   chop tool/probe pass "wood" to find trees. WITHOUT a tag the call
--   is the foraging AI's food search, so only species whose yield
--   contains at least one EDIBLE item count: before choppable trees
--   every harvestable was food and the distinction didn't exist, but a
--   bare call must not send a starving unit to fell an oak for
--   inedible logs. nil when nothing matching is in range.
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

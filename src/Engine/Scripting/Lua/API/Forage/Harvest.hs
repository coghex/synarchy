{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Forage harvest verb (#94/#332/#334): picks the tile's harvestable
--   instance (wild flora or a planted crop plot), rolls and spawns its
--   yield as ground items, and starts the regrowth timer / clears the
--   plot. See Engine.Scripting.Lua.API.Forage.Query for the read-only
--   counterparts and .Crop for planting.
module Engine.Scripting.Lua.API.Forage.Harvest
    ( worldHarvestFloraFn
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
import World.Flora.Growth (floraGrowth, harvestOpen)
import World.Flora.CropPlot (CropPlot(..), cropPlotElapsedDays,
                             cropPlotInstance)
import Item.Types (ItemDef(..), ItemInstance(..), ItemContainer(..),
                   lookupItemDef)
import Item.Ground (spawnGroundItem)
import Item.Roll (rollItemSpec, rollItemWeight)
import Engine.Scripting.Lua.API.Forage.Lookup (floraAt, growthClock)

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
                tagFilter = TE.decodeUtf8Lenient <$> mTag
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
                                -- elapsed is the plot's own age clock; the
                                -- fruiting-window gate reads the real
                                -- calendar day (doy), matching
                                -- world.findHarvestableFlora's crop-plot
                                -- scan and world.getCropPlotAt — a future
                                -- fruiting-stage groundcover species must
                                -- agree across all three query/action
                                -- entry points.
                                if harvestOpen sp doy g
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

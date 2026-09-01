{-# LANGUAGE Strict #-}
-- | Forage harvest verb (#94/#332/#334): picks the tile's harvestable
--   instance (wild flora or a planted crop plot), rolls and spawns its
--   yield as ground items, and starts the regrowth timer / clears the
--   plot. See Engine.Scripting.Lua.API.Forage.Query for the read-only
--   counterparts and .Crop for planting.
module Engine.Scripting.Lua.API.Forage.Harvest
    ( worldHarvestFloraFn
    , worldHarvestFloraInstanceFn
    ) where

import UPrelude
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.Core
    (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import System.Random (randomR)
import Engine.Core.State (EngineEnv, activeWorldStateFrom, freshItemInstanceId)
import World.Types
import World.Generate.Coordinates (canonicalTile)
import World.Flora.Growth (floraGrowth, harvestOpen)
import World.Flora.CropPlot (CropPlot(..), cropPlotElapsedDays,
                             cropPlotInstance)
import Item.Types (lookupItemDef)
import Item.Ground (spawnGroundItem)
import Item.Materialize (materializeItem, pristineItem)
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
            let rawGX = fromIntegral gx'
                rawGY = fromIntegral gy'
                tagFilter = TE.decodeUtf8Lenient <$> mTag
            mSpawned ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom (wsWorldManagerRef (toWorldSimCapability env))
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        -- #1175: the chop AI harvests at a chop-designation
                        -- coord, which a pre-#1175 save can hold as a
                        -- u-alias, and every tile-keyed map consulted below
                        -- (crop plots, flora harvests) is canonical.
                        -- Identity inland.
                        worldSize ← pageWrapWorldSize ws
                        let (gx, gy) = canonicalTile worldSize rawGX rawGY
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
                        cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
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
                            Nothing → harvestWildFlora env ws gx gy
                                          tagFilter Nothing doy absDay
            pushHarvestResult mSpawned
            return 1
        _ → Lua.pushnil >> return 1

-- | world.harvestFloraInstance(gx, gy, instanceId [, tag])
--   → array of {id, gid} | nil
--
--   The EXACT-INSTANCE harvest (#1854 requirement 10). Same result
--   shape and the same yields as world.harvestFlora, but it names ONE
--   plant: the chop AI fells the tree it claimed, never the berry bush
--   beside it, and only that tree's own regrowth timer starts. The tile
--   is still passed so the lookup stays a single seam-aware tile read
--   rather than a scan of every resident chunk.
--
--   Deliberately ADDITIVE: world.harvestFlora keeps its coordinate
--   contract and result shape for every existing forage caller (needs,
--   farm-harvest), which is what stops this from being a silent API
--   break.
--
--   nil when the tile does not hold that instance, when the instance is
--   not a (matching) harvestable species, or when its own timer is
--   live — the same nil-on-failure signalling the rest of the family
--   uses.
worldHarvestFloraInstanceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHarvestFloraInstanceFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mIid ← Lua.tointeger 3
    mTag ← Lua.tostring 4
    case (mGx, mGy, floraInstanceIdFromLua . fromIntegral =≪ mIid) of
        (Just gx', Just gy', Just iid) → do
            let rawGX = fromIntegral gx'
                rawGY = fromIntegral gy'
                tagFilter = TE.decodeUtf8Lenient <$> mTag
            mSpawned ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom
                          (wsWorldManagerRef (toWorldSimCapability env))
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        worldSize ← pageWrapWorldSize ws
                        let (gx, gy) = canonicalTile worldSize rawGX rawGY
                        (doy, absDay) ← growthClock ws
                        harvestWildFlora env ws gx gy tagFilter (Just iid)
                            doy absDay
            pushHarvestResult mSpawned
            return 1
        _ → Lua.pushnil >> return 1

-- | The wild-flora harvest both verbs above share.
--
--   #1854: the regrowth timer is read and written per INSTANCE, so the
--   plant taken is the only one whose state moves. A bare (untagged)
--   call keeps its historical coordinate behaviour exactly — first
--   matching harvestable species on the tile, in the chunk's own stored
--   order, gated on the #332 growth window — with one deliberate
--   improvement that falls straight out of per-instance keying: a
--   co-tenant with a live timer no longer suppresses the whole tile, it
--   is simply skipped.
harvestWildFlora
    ∷ EngineEnv → WorldState → Int → Int → Maybe Text
    → Maybe FloraInstanceId → Int → Int → IO (Maybe [(Text, Int)])
harvestWildFlora env ws gx gy tagFilter mWanted doy absDay = do
    insts ← floraAt (toWorldSimCapability env) ws gx gy
    harvests ← readIORef (wsFloraHarvestsRef ws)
    let -- #332: only the BARE (forage) call checks the growth window; a
        -- tagged call is a designation flow (chop "wood") and takes the
        -- plant in any growth state.
        windowOk i sp = case tagFilter of
            Just _  → True
            Nothing → harvestOpen sp doy (floraGrowth sp absDay i)
        picked = listToMaybe
            [ (i, fh)
            | (i, sp) ← insts
            , maybe True (≡ fiInstanceId i) mWanted
            , Just fh ← [fsHarvest sp]
            , maybe True (`elem` fhTags fh) tagFilter
            , windowOk i sp
            , HM.lookupDefault 0 (fiInstanceId i) harvests ≤ 0
            ]
    case picked of
        Nothing → pure Nothing
        Just (i, fh) → do
            spawned ← spawnYields env ws gx gy (fhYield fh)
            atomicModifyIORef' (wsFloraHarvestsRef ws) $ \hs →
                (HM.insert (fiInstanceId i) (fhRegrowth fh) hs, ())
            bumpQuadCacheGen ws
            pure (Just spawned)

-- | The one spelling of both verbs' Lua return value, so the exact
--   path can never drift from the coordinate path's shape.
pushHarvestResult
    ∷ Maybe [(Text, Int)] → Lua.LuaE Lua.Exception ()
pushHarvestResult Nothing = Lua.pushnil
pushHarvestResult (Just spawned) = do
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] spawned) $ \(i, (name, gid)) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.setfield (-2) "id"
        Lua.pushinteger (fromIntegral gid)
        Lua.setfield (-2) "gid"
        Lua.rawseti (-2) (fromIntegral i)

-- | Roll and spawn one harvest's yields as ground items scattered a
--   little around the tile center. Unknown item names are skipped (the
--   YAML referenced an item that doesn't exist — same silent-skip as
--   starting_inventory). Returns (defName, groundId) per spawned item.
spawnYields ∷ EngineEnv → WorldState → Int → Int → [(Text, Int, Int)]
            → IO [(Text, Int)]
spawnYields env ws gx gy yields = do
    itemMgr ← readReadOnlyRef (crvItemManagerRef (toContentRegistriesViewCapability env))
    logger ← readIORef (ccLoggerRef (toCoreCapability env))
    fmap concat $ forM yields $ \(name, lo, hi) →
        case lookupItemDef name itemMgr of
            Nothing → pure []
            Just _ → do
                count ← atomicModifyIORef' (ucStatRNGRef (toUnitCombatCapability env)) $ \g →
                    let (v, g') = randomR (lo, hi) g in (g', v)
                fmap catMaybes ∘ forM [1 .. max 0 count] $ \_ → do
                    -- Every instance value is the materializer's (#1418);
                    -- this path contributes no override.
                    mInst ← materializeItem itemMgr logger
                                (ucStatRNGRef (toUnitCombatCapability env))
                                (freshItemInstanceId env) pristineItem name
                    (ju, jv) ← atomicModifyIORef' (ucStatRNGRef (toUnitCombatCapability env)) $ \g →
                        let (u, g')  = randomR (-0.3, 0.3 ∷ Float) g
                            (v, g'') = randomR (-0.3, 0.3 ∷ Float) g'
                        in (g'', (u, v))
                    forM mInst $ \inst → do
                        gid ← atomicModifyIORef' (wsGroundItemsRef ws) $
                            spawnGroundItem inst
                                (fromIntegral gx + 0.5 + ju)
                                (fromIntegral gy + 0.5 + jv)
                        pure (name, gid)

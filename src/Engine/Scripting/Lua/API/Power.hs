{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the power-node registry (#358) and its connectivity
--   + energy balance (#360). power.placeNode pops a solar_panel/
--   high_voltage_battery item out of a unit's inventory and turns it
--   into a placed, persistent power node — mirroring the portal's
--   instant-build path (building.spawn with bdBuildWork = 0) but sourced
--   from an item instead of being free. getNode / getNodeForBuilding /
--   listNodes report each node's own role + parameters (+ current charge
--   for storage nodes); listNetworks / getNetworkForNode report the live
--   connected-component view — which nodes share a wired network and
--   that network's current generation/drain/stored/capacity/powered
--   status (Power.Network). isBuildingPowered (#361) answers the
--   gating question a requires_power workshop cares about: is this
--   BUILDING (not a registry node) currently allowed to run.
module Engine.Scripting.Lua.API.Power
    ( powerIsPlaceableFn
    , powerPlaceNodeFn
    , powerGetNodeFn
    , powerGetNodeForBuildingFn
    , powerListNodesFn
    , powerListNetworksFn
    , powerGetNetworkForNodeFn
    , powerIsBuildingPoweredFn
    , powerIsStationPoweredForRecipeFn
    , isBuildingPowered
    , isRecipePoweredAt
    ) where

import UPrelude
import Data.List (find)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import World.Time.Types (worldTimeToSunAngle)
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData)
import qualified Engine.Core.Queue as Q
import Building.Types
import Building.Command.Types (BuildingCommand(..))
import Building.Placement (canPlaceAt, PlacementResult(..))
import Craft.Types (RecipeDef(..), lookupRecipe)
import Unit.Types (UnitId(..), UnitManager(..), UnitInstance(..))
import Unit.Pathing.Cost (lookupTerrainZ)
import Item.Types (ItemInstance(..))
import Power.Types
import Power.Network (wireTilesOn, positionsOf, computeSnapshots, consumersOn,
                      activeCraftConsumersOn, combineConsumers)
import qualified Power.Network as PN

-- | power.isPlaceable(itemDefName) → bool. Lets a caller (the build
--   tool's placement click) decide whether a def routes through
--   power.placeNode (item-consuming) or the free building.spawn path,
--   without hardcoding the placeable-item name list a second time in
--   Lua — the single source of truth stays 'powerNodeSpecFor'.
powerIsPlaceableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerIsPlaceableFn _env = do
    nameArg ← Lua.tostring 1
    let placeable = case nameArg of
            Nothing → False
            Just bs → case powerNodeSpecFor (TE.decodeUtf8Lenient bs) of
                Just _  → True
                Nothing → False
    Lua.pushboolean placeable
    return 1

-- | power.placeNode(uid, itemDefName, gx, gy [, pageId]) → nodeId,
--   buildingId on success, or nil, reason on failure. Pops one
--   matching item instance out of the unit's inventory FIRST, then
--   validates placement — a rejected placement splices the item back
--   at its original index (mirrors unit.transferItemToBuilding's
--   rollback). An explicit pageId behaves like building.spawn's (pins
--   the target page instead of the active world, #76).
powerPlaceNodeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerPlaceNodeFn env = do
    uidArg  ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    xArg    ← Lua.tointeger 3
    yArg    ← Lua.tointeger 4
    pageArg ← Lua.tostring 5
    case (uidArg, nameArg, xArg, yArg) of
        (Just nU, Just nameBS, Just x, Just y) → do
            let uid     = UnitId (fromIntegral nU)
                defName = TE.decodeUtf8Lenient nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            result ← Lua.liftIO $ case powerNodeSpecFor defName of
                Nothing → pure (Left "not a placeable power item")
                Just (role, param) → do
                    mTarget ← case pageArg of
                        Just pidBS → do
                            let pid = WorldPageId (TE.decodeUtf8 pidBS)
                            wm ← readIORef (worldManagerRef env)
                            pure $ (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                        Nothing → activeWorldPage env
                    case mTarget of
                        Nothing → pure (Left "no active world")
                        Just (pid, ws) →
                            placeNodeOn env ws pid defName uid gx gy role param
            case result of
                Right (PowerNodeId n, BuildingId b) → do
                    Lua.pushinteger (fromIntegral n)
                    Lua.pushinteger (fromIntegral b)
                    return 2
                Left err → do
                    Lua.pushnil
                    Lua.pushstring (TE.encodeUtf8 err)
                    return 2
        _ → do
            Lua.pushnil
            Lua.pushstring "power.placeNode: expected (uid, itemDefName,\
                           \ gx, gy [, pageId])"
            return 2

-- | The pop → validate → (rollback | commit) core, isolated so the Lua
--   glue above only handles argument marshalling.
placeNodeOn ∷ EngineEnv → WorldState → WorldPageId → Text → UnitId → Int → Int
            → PowerRole → Float → IO (Either Text (PowerNodeId, BuildingId))
placeNodeOn env ws pid defName uid gx gy role param = do
    mPopped ← atomicModifyIORef' (unitManagerRef env) $ \um →
        case HM.lookup uid (umInstances um) of
            Nothing → (um, Nothing)
            Just u  → case popItemByName defName (uiInventory u) of
                Nothing → (um, Nothing)
                Just (item, ix, newInv) →
                    let u' = u { uiInventory = newInv }
                    in ( um { umInstances = HM.insert uid u' (umInstances um) }
                       , Just (item, ix) )
    case mPopped of
        Nothing → pure (Left ("unit has no " <> defName))
        Just (item, ix) → do
            bm  ← readIORef (buildingManagerRef env)
            wtd ← readIORef (wsTilesRef ws)
            case HM.lookup defName (bmDefs bm) of
                Nothing → do
                    rollback item ix
                    pure (Left ("no building def for " <> defName))
                Just def →
                    case canPlaceAt
                            (bm { bmInstances =
                                    buildingsOnPage pid (bmInstances bm) })
                            wtd def gx gy of
                        NotPlaceable reason → do
                            rollback item ix
                            pure (Left reason)
                        Placeable → do
                            let gz = floorZAt wtd gx gy
                            bid ← atomicModifyIORef'
                                    (buildingManagerRef env) $ \bm' →
                                        let (bid', bm'') = nextBuildingId bm'
                                        in (bm'', bid')
                            Q.writeQueue (buildingQueue env) $
                                BuildingSpawn bid defName gx gy gz pid
                            nid ← atomicModifyIORef' (wsPowerNodesRef ws) $
                                addPowerNode bid role param
                            pure (Right (nid, bid))
  where
    -- Splice the popped instance back at its ORIGINAL index — list
    -- order is gameplay/UI-visible (unit.getInventory), so a rejected
    -- placement must leave the unit's inventory exactly as it was.
    rollback item ix = atomicModifyIORef' (unitManagerRef env) $ \um →
        case HM.lookup uid (umInstances um) of
            Nothing → (um, ())
            Just u  →
                let u' = u { uiInventory = insertAt ix item (uiInventory u) }
                in (um { umInstances = HM.insert uid u' (umInstances um) }, ())

-- | Terrain Z at the anchor tile. Falls back to 0 if the chunk isn't
--   loaded — shouldn't happen since canPlaceAt already verified,
--   defensive. Mirrors Buildings.hs's private helper of the same name.
floorZAt ∷ WorldTileData → Int → Int → Int
floorZAt wtd gx gy = case lookupTerrainZ wtd gx gy of
    Just z  → z
    Nothing → 0

-- | Pop the first item instance matching @name@, reporting its
--   0-based index for a possible rollback. Mirrors Units.hs's private
--   popFirstByNameIx.
popItemByName ∷ Text → [ItemInstance] → Maybe (ItemInstance, Int, [ItemInstance])
popItemByName = go 0
  where
    go _ _    [] = Nothing
    go i name (x:xs)
        | iiDefName x ≡ name = Just (x, i, xs)
        | otherwise          = (\(it, j, rest) → (it, j, x : rest))
                                <$> go (i + 1) name xs

-- | Insert @x@ at index @i@, appending if the list shrank under a
--   concurrent edit between pop and rollback. Mirrors Units.hs's
--   private insertAt.
insertAt ∷ Int → a → [a] → [a]
insertAt i x xs = let (pre, post) = splitAt i xs in pre ++ x : post

-- | power.getNode(nodeId) → table | nil.
powerGetNodeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerGetNodeFn env = do
    idArg ← Lua.tointeger 1
    mNode ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) → do
                    nodes ← readIORef (wsPowerNodesRef ws)
                    return (lookupPowerNode (PowerNodeId (fromIntegral n)) nodes)
    case mNode of
        Just node → pushNode node >> return 1
        Nothing   → Lua.pushnil >> return 1

-- | power.getNodeForBuilding(bid) → table | nil.
powerGetNodeForBuildingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerGetNodeForBuildingFn env = do
    idArg ← Lua.tointeger 1
    mNode ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) → do
                    nodes ← readIORef (wsPowerNodesRef ws)
                    return (nodeForBuilding (BuildingId (fromIntegral n)) nodes)
    case mNode of
        Just node → pushNode node >> return 1
        Nothing   → Lua.pushnil >> return 1

-- | power.listNodes() → array of node tables on the active world,
--   oldest first.
powerListNodesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerListNodesFn env = do
    nodeList ← Lua.liftIO $ do
        mPage ← activeWorldPage env
        case mPage of
            Nothing      → return []
            Just (_, ws) → allNodes ⊚ readIORef (wsPowerNodesRef ws)
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] nodeList) $ \(i, node) → do
        pushNode node
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | Push one node as a Lua table: { id, building, role, peakWatts,
--   capacityWh, storedWh }. role is "source" | "storage". storedWh
--   (#360) is always 0 for a source node.
pushNode ∷ PowerNode → Lua.LuaE Lua.Exception ()
pushNode node = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                   >> Lua.setfield (-2) k
    putI "id"       (unPowerNodeId (pnId node))
    putI "building" (unBuildingId (pnBuilding node))
    Lua.pushstring (roleText (pnRole node))
    Lua.setfield (-2) "role"
    putN "peakWatts"  (pnPeakWatts node)
    putN "capacityWh" (pnCapacityWh node)
    putN "storedWh"   (pnStoredWh node)
  where
    roleText PowerSource  = "source"
    roleText PowerStorage = "storage"

-- | The full live consumer demand on a page: #361's always-on
--   'bdPowerDrain' buildings unioned with #590's job-dependent active
--   craft-bill load — every network status computation needs BOTH
--   folded in, or a busy crafting station's demand could hide behind an
--   under-reported Brownout.
liveConsumersOn ∷ EngineEnv → WorldPageId → Double → BuildingManager
               → WorldState → IO (HM.HashMap BuildingId ((Int, Int), Float))
liveConsumersOn env pageId now bm ws = do
    rm    ← readIORef (recipeManagerRef env)
    bills ← readIORef (wsCraftBillsRef ws)
    pure $ combineConsumers (consumersOn pageId now bm)
                             (activeCraftConsumersOn pageId now bm rm bills)

-- | Gather the active world's current network snapshots (#360):
--   connectivity + generation/drain/stored/capacity/status, recomputed
--   live from this instant's sun angle, folding in every requires_power
--   building's drain (#361) plus every active craft job's draw (#590)
--   via 'liveConsumersOn'.
activeNetworkSnapshots ∷ EngineEnv → IO [PN.PowerNetworkSnapshot]
activeNetworkSnapshots env = do
    mPage ← activeWorldPage env
    case mPage of
        Nothing → pure []
        Just (pageId, ws) → do
            nodes ← readIORef (wsPowerNodesRef ws)
            wt    ← readIORef (wsTimeRef ws)
            td    ← readIORef (wsTilesRef ws)
            bm    ← readIORef (buildingManagerRef env)
            now   ← readIORef (gameTimeRef env)
            let sunAngle   = worldTimeToSunAngle wt
                wireTiles  = wireTilesOn td
                positions  = positionsOf pageId bm nodes
            consumers ← liveConsumersOn env pageId now bm ws
            pure (computeSnapshots sunAngle HM.empty wireTiles nodes
                                    positions consumers)

-- | Whether a building's OWN power requirement (#361), if any, is
--   currently met. A building whose def has no power_drain (≤ 0) is
--   trivially always powered — this is only meaningful to ask about an
--   always-on consumer (a hypothetical future device; no shipped/craft
--   building sets this any more, see #590). Otherwise: false if the
--   building doesn't exist, isn't Built, or its network (if it has one
--   at all) is in Brownout; true once wired to a network whose
--   generation/storage currently covers the FULL demand (including any
--   concurrently active craft jobs on the same network). Crafting/
--   repair should use 'isRecipePoweredAt' instead — a station with no
--   'bdPowerDrain' of its own is trivially "powered" here regardless of
--   what its recipes demand.
isBuildingPowered ∷ EngineEnv → BuildingId → IO Bool
isBuildingPowered env bid = do
    bm ← readIORef (buildingManagerRef env)
    case HM.lookup bid (bmInstances bm) of
        Nothing → pure False
        Just inst → case HM.lookup (biDefName inst) (bmDefs bm) of
            Nothing  → pure False
            Just def
                | bdPowerDrain def ≤ 0 → pure True
                | otherwise → do
                    wm ← readIORef (worldManagerRef env)
                    case lookup (biPage inst) (wmWorlds wm) of
                        Nothing → pure False
                        Just ws → do
                            nodes ← readIORef (wsPowerNodesRef ws)
                            wt    ← readIORef (wsTimeRef ws)
                            td    ← readIORef (wsTilesRef ws)
                            now   ← readIORef (gameTimeRef env)
                            consumers ← liveConsumersOn env (biPage inst) now bm ws
                            let sunAngle   = worldTimeToSunAngle wt
                                wireTiles  = wireTilesOn td
                                positions  = positionsOf (biPage inst) bm nodes
                                nets = computeSnapshots sunAngle HM.empty
                                            wireTiles nodes positions consumers
                            pure $ case find (elem bid . PN.pnwConsumerIds) nets of
                                Just net → PN.pnwStatus net ≡ PN.Powered
                                Nothing  → False

-- | Whether a STATION can currently satisfy a recipe's power demand
--   (#590) — the job-dependent gate 'Craft.Execute.Execute.validateStation'
--   and the craft_job AI's per-tick working-phase check both use. A
--   recipe with @drawW ≤ 0@ (the default — most recipes) is always
--   satisfied, at any station, wired or not (requirement #3). A
--   positive-drawing recipe needs its station's tile on a network whose
--   generation/storage covers the FULL live demand — the existing
--   demand from every other always-on/active-job consumer PLUS this
--   call's own @drawW@ (folded in synthetically so a bare, not-yet-
--   claimed 'craft.executeAt'/'repair.repairAt' call is judged
--   correctly, and a bill that already registers this exact station+
--   load via 'activeCraftConsumersOn' just gets overwritten with the
--   same number). False if the station doesn't exist, isn't on any
--   power network at all (no source/storage reachable), or the network
--   is in Brownout.
isRecipePoweredAt ∷ EngineEnv → BuildingId → Float → IO Bool
isRecipePoweredAt env bid drawW
    | drawW ≤ 0 = pure True
    | otherwise = do
        bm ← readIORef (buildingManagerRef env)
        case HM.lookup bid (bmInstances bm) of
            Nothing → pure False
            Just inst → do
                wm ← readIORef (worldManagerRef env)
                case lookup (biPage inst) (wmWorlds wm) of
                    Nothing → pure False
                    Just ws → do
                        nodes ← readIORef (wsPowerNodesRef ws)
                        wt    ← readIORef (wsTimeRef ws)
                        td    ← readIORef (wsTilesRef ws)
                        now   ← readIORef (gameTimeRef env)
                        base  ← liveConsumersOn env (biPage inst) now bm ws
                        let sunAngle  = worldTimeToSunAngle wt
                            wireTiles = wireTilesOn td
                            positions = positionsOf (biPage inst) bm nodes
                            tile      = (biAnchorX inst, biAnchorY inst)
                            consumers = HM.insert bid (tile, drawW) base
                            nets = computeSnapshots sunAngle HM.empty
                                        wireTiles nodes positions consumers
                        pure $ case find (elem bid . PN.pnwConsumerIds) nets of
                            Just net → PN.pnwStatus net ≡ PN.Powered
                            Nothing  → False

-- | power.isBuildingPowered(bid) → bool (#361).
powerIsBuildingPoweredFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerIsBuildingPoweredFn env = do
    idArg ← Lua.tointeger 1
    ok ← case idArg of
        Nothing → return False
        Just n  → Lua.liftIO $ isBuildingPowered env (BuildingId (fromIntegral n))
    Lua.pushboolean ok
    return 1

-- | power.isStationPoweredForRecipe(bid, recipeId) → bool (#590). The
--   job-dependent counterpart to isBuildingPowered: looks the recipe up
--   itself (an unknown recipe id is trivially "unpowered" — false, same
--   as validateStation's own "unknown recipe" refusal reaching it via a
--   different error) and delegates to isRecipePoweredAt with its
--   rdPowerDraw. Unlike isBuildingPowered, this is meaningful for ANY
--   station regardless of the building def's own bdPowerDrain.
powerIsStationPoweredForRecipeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerIsStationPoweredForRecipeFn env = do
    bidArg ← Lua.tointeger 1
    ridArg ← Lua.tostring 2
    ok ← case (bidArg, ridArg) of
        (Just b, Just ridBS) → Lua.liftIO $ do
            rm ← readIORef (recipeManagerRef env)
            let rid   = TE.decodeUtf8 ridBS
                drawW = maybe 0 rdPowerDraw (lookupRecipe rid rm)
            isRecipePoweredAt env (BuildingId (fromIntegral b)) drawW
        _ → return False
    Lua.pushboolean ok
    return 1

-- | power.listNetworks() → array of network tables on the active world.
--   Order is incidental (component discovery order) — callers key off
--   nodeIds/building ids, not array position.
powerListNetworksFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerListNetworksFn env = do
    nets ← Lua.liftIO $ activeNetworkSnapshots env
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] nets) $ \(i, net) → do
        pushNetwork net
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | power.getNetworkForNode(nodeId) → table | nil — the network the
--   given node currently attaches to (nil if it isn't wired into one:
--   not adjacent to any wire tile).
powerGetNetworkForNodeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
powerGetNetworkForNodeFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n  → do
            let nid = PowerNodeId (fromIntegral n)
            nets ← Lua.liftIO $ activeNetworkSnapshots env
            case find (elem nid . PN.pnwNodeIds) nets of
                Just net → pushNetwork net >> return 1
                Nothing  → Lua.pushnil >> return 1

-- | Push one network snapshot: { nodeIds = {...}, consumerIds = {...},
--   generationW, drainW, storedWh, capacityWh, powered }. consumerIds
--   (#361) are the requires_power buildings attached to this network —
--   their drain is already folded into drainW.
pushNetwork ∷ PN.PowerNetworkSnapshot → Lua.LuaE Lua.Exception ()
pushNetwork net = do
    Lua.newtable
    let putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                   >> Lua.setfield (-2) k
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] (PN.pnwNodeIds net)) $ \(i, nid) → do
        Lua.pushinteger (fromIntegral (unPowerNodeId nid))
        Lua.rawseti (-2) (fromIntegral i)
    Lua.setfield (-2) "nodeIds"
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] (PN.pnwConsumerIds net)) $ \(i, bid) → do
        Lua.pushinteger (fromIntegral (unBuildingId bid))
        Lua.rawseti (-2) (fromIntegral i)
    Lua.setfield (-2) "consumerIds"
    putN "generationW" (PN.pnwGenerationW net)
    putN "drainW"      (PN.pnwDrainW net)
    putN "storedWh"    (PN.pnwStoredWh net)
    putN "capacityWh"  (PN.pnwCapacityWh net)
    Lua.pushboolean (PN.pnwStatus net ≡ PN.Powered)
    Lua.setfield (-2) "powered"

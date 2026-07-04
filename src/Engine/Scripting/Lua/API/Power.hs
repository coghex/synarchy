{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the power-node registry (#358). power.placeNode
--   pops a solar_panel/high_voltage_battery item out of a unit's
--   inventory and turns it into a placed, persistent power node —
--   mirroring the portal's instant-build path (building.spawn with
--   bdBuildWork = 0) but sourced from an item instead of being free.
--   The read-only queries (getNode / getNodeForBuilding / listNodes)
--   are the #358 "reports its role + parameters" surface. Network
--   attachment / energy balance are #359/#360, not here.
module Engine.Scripting.Lua.API.Power
    ( powerIsPlaceableFn
    , powerPlaceNodeFn
    , powerGetNodeFn
    , powerGetNodeForBuildingFn
    , powerListNodesFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData)
import qualified Engine.Core.Queue as Q
import Building.Types
import Building.Command.Types (BuildingCommand(..))
import Building.Placement (canPlaceAt, PlacementResult(..))
import Unit.Types (UnitId(..), UnitManager(..), UnitInstance(..))
import Unit.Pathing.Cost (lookupTerrainZ)
import Item.Types (ItemInstance(..))
import Power.Types

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
--   capacityWh }. role is "source" | "storage".
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
  where
    roleText PowerSource  = "source"
    roleText PowerStorage = "storage"

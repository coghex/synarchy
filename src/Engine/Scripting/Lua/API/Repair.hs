{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the repair policy layer (#301): the station/axis
--   model on top of the `unit.repairItem` primitive (#300). Repair
--   flows are ordinary RecipeDef entries (data/recipes/repair.yaml)
--   tagged with `rdRepairAxis` — "condition" (restored at the furnace,
--   station "repair_condition") or "sharpness" (honed at the
--   workbench, station "repair_sharpness") — so they reuse the same
--   catalogue, station-gating (Engine.Scripting.Lua.API.Craft.
--   validateStation), and all-or-nothing ingredient consumption
--   (Craft.Execute.consumeIngredients) as ordinary crafts, but target
--   an EXISTING item instance instead of producing new ones.
--
--   repair.repairAt always restores the targeted axis fully to 100 (a
--   broken, 0-condition item repairs the same way as a lightly worn
--   one) — the "how much per action" question the epic (#299/#301)
--   left open, settled here as full-restore-per-visit for simplicity;
--   partial/metered restoration is a future refinement, not a
--   rebalance of what's built here. An axis already at 100 refuses
--   before any cost is consumed, so an AI (#302) can't waste a
--   whetstone honing an already-keen edge.
module Engine.Scripting.Lua.API.Repair
    ( repairGetFn
    , repairGetNamesFn
    , repairAtFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Craft.Types (RecipeManager(..), RecipeDef(..), lookupRecipe,
                    RepairAxis(..), repairAxisName)
import Craft.Execute (consumeIngredients)
import Engine.Scripting.Lua.API.Craft (pushRecipe, validateStation)
import Item.Types (ItemInstance(..), ItemManager)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import Building.Types (BuildingId(..))
import Engine.Scripting.Lua.API.Units (applyRepairToUnit, findHeldItemById)

-- | repair.get(id) → table | nil. Same shape as craft.get, restricted
--   to recipes tagged with a repair axis.
repairGetFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
repairGetFn env = do
    idArg ← Lua.tostring 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just idBS → do
            let key = TE.decodeUtf8 idBS
            mDef ← Lua.liftIO $ do
                m ← readIORef (recipeManagerRef env)
                pure (lookupRecipe key m)
            case mDef of
                Just d | rdRepairAxis d ≢ Nothing → pushRecipe d >> return 1
                _ → Lua.pushnil >> return 1

-- | repair.getNames() → array of repair-tagged recipe ids only.
repairGetNamesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
repairGetNamesFn env = do
    m ← Lua.liftIO $ readIORef (recipeManagerRef env)
    let names = [ rdId d | d ← HM.elems (rmDefs m), rdRepairAxis d ≢ Nothing ]
    Lua.newtable
    forM_ (zip [1..] names) $ \(i, n) → do
        Lua.pushstring (TE.encodeUtf8 n)
        Lua.rawseti (-2) i
    return 1

-- | repair.repairAt(uid, recipeId, instanceId, bid) → table | nil, err?.
--   The station-gated repair verb: `recipeId` must be a repair-tagged
--   recipe, `bid` must be a Built station on the unit's page offering
--   that recipe's station operation with the unit adjacent (same gate
--   as craft.executeAt), and the targeted instance (searched across
--   inventory/equipment/accessories, same reach as unit.repairItem)
--   must not already be at 100 on the recipe's axis. On success,
--   consumes the recipe's inputs/fuel from the unit's inventory
--   all-or-nothing and restores the axis fully to 100, returning the
--   same { defName, condition, sharpness, conditionApplied,
--   sharpnessApplied } shape as unit.repairItem. On refusal, returns
--   nil plus a reason and touches nothing.
repairAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
repairAtFn env = do
    idArg    ← Lua.tointeger 1
    ridArg   ← Lua.tostring 2
    instArg  ← Lua.tointeger 3
    bidArg   ← Lua.tointeger 4
    case (idArg, ridArg, instArg, bidArg) of
        (Just n, Just ridBS, Just iidI, Just b) → do
            let uid = UnitId (fromIntegral n)
                rid = TE.decodeUtf8 ridBS
                iid = fromIntegral iidI ∷ Word64
                bid = BuildingId (fromIntegral b)
            result ← Lua.liftIO $ runRepairAt env uid rid iid bid
            case result of
                Left err → do
                    Lua.pushnil
                    Lua.pushstring (TE.encodeUtf8 err)
                    return 2
                Right (defName, cond1, sharp1, cApp, sApp) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 defName)
                    Lua.setfield (-2) "defName"
                    Lua.pushnumber (Lua.Number (realToFrac cond1))
                    Lua.setfield (-2) "condition"
                    Lua.pushnumber (Lua.Number (realToFrac sharp1))
                    Lua.setfield (-2) "sharpness"
                    Lua.pushnumber (Lua.Number (realToFrac cApp))
                    Lua.setfield (-2) "conditionApplied"
                    Lua.pushnumber (Lua.Number (realToFrac sApp))
                    Lua.setfield (-2) "sharpnessApplied"
                    return 1
        _ → do
            Lua.pushnil
            Lua.pushstring (TE.encodeUtf8
                ("repair.repairAt: expected (uid, recipeId, instanceId, buildingId)" ∷ Text))
            return 2

runRepairAt ∷ EngineEnv → UnitId → Text → Word64 → BuildingId
            → IO (Either Text (Text, Float, Float, Float, Float))
runRepairAt env uid rid iid bid = do
    rm ← readIORef (recipeManagerRef env)
    case lookupRecipe rid rm of
        Nothing → return (Left ("unknown recipe " <> rid))
        Just recipe → case rdRepairAxis recipe of
            Nothing → return (Left (rid <> " is not a repair recipe"))
            Just axis → do
                -- Repairs aren't bill-driven (no Craft.Bills entry ever
                -- registers a repair job as an active consumer), so
                -- there's no bill to exclude here — Nothing is always
                -- correct, unlike craft.executeAt's job.billId case.
                gate ← validateStation env Nothing uid rid bid
                case gate of
                    Left err → return (Left err)
                    Right () → do
                        itemMgr ← readIORef (itemManagerRef env)
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            applyRepairAt axis recipe iid itemMgr uid um

-- | The pure atomic step: find the targeted instance, refuse if
--   already full on the recipe's axis, consume the recipe's demands
--   all-or-nothing, then restore that axis to 100. All three checks
--   run before any mutation, so a refusal at any stage leaves the unit
--   manager untouched.
applyRepairAt ∷ RepairAxis → RecipeDef → Word64 → ItemManager → UnitId
              → UnitManager
              → (UnitManager, Either Text (Text, Float, Float, Float, Float))
applyRepairAt axis recipe iid itemMgr uid um = case HM.lookup uid (umInstances um) of
    Nothing → (um, Left "no such unit")
    Just u → case findHeldItemById iid u of
        Nothing → (um, Left "no such item instance")
        Just it →
            let current = axisValue axis it
            in if current ≥ 100
                then (um, Left ("already at full " <> repairAxisName axis))
                else case consumeIngredients recipe (uiInventory u) of
                    Left err → (um, Left err)
                    Right inv' →
                        let u1 = u { uiInventory = inv' }
                            delta = 100 - current
                            (condD, sharpD) = case axis of
                                RepairCondition → (delta, 0)
                                RepairSharpness → (0, delta)
                        in case applyRepairToUnit iid condD sharpD itemMgr u1 of
                            Nothing → (um, Left "no such item instance")
                            Just (u2, r) →
                                ( um { umInstances = HM.insert uid u2
                                                               (umInstances um) }
                                , Right r )
  where
    axisValue RepairCondition = iiCondition
    axisValue RepairSharpness = iiSharpness

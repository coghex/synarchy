{-# LANGUAGE Strict #-}
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
--
--   Narrowed to the @content-registries@ capability (#890, epic #537),
--   and since #1896 to its READ-ONLY view: the recipe + item catalogues
--   are reached only through 'ContentRegistriesViewCapability', whose
--   handles this module cannot write even in principle. It is a pure
--   reader of both — @repair.repairAt@ mutates the unit manager, never
--   a registry. The one @units-buildings-combat@
--   field this module writes (the unit manager) is passed in as the bare
--   'IORef' it is. 'repairAtFn' still takes an 'EngineEnv', but purely
--   as the opaque token the station gate ('validateStation') demands —
--   this module dereferences no 'EngineEnv' field itself.
--
--   That parameter did __not__ go away when @units-buildings-combat@
--   migrated (#895\/#896, SS7.5), as an earlier version of this comment
--   predicted it would. The gate is not a single-capability consumer:
--   it composes FOUR already-landed records
--   ('ContentRegistriesViewCapability' for the recipe, 'BuildingCapability'
--   for the station, 'UnitCombatCapability' for the crafter,
--   'WorldSimCapability' for the game clock) and then calls
--   @Engine.Scripting.Lua.API.Power.isRecipePoweredAt@, which composes
--   two of the same four again plus per-page world state. An 'EngineEnv'
--   is what those projections are taken FROM, so the token stays until
--   something narrows the gate itself — which is not any SS7 row's
--   scope, since no capability owns it.
module Engine.Scripting.Lua.API.Repair
    ( repairGetFn
    , repairGetNamesFn
    , repairAtFn
    , applyRepairAt
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (IORef, atomicModifyIORef')
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..))
import Craft.Types (RecipeManager(..), RecipeDef(..), lookupRecipe,
                    RepairAxis(..), repairAxisName)
import Craft.Execute (consumeIngredients)
import Engine.Scripting.Lua.API.Craft (pushRecipe, validateStation)
import Item.Types (ItemInstance(..), ItemManager)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import Building.Types (BuildingId(..))
import Engine.Scripting.Lua.API.Units
    (applyRepairToUnit, findHeldItemById, repairDeltasFinite)

-- | repair.get(id) → table | nil. Same shape as craft.get, restricted
--   to recipes tagged with a repair axis.
repairGetFn ∷ ContentRegistriesViewCapability
            → Lua.LuaE Lua.Exception Lua.NumResults
repairGetFn regs = do
    idArg ← Lua.tostring 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just idBS → do
            let key = TE.decodeUtf8Lenient idBS
            mDef ← Lua.liftIO $ do
                m ← readReadOnlyRef (crvRecipeManagerRef regs)
                pure (lookupRecipe key m)
            case mDef of
                Just d | rdRepairAxis d ≢ Nothing → pushRecipe d >> return 1
                _ → Lua.pushnil >> return 1

-- | repair.getNames() → array of repair-tagged recipe ids only.
repairGetNamesFn ∷ ContentRegistriesViewCapability
                 → Lua.LuaE Lua.Exception Lua.NumResults
repairGetNamesFn regs = do
    m ← Lua.liftIO $ readReadOnlyRef (crvRecipeManagerRef regs)
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
--   nil plus a reason and touches nothing — including the #1732 case
--   where the axis restoration this verb DERIVES is not finite.
repairAtFn ∷ ContentRegistriesViewCapability → IORef UnitManager → EngineEnv
           → Lua.LuaE Lua.Exception Lua.NumResults
repairAtFn regs umRef env = do
    idArg    ← Lua.tointeger 1
    ridArg   ← Lua.tostring 2
    instArg  ← Lua.tointeger 3
    bidArg   ← Lua.tointeger 4
    case (idArg, ridArg, instArg, bidArg) of
        (Just n, Just ridBS, Just iidI, Just b) → do
            let uid = UnitId (fromIntegral n)
                rid = TE.decodeUtf8Lenient ridBS
                iid = fromIntegral iidI ∷ Word64
                bid = BuildingId (fromIntegral b)
            result ← Lua.liftIO $
                runRepairAt regs umRef env uid rid iid bid
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

runRepairAt ∷ ContentRegistriesViewCapability → IORef UnitManager → EngineEnv
            → UnitId → Text → Word64 → BuildingId
            → IO (Either Text (Text, Float, Float, Float, Float))
runRepairAt regs umRef env uid rid iid bid = do
    rm ← readReadOnlyRef (crvRecipeManagerRef regs)
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
                        itemMgr ← readReadOnlyRef (crvItemManagerRef regs)
                        atomicModifyIORef' umRef $ \um →
                            applyRepairAt axis recipe iid itemMgr uid um

-- | The pure atomic step: find the targeted instance, refuse if
--   already full on the recipe's axis, refuse if the restoration this
--   step DERIVES is not finite, consume the recipe's demands
--   all-or-nothing, then restore that axis to 100. All four checks run
--   before any mutation, so a refusal at any stage leaves the unit
--   manager untouched.
--
--   The finiteness check (#1732) is the shared core's rule reaching
--   this entry point too, and it is stated HERE rather than left to
--   'applyRepairToUnit' for two reasons. It must run BEFORE
--   'consumeIngredients', so a refusal costs the unit nothing; and it
--   names what actually went wrong, where the core's 'Nothing' is
--   indistinguishable from a missing instance. @delta@ is @100 -
--   current@, so it is non-finite exactly when the stored axis is —
--   @current ≥ 100@ is False for NaN, which is what lets such a value
--   reach this point at all. This validates the DERIVED delta only; no
--   other stored item scalar is inspected here.
applyRepairAt ∷ RepairAxis → RecipeDef → Word64 → ItemManager → UnitId
              → UnitManager
              → (UnitManager, Either Text (Text, Float, Float, Float, Float))
applyRepairAt axis recipe iid itemMgr uid um = case HM.lookup uid (umInstances um) of
    Nothing → (um, Left "no such unit")
    Just u → case findHeldItemById iid u of
        Nothing → (um, Left "no such item instance")
        Just it →
            let current = axisValue axis it
                delta   = 100 - current
                (condD, sharpD) = case axis of
                    RepairCondition → (delta, 0)
                    RepairSharpness → (0, delta)
            in if current ≥ 100
                then (um, Left ("already at full " <> repairAxisName axis))
                else if not (repairDeltasFinite condD sharpD)
                  then (um, Left ("non-finite " <> repairAxisName axis
                                  <> " repair delta"))
                  else case consumeIngredients recipe (uiInventory u) of
                    Left err → (um, Left err)
                    Right inv' →
                        let u1 = u { uiInventory = inv' }
                        in case applyRepairToUnit iid condD sharpD itemMgr u1 of
                            Nothing → (um, Left "no such item instance")
                            Just (u2, r) →
                                ( um { umInstances = HM.insert uid u2
                                                               (umInstances um) }
                                , Right r )
  where
    axisValue RepairCondition = iiCondition
    axisValue RepairSharpness = iiSharpness

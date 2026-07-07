{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | craft.execute runs one craft against a unit's inventory — verify
--   inputs + fuel, consume them, produce the outputs. craft.executeAt
--   is the station-aware variant (#326): same consumption, gated on a
--   Built work station that offers the recipe's station kind with the
--   unit adjacent. validateStation is shared with
--   Engine.Scripting.Lua.API.Repair's repairAt gate.
module Engine.Scripting.Lua.API.Craft.Execute
    ( craftExecuteFn
    , craftExecuteAtFn
    , validateStation
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Craft.Types
import Craft.Execute (consumeIngredients, craftQuality)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemInstance(..), ItemContainer(..),
                   lookupItemDef)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..),
                       BuildingActivity(..), BuildingManager(..),
                       currentActivity, footprintDist)
import Engine.Scripting.Lua.API.Power (isBuildingPowered)

-- | craft.execute(uid, recipeId) → ok, idsOrErr. Runs one craft
--   against the unit's TOP-LEVEL inventory: knowledge gate, then
--   all-or-nothing consumption of inputs + fuel, then the outputs are
--   appended — at crafter-derived quality when the recipe is
--   skill-tagged (#343). Success returns true plus the array of the
--   freshly created outputs' instance ids (see pushCraftResult);
--   failure returns false plus a reason, inventory untouched.
craftExecuteFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftExecuteFn env = do
    idArg  ← Lua.tointeger 1
    ridArg ← Lua.tostring 2
    case (idArg, ridArg) of
        (Just n, Just ridBS) → do
            let uid = UnitId (fromIntegral n)
                rid = TE.decodeUtf8 ridBS
            result ← Lua.liftIO $ executeCraft env uid rid
            pushCraftResult result
        _ → pushCraftResult (Left "craft.execute: expected (uid, recipeId)")

-- | craft.executeAt(uid, recipeId, bid) → ok, idsOrErr. The
--   station-aware craft (#326): identical semantics to craft.execute
--   (including the created-instance-ids success return), but refused
--   unless `bid` is a BUILT work station on the unit's world page
--   whose def offers the recipe's station kind, with the unit
--   standing on or adjacent to the footprint (Chebyshev ≤ 1).
--   craft.execute stays station-blind (tests / debug console); the
--   craft AI (#329) routes through this.
craftExecuteAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftExecuteAtFn env = do
    idArg  ← Lua.tointeger 1
    ridArg ← Lua.tostring 2
    bidArg ← Lua.tointeger 3
    case (idArg, ridArg, bidArg) of
        (Just n, Just ridBS, Just b) → do
            let uid = UnitId (fromIntegral n)
                rid = TE.decodeUtf8 ridBS
                bid = BuildingId (fromIntegral b)
            result ← Lua.liftIO $ do
                gate ← validateStation env uid rid bid
                case gate of
                    Left err → return (Left err)
                    Right () → executeCraft env uid rid
            pushCraftResult result
        _ → pushCraftResult
                (Left "craft.executeAt: expected (uid, recipeId, buildingId)")

-- | The station gate for craft.executeAt (#326). Read-only pre-checks
--   — consumption re-verifies the inventory atomically inside
--   executeCraft, so the only race is a unit stepping away between
--   gate and swap, which at worst completes a craft it was adjacent
--   for one tick earlier (same benign window as the construct AI's
--   building queries).
validateStation ∷ EngineEnv → UnitId → Text → BuildingId
                → IO (Either Text ())
validateStation env uid rid bid = do
    rm      ← readIORef (recipeManagerRef env)
    bm      ← readIORef (buildingManagerRef env)
    um      ← readIORef (unitManagerRef env)
    now     ← readIORef (gameTimeRef env)
    powered ← isBuildingPowered env bid
    return $ do
        recipe ← note ("unknown recipe " <> rid) (lookupRecipe rid rm)
        inst   ← note "no such building" (HM.lookup bid (bmInstances bm))
        def    ← note ("unknown building def " <> biDefName inst)
                      (HM.lookup (biDefName inst) (bmDefs bm))
        u      ← note "no such unit" (HM.lookup uid (umInstances um))
        unless (uiPage u ≡ biPage inst) $
            Left "station is on another world page"
        unless (rdStation recipe `elem` bdOperations def) $
            Left ("station " <> biDefName inst <> " does not offer "
                  <> rdStation recipe)
        unless (currentActivity now inst def ≡ Built) $
            Left "station not built yet"
        -- #361: a requires_power station that isn't wired to a
        -- charged network can't run its recipes. Checked here even
        -- though the craft_job AI already gates its own progress-pour
        -- loop on the same query (unit_ai.lua), so craft.executeAt
        -- stays correct for any other caller (debug console, tests,
        -- a future station type) too.
        unless powered $
            Left ("station " <> biDefName inst <> " has no power")
        let utile = (floor (uiGridX u), floor (uiGridY u))
        unless (footprintDist inst utile ≤ 1) $
            Left "unit not adjacent to station"
  where
    note e = maybe (Left e) Right

-- | Shared return shape for the execute fns. Success is (true,
--   [instanceId…]) — the FRESHLY CREATED outputs' ids, so a caller can
--   target exactly the crafted instances (the craft AI deposits them
--   at the station via unit.dropItemById; a same-def item already in
--   the crafter's inventory is never confused for the new output).
--   Failure is (false, reason).
pushCraftResult ∷ Either Text [Word64]
                → Lua.LuaE Lua.Exception Lua.NumResults
pushCraftResult (Right ids) = do
    Lua.pushboolean True
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] ids) $ \(i, iid) → do
        Lua.pushinteger (fromIntegral iid)
        Lua.rawseti (-2) (fromIntegral i)
    return 2
pushCraftResult (Left err) = do
    Lua.pushboolean False
    Lua.pushstring (TE.encodeUtf8 err)
    return 2

-- | The IO side of craft.execute. Output instances are rolled BEFORE
--   the unit-manager update (rolls + fresh ids are IO), then the
--   verify-consume-produce step is a single atomicModifyIORef' — the
--   same pattern as unit.addItem, so nothing can race between the
--   inventory check and the swap. Rolled outputs are discarded when
--   the craft fails (a few wasted instance ids, never a dupe).
--   Success carries the created outputs' instance ids.
executeCraft ∷ EngineEnv → UnitId → Text → IO (Either Text [Word64])
executeCraft env uid rid = do
    rm ← readIORef (recipeManagerRef env)
    case lookupRecipe rid rm of
        Nothing → return (Left ("unknown recipe " <> rid))
        Just recipe | rdRepairAxis recipe ≢ Nothing →
            return (Left (rid <> " is a repair recipe: use repair.repairAt"))
        Just recipe → do
            im ← readIORef (itemManagerRef env)
            case mapM (resolve im) (rdOutputs recipe) of
                Left err → return (Left err)
                Right outs → do
                    instances ← concat
                        ⊚ mapM (rollOutputs env (rdOutputTemp recipe)) outs
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        applyCraft recipe instances uid um
  where
    resolve im ing = case lookupItemDef (riItem ing) im of
        Nothing  → Left ("unknown output item " <> riItem ing)
        Just def → Right (ing, def)

-- | Verify knowledge + inputs and swap the new inventory in — pure, so
--   it can live inside one atomicModifyIORef' on the unit manager.
--   Skill-tagged recipes (#343) overwrite the outputs' rolled quality
--   with craftQuality from the crafter's skill (absent skill = 0) and
--   the gated knowledge's level, read here inside the same atomic
--   update, so quality can't race a concurrent skill change.
applyCraft ∷ RecipeDef → [ItemInstance] → UnitId → UnitManager
           → (UnitManager, Either Text [Word64])
applyCraft recipe outs uid um = case HM.lookup uid (umInstances um) of
    Nothing → (um, Left "no such unit")
    Just u → case rdKnowledge recipe of
        Just k | not (HM.member k (uiKnowledge u)) →
            (um, Left ("missing knowledge " <> k))
        _ → case consumeIngredients recipe (uiInventory u) of
            Left err   → (um, Left err)
            Right inv' →
                let outs' = case rdSkill recipe of
                        Nothing → outs
                        Just s  →
                            let q = craftQuality
                                     (HM.lookupDefault 0 s (uiSkills u))
                                     ((\k → HM.lookup k (uiKnowledge u))
                                        =≪ rdKnowledge recipe)
                            in map (\o → o { iiQuality = q }) outs
                    u' = u { uiInventory = inv' ⧺ outs' }
                in ( um { umInstances = HM.insert uid u' (umInstances um) }
                   , Right (map iiInstanceId outs') )

-- | Roll one output line into fresh instances. A crafted item is
--   factory-new: condition and sharpness start at 100 (per #325; loot
--   spawns roll condition to simulate age, crafts don't). Quality rolls
--   from the def's spec here as the fallback for skill-less recipes;
--   skill-tagged recipes overwrite it in applyCraft with the crafter-
--   derived craftQuality (#343). Containers spawn at their default
--   fill, same as unit.addItem. @outputTemp@ is the recipe's
--   rdOutputTemp (#344/#346) — Just sets a tracked spawn temperature
--   (hot bar off the smelter, 100 °C fresh-brewed coffee), Nothing
--   spawns at ambient (the historical default).
rollOutputs ∷ EngineEnv → Maybe Float → (RecipeIngredient, ItemDef)
            → IO [ItemInstance]
rollOutputs env outputTemp (ing, def) = replicateM (max 0 (riCount ing)) $ do
    qual ← rollItemSpec (idQualitySpec def) (statRNGRef env)
    wght ← rollItemWeight def (statRNGRef env)
    iid  ← freshItemInstanceId env
    let fill = case idContainer def of
            Just c  → max 0 (min (icCapacity c) (icDefaultFill c))
            Nothing → 0
    pure ItemInstance
        { iiDefName     = riItem ing
        , iiCurrentFill = fill
        , iiQuality     = qual
        , iiCondition   = 100.0
        , iiWeight      = wght
        , iiSharpness   = 100.0
        , iiContents    = []
        , iiInstanceId  = iid
        , iiTemp        = outputTemp
        }

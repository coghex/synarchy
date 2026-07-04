{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the crafting recipe catalogue (#325).
--   engine.loadRecipeYaml loads data/recipes/*.yaml into the
--   RecipeManager (mirrors engine.loadInfectionYaml); craft.get /
--   craft.getNames give read-only access; craft.execute runs one craft
--   against a unit's inventory — verify inputs + fuel, consume them,
--   produce the outputs. craft.executeAt is the station-aware variant
--   (#326): same consumption, gated on a Built work station that
--   offers the recipe's station kind with the unit adjacent. Work is
--   data for the craft AI (#329).
module Engine.Scripting.Lua.API.Craft
    ( loadRecipeYamlFn
    , craftGetFn
    , craftGetNamesFn
    , craftExecuteFn
    , craftExecuteAtFn
    , craftAddBillFn
    , craftCancelBillFn
    , craftGetBillFn
    , craftGetBillsFn
    , craftClaimBillFn
    , craftReleaseBillFn
    , craftAddBillProgressFn
    , craftCompleteBillCycleFn
    , craftSetBillPausedFn
    , craftReorderBillFn
    , pushRecipe
    , validateStation
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Data.List (sortOn)
import Engine.Core.State (EngineEnv(..), freshItemInstanceId,
                          activeWorldPage)
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.YamlRecipes
import Craft.Types
import Craft.Bills
import Craft.Execute (consumeIngredients, craftQuality)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemInstance(..), ItemContainer(..),
                   lookupItemDef)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.State.Types (WorldState(..))
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..),
                       BuildingActivity(..), BuildingManager(..),
                       currentActivity, footprintDist)

-- | engine.loadRecipeYaml(path) — parse a YAML file of recipe defs,
--   register each into the RecipeManager, return the count.
loadRecipeYamlFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadRecipeYamlFn env = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → Lua.pushnumber 0 >> return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadRecipeYaml logger filePath
                total ← foldM (\acc d → do
                    let recipe = RecipeDef
                            { rdId        = ryId d
                            , rdName      = ryName d
                            , rdStation   = ryStation d
                            , rdInputs    = map ingr (ryInputs d)
                            , rdFuel      = ingr ⊚ ryFuel d
                            , rdWork      = ryWork d
                            , rdOutputs   = map ingr (ryOutputs d)
                            , rdKnowledge = ryKnowledge d
                            , rdSkill     = rySkill d
                            , rdRepairAxis = toRepairAxis ⊚ ryRepairAxis d
                            }
                    atomicModifyIORef' (recipeManagerRef env) $ \m →
                        (RecipeManager
                            { rmDefs = HM.insert (ryId d) recipe
                                                 (rmDefs m) }, ())
                    return (acc + 1)
                    ) (0 ∷ Int) defs
                logInfo logger CatAsset $
                    "loadRecipeYaml: loaded " <> T.pack (show total)
                    <> " recipes from " <> T.pack filePath
                return total
            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1
  where
    ingr i = RecipeIngredient { riItem = ryiItem i, riCount = ryiCount i }
    -- Total given Engine.Asset.YamlRecipes' parser already rejects any
    -- repair_axis value but "condition"/"sharpness" — this is the ONE
    -- place raw YAML text becomes a RepairAxis; every consumer past
    -- this point works with the sum type, not a string to compare.
    toRepairAxis "sharpness" = RepairSharpness
    toRepairAxis _           = RepairCondition

-- | Push a RecipeIngredient as a Lua `{ item, count }` table.
pushIngredient ∷ RecipeIngredient → Lua.LuaE Lua.Exception ()
pushIngredient i = do
    Lua.newtable
    Lua.pushstring (TE.encodeUtf8 (riItem i))
    Lua.setfield (-2) "item"
    Lua.pushinteger (fromIntegral (riCount i))
    Lua.setfield (-2) "count"

-- | Push a RecipeDef as a Lua table: { id, name, station, work,
--   knowledge?, skill?, repairAxis?, inputs = [{item,count}…],
--   fuel = {item,count}?, outputs = [{item,count}…] }. Shared by
--   craft.get and repair.get (Engine.Scripting.Lua.API.Repair) so both
--   accessors report the identical shape.
pushRecipe ∷ RecipeDef → Lua.LuaE Lua.Exception ()
pushRecipe d = do
    Lua.newtable
    let putS k v = Lua.pushstring (TE.encodeUtf8 v)
                   >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                   >> Lua.setfield (-2) k
    putS "id"      (rdId d)
    putS "name"    (rdName d)
    putS "station" (rdStation d)
    putN "work"    (rdWork d)
    forM_ (rdKnowledge d)  $ putS "knowledge"
    forM_ (rdSkill d)      $ putS "skill"
    forM_ (rdRepairAxis d) $ \axis → putS "repairAxis" (repairAxisName axis)
    Lua.newtable
    forM_ (zip [1..] (rdInputs d)) $ \(i, ing) → do
        pushIngredient ing
        Lua.rawseti (-2) i
    Lua.setfield (-2) "inputs"
    forM_ (rdFuel d) $ \f → do
        pushIngredient f
        Lua.setfield (-2) "fuel"
    Lua.newtable
    forM_ (zip [1..] (rdOutputs d)) $ \(i, ing) → do
        pushIngredient ing
        Lua.rawseti (-2) i
    Lua.setfield (-2) "outputs"

-- | craft.get(id) → table | nil. Read-only access to one recipe:
--   { id, name, station, work, knowledge?, skill?,
--     inputs = [{item,count}…], fuel = {item,count}?,
--     outputs = [{item,count}…] }.
craftGetFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftGetFn env = do
    idArg ← Lua.tostring 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just idBS → do
            let key = TE.decodeUtf8 idBS
            mDef ← Lua.liftIO $ do
                m ← readIORef (recipeManagerRef env)
                pure (lookupRecipe key m)
            case mDef of
                Nothing → Lua.pushnil >> return 1
                Just d  → pushRecipe d >> return 1

-- | craft.getNames() → array of all loaded recipe ids.
craftGetNamesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftGetNamesFn env = do
    m ← Lua.liftIO $ readIORef (recipeManagerRef env)
    Lua.newtable
    forM_ (zip [1..] (HM.keys (rmDefs m))) $ \(i, k) → do
        Lua.pushstring (TE.encodeUtf8 k)
        Lua.rawseti (-2) i
    return 1

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
    rm  ← readIORef (recipeManagerRef env)
    bm  ← readIORef (buildingManagerRef env)
    um  ← readIORef (unitManagerRef env)
    now ← readIORef (gameTimeRef env)
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
                    instances ← concat ⊚ mapM (rollOutputs env) outs
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

-- Craft bills (#329) -----------------------------------------------
--
-- The bill queue lives per world page (wsCraftBillsRef) and has no
-- world-thread side effects, so every verb below is one synchronous
-- atomicModifyIORef' — claims between two crafters resolve atomically
-- and callers read results back immediately (no queue round-trip).
-- The pure transitions live in Craft.Bills; this layer adds argument
-- decoding, the add-time validation, and the Lua table shape.

-- | craft.addBill(bid, recipeId [, count]) → billId | nil, err.
--   Queue a standing order at a station: run @recipeId@ @count@ times
--   (omitted or < 1 = repeat forever). Validated here so a malformed
--   bill can never enter the queue: the recipe must exist and not be
--   repair-tagged, the station must exist on the active page, and its
--   def must offer the recipe's station operation. The station does
--   NOT need to be Built yet — queueing bills on an under-construction
--   station is fine; the craft AI only works Built ones.
craftAddBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftAddBillFn env = do
    bidArg   ← Lua.tointeger 1
    ridArg   ← Lua.tostring 2
    countArg ← Lua.tointeger 3
    case (bidArg, ridArg) of
        (Just b, Just ridBS) → do
            let bid   = BuildingId (fromIntegral b)
                rid   = TE.decodeUtf8 ridBS
                count = maybe (-1) fromIntegral countArg
            result ← Lua.liftIO $ do
                mPage ← activeWorldPage env
                rm    ← readIORef (recipeManagerRef env)
                bm    ← readIORef (buildingManagerRef env)
                let gate = do
                        (pageId, ws) ← note "no active world" mPage
                        recipe ← note ("unknown recipe " <> rid)
                                      (lookupRecipe rid rm)
                        when (rdRepairAxis recipe ≢ Nothing) $
                            Left (rid <> " is a repair recipe — bills run \
                                         \craft recipes only")
                        inst ← note "no such building"
                                    (HM.lookup bid (bmInstances bm))
                        def  ← note ("unknown building def " <> biDefName inst)
                                    (HM.lookup (biDefName inst) (bmDefs bm))
                        unless (biPage inst ≡ pageId) $
                            Left "station is on another world page"
                        unless (rdStation recipe `elem` bdOperations def) $
                            Left ("station " <> biDefName inst
                                  <> " does not offer " <> rdStation recipe)
                        Right ws
                case gate of
                    Left err → return (Left err)
                    Right ws → do
                        billId ← atomicModifyIORef' (wsCraftBillsRef ws) $
                            addBill bid rid count
                        return (Right billId)
            case result of
                Right (BillId n) → do
                    Lua.pushinteger (fromIntegral n)
                    return 1
                Left err → do
                    Lua.pushnil
                    Lua.pushstring (TE.encodeUtf8 err)
                    return 2
        _ → do
            Lua.pushnil
            Lua.pushstring "craft.addBill: expected (buildingId, recipeId\
                           \ [, count])"
            return 2
  where
    note e = maybe (Left e) Right

-- | craft.cancelBill(billId) → true | false.
craftCancelBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftCancelBillFn env = withBillId env $ \ws billId →
    atomicModifyIORef' (wsCraftBillsRef ws) (removeBill billId)

-- | craft.releaseBill(billId) → true | false. Back to pending; cycle
--   progress is kept (see Craft.Bills.releaseBill).
craftReleaseBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftReleaseBillFn env = withBillId env $ \ws billId →
    atomicModifyIORef' (wsCraftBillsRef ws) (releaseBill billId)

-- | craft.setBillPaused(billId, paused) → true | false. Pausing blocks
--   fresh claims (Craft.Bills.claimAvailable) without touching a
--   claimant already mid-cycle — the #330 panel's pause control.
craftSetBillPausedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftSetBillPausedFn env = do
    idArg     ← Lua.tointeger 1
    pausedArg ← Lua.toboolean 2
    ok ← case idArg of
        Nothing → return False
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        setBillPaused (BillId (fromIntegral n)) pausedArg
    Lua.pushboolean ok
    return 1

-- | craft.reorderBill(billId, "up" | "down") → true | false. Swaps
--   with the immediate neighbour at the same station in the current
--   listing order (Craft.Bills.reorderBill) — the #330 panel's manual
--   reorder control. False at either end of the queue, for an unknown
--   bill, or for any direction string but "up"/"down".
craftReorderBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftReorderBillFn env = do
    idArg  ← Lua.tointeger 1
    dirArg ← Lua.tostring 2
    ok ← case (idArg, dirArg >>= toDirection) of
        (Just n, Just dir) → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        reorderBill dir (BillId (fromIntegral n))
        _ → return False
    Lua.pushboolean ok
    return 1
  where
    toDirection bs = case TE.decodeUtf8 bs of
        "up"   → Just MoveUp
        "down" → Just MoveDown
        _      → Nothing

-- | Shared decode + bool-return shape for the (billId) → bool verbs.
withBillId ∷ EngineEnv → (WorldState → BillId → IO Bool)
           → Lua.LuaE Lua.Exception Lua.NumResults
withBillId env act = do
    idArg ← Lua.tointeger 1
    ok ← case idArg of
        Nothing → return False
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) → act ws (BillId (fromIntegral n))
    Lua.pushboolean ok
    return 1

-- | craft.claimBill(billId, uid, timeout) → true | false. Atomic
--   claim-or-refresh: succeeds when the bill is unclaimed, already
--   ours, or the standing claim is stale (dead claimant, or no refresh
--   within @timeout@ game-seconds — the AI passes its
--   craft_claim_timeout param so the tunable stays Lua-side with the
--   others). Exactly one of two simultaneous claimants wins.
craftClaimBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftClaimBillFn env = do
    idArg      ← Lua.tointeger 1
    uidArg     ← Lua.tointeger 2
    timeoutArg ← Lua.tonumber 3
    ok ← case (idArg, uidArg, timeoutArg) of
        (Just n, Just u, Just tmo) → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return False
                Just (_, ws) → do
                    now ← readIORef (gameTimeRef env)
                    um  ← readIORef (unitManagerRef env)
                    let alive c = HM.member c (umInstances um)
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        claimBill now (realToFrac tmo) alive
                                  (BillId (fromIntegral n))
                                  (UnitId (fromIntegral u))
        _ → return False
    Lua.pushboolean ok
    return 1

-- | craft.addBillProgress(billId, delta) → newProgress | nil. Pours
--   work into the current cycle (clamped to [0, 1]) and returns the
--   post-add progress synchronously — the AI acts on 1.0 by firing
--   craft.executeAt + craft.completeBillCycle itself, so consumption
--   and production stay in the executeAt verb (one authority).
craftAddBillProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftAddBillProgressFn env = do
    idArg    ← Lua.tointeger 1
    deltaArg ← Lua.tonumber 2
    result ← case (idArg, deltaArg) of
        (Just n, Just delta) → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        addBillProgress (BillId (fromIntegral n))
                                        (realToFrac delta)
        _ → return Nothing
    case result of
        Just p  → Lua.pushnumber (Lua.Number (realToFrac p)) >> return 1
        Nothing → Lua.pushnil >> return 1

-- | craft.completeBillCycle(billId) → remaining | nil. One craft
--   cycle done: progress resets, finite counts tick down (the bill is
--   removed at 0), repeat bills return -1 forever.
craftCompleteBillCycleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftCompleteBillCycleFn env = do
    idArg ← Lua.tointeger 1
    result ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) →
                    atomicModifyIORef' (wsCraftBillsRef ws) $
                        completeBillCycle (BillId (fromIntegral n))
    case result of
        Just r  → Lua.pushinteger (fromIntegral r) >> return 1
        Nothing → Lua.pushnil >> return 1

-- | craft.getBill(billId) → table | nil.
craftGetBillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftGetBillFn env = do
    idArg ← Lua.tointeger 1
    mBill ← case idArg of
        Nothing → return Nothing
        Just n  → Lua.liftIO $ do
            mPage ← activeWorldPage env
            case mPage of
                Nothing      → return Nothing
                Just (_, ws) → do
                    bills ← readIORef (wsCraftBillsRef ws)
                    return (lookupBill (BillId (fromIntegral n)) bills)
    case mBill of
        Just bill → pushBill bill >> return 1
        Nothing   → Lua.pushnil >> return 1

-- | craft.getBills([bid]) → array of bill tables on the active world,
--   oldest first — every bill, or one station's queue when @bid@ is
--   given (the #330 station panel's view).
craftGetBillsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftGetBillsFn env = do
    bidArg ← Lua.tointeger 1
    billList ← Lua.liftIO $ do
        mPage ← activeWorldPage env
        case mPage of
            Nothing      → return []
            Just (_, ws) → do
                bills ← readIORef (wsCraftBillsRef ws)
                return $ case bidArg of
                    Just b  → billsForStation (BuildingId (fromIntegral b))
                                              bills
                    Nothing → sortOn cbId (HM.elems (cbsBills bills))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] billList) $ \(i, bill) → do
        pushBill bill
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | Push one bill as a Lua table: { id, station, recipe, remaining,
--   progress, seq, paused, claimant?, claimedAt? }. remaining -1 =
--   repeat forever; seq is the manual-reorder sort key
--   (billsForStation / #330's reorderBill).
pushBill ∷ CraftBill → Lua.LuaE Lua.Exception ()
pushBill bill = do
    Lua.newtable
    let putI k v = Lua.pushinteger (fromIntegral v) >> Lua.setfield (-2) k
        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                   >> Lua.setfield (-2) k
    putI "id"        (unBillId (cbId bill))
    putI "station"   (unBuildingId (cbStation bill))
    Lua.pushstring (TE.encodeUtf8 (cbRecipe bill))
    Lua.setfield (-2) "recipe"
    Lua.pushinteger (fromIntegral (cbRemaining bill))
    Lua.setfield (-2) "remaining"
    putN "progress"  (cbProgress bill)
    putI "seq"       (cbSeq bill)
    Lua.pushboolean (cbPaused bill)
    Lua.setfield (-2) "paused"
    forM_ (cbClaimant bill) $ \(UnitId u) → do
        putI "claimant" u
        putN "claimedAt" (cbClaimedAt bill)

-- | Roll one output line into fresh instances. A crafted item is
--   factory-new: condition and sharpness start at 100 (per #325; loot
--   spawns roll condition to simulate age, crafts don't). Quality rolls
--   from the def's spec here as the fallback for skill-less recipes;
--   skill-tagged recipes overwrite it in applyCraft with the crafter-
--   derived craftQuality (#343). Containers spawn at their default
--   fill, same as unit.addItem.
rollOutputs ∷ EngineEnv → (RecipeIngredient, ItemDef) → IO [ItemInstance]
rollOutputs env (ing, def) = replicateM (max 0 (riCount ing)) $ do
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
        -- At ambient for now — a recipe-declared output temperature
        -- (hot bar off the smelter, 100 °C coffee) is the cooking
        -- tier's slice (#344 provides the field + setters, #346 the
        -- recipe schema hook).
        , iiTemp        = Nothing
        }

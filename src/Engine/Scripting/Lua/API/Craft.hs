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
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.YamlRecipes
import Craft.Types
import Craft.Execute (consumeIngredients, craftQuality)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemInstance(..), ItemContainer(..),
                   lookupItemDef)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
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
                            , rdRepairAxis = ryRepairAxis d
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
    forM_ (rdRepairAxis d) $ putS "repairAxis"
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

-- | craft.execute(uid, recipeId) → ok, err?. Runs one craft against the
--   unit's TOP-LEVEL inventory: knowledge gate, then all-or-nothing
--   consumption of inputs + fuel, then the outputs are appended — at
--   crafter-derived quality when the recipe is skill-tagged (#343). On
--   failure returns false plus a reason and the inventory is untouched.
craftExecuteFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
craftExecuteFn env = do
    idArg  ← Lua.tointeger 1
    ridArg ← Lua.tostring 2
    case (idArg, ridArg) of
        (Just n, Just ridBS) → do
            let uid = UnitId (fromIntegral n)
                rid = TE.decodeUtf8 ridBS
            result ← Lua.liftIO $ executeCraft env uid rid
            pushOkErr result
        _ → pushOkErr (Left "craft.execute: expected (uid, recipeId)")

-- | craft.executeAt(uid, recipeId, bid) → ok, err?. The station-aware
--   craft (#326): identical consumption semantics to craft.execute,
--   but refused unless `bid` is a BUILT work station on the unit's
--   world page whose def offers the recipe's station kind, with the
--   unit standing on or adjacent to the footprint (Chebyshev ≤ 1).
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
            pushOkErr result
        _ → pushOkErr
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

-- | Shared (ok) / (false, reason) return shape for the execute fns.
pushOkErr ∷ Either Text () → Lua.LuaE Lua.Exception Lua.NumResults
pushOkErr (Right ()) = Lua.pushboolean True >> return 1
pushOkErr (Left err) = do
    Lua.pushboolean False
    Lua.pushstring (TE.encodeUtf8 err)
    return 2

-- | The IO side of craft.execute. Output instances are rolled BEFORE
--   the unit-manager update (rolls + fresh ids are IO), then the
--   verify-consume-produce step is a single atomicModifyIORef' — the
--   same pattern as unit.addItem, so nothing can race between the
--   inventory check and the swap. Rolled outputs are discarded when
--   the craft fails (a few wasted instance ids, never a dupe).
executeCraft ∷ EngineEnv → UnitId → Text → IO (Either Text ())
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
           → (UnitManager, Either Text ())
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
                   , Right () )

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

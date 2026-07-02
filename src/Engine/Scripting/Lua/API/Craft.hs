{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the crafting recipe catalogue (#325).
--   engine.loadRecipeYaml loads data/recipes/*.yaml into the
--   RecipeManager (mirrors engine.loadInfectionYaml); craft.get /
--   craft.getNames give read-only access; craft.execute runs one craft
--   against a unit's inventory — verify inputs + fuel, consume them,
--   produce the outputs. No stations or AI yet: station kind is data
--   carried by the recipe (#326), work is data for the craft AI (#329).
module Engine.Scripting.Lua.API.Craft
    ( loadRecipeYamlFn
    , craftGetFn
    , craftGetNamesFn
    , craftExecuteFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM, replicateM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.YamlRecipes
import Craft.Types
import Craft.Execute (consumeIngredients)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemInstance(..), ItemContainer(..),
                   lookupItemDef)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))

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

-- | craft.get(id) → table | nil. Read-only access to one recipe:
--   { id, name, station, work, knowledge?, inputs = [{item,count}…],
--     fuel = {item,count}?, outputs = [{item,count}…] }.
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
                Just d → do
                    Lua.newtable
                    let putS k v = Lua.pushstring (TE.encodeUtf8 v)
                                   >> Lua.setfield (-2) k
                        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                                   >> Lua.setfield (-2) k
                    putS "id"      (rdId d)
                    putS "name"    (rdName d)
                    putS "station" (rdStation d)
                    putN "work"    (rdWork d)
                    forM_ (rdKnowledge d) $ putS "knowledge"
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
                    return 1

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
--   consumption of inputs + fuel, then the outputs are appended. On
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
            finish result
        _ → finish (Left "craft.execute: expected (uid, recipeId)")
  where
    finish (Right ()) = Lua.pushboolean True >> return 1
    finish (Left err) = do
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
                let u' = u { uiInventory = inv' ⧺ outs }
                in ( um { umInstances = HM.insert uid u' (umInstances um) }
                   , Right () )

-- | Roll one output line into fresh instances. A crafted item is
--   factory-new: condition and sharpness start at 100 (per #325; loot
--   spawns roll condition to simulate age, crafts don't). Quality still
--   rolls from the def's spec — tying it to the crafter's skill is the
--   quality issue (#343). Containers spawn at their default fill, same
--   as unit.addItem.
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
        }

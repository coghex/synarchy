{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the crafting recipe catalogue (#325).
--   engine.loadRecipeYaml loads data/recipes/*.yaml into the
--   RecipeManager (mirrors engine.loadInfectionYaml); craft.get /
--   craft.getNames give read-only access. pushRecipe is shared with
--   Engine.Scripting.Lua.API.Repair (repair.get) and the execute/bill
--   sub-modules never need it, so it lives here alone with its
--   catalogue reads.
module Engine.Scripting.Lua.API.Craft.Recipe
    ( loadRecipeYamlFn
    , craftGetFn
    , craftGetNamesFn
    , pushRecipe
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.YamlRecipes
import Craft.Types

-- | engine.loadRecipeYaml(path) — parse a YAML file of recipe defs,
--   register each into the RecipeManager, return the count.
loadRecipeYamlFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadRecipeYamlFn env = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → Lua.pushnumber 0 >> return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
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
                            , rdOutputTemp = ryOutputTemp d
                            , rdPowerDraw  = ryPowerDraw d
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
--   knowledge?, skill?, repairAxis?, powerDraw, inputs = [{item,count}…],
--   fuel = {item,count}?, outputs = [{item,count}…] }. Shared by
--   craft.get and repair.get (Engine.Scripting.Lua.API.Repair) so both
--   accessors report the identical shape. powerDraw (#590) is always
--   present (0 = never power-gated), unlike the other optional fields.
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
    forM_ (rdOutputTemp d) $ putN "outputTemp"
    putN "powerDraw" (rdPowerDraw d)
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
            let key = TE.decodeUtf8Lenient idBS
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

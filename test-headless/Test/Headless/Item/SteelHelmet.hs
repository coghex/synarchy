{-# LANGUAGE Strict #-}
-- | Production-content coverage for the forgeable steel helmet (#1785).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "steel helmet content"'@.
module Test.Headless.Item.SteelHelmet (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (doesFileExist)
import Combat.Resolution.Damage (defenderArmor)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlEquipment
    ( EquipmentYamlClass(..), EquipmentYamlSlot(..), loadEquipmentYaml )
import Engine.Asset.YamlItems (ItemYamlDef(..), loadItemYaml)
import Engine.Asset.YamlRecipes
    ( RecipeYamlDef(..), RecipeYamlIngredient(..), loadRecipeYaml )
import Engine.Asset.YamlSubstance (SubstanceYamlDef(..), loadSubstanceYaml)
import Engine.Asset.YamlUnits
    ( UnitYamlBodyPart(..), UnitYamlDef(..), loadUnitYaml )
import Engine.Core.Log
    ( LoggerState, LogBackend(..), LogConfig(..), defaultLogConfig
    , initLogger )
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Items.Defs (itemDefFromYaml)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Equipment.Types
    ( EquipmentClass(..), EquipmentClassManager(..), EquipmentSlot(..) )
import Item.Types
    ( ItemArmor(..), ItemDef(..), ItemInstance(..), ItemManager(..) )
import Substance.Types (SubstanceDef(..), SubstanceManager(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))

helmetItemPath ∷ FilePath
helmetItemPath = "data/items/steel_helmet.yaml"

helmetTexturePath ∷ FilePath
helmetTexturePath = "assets/textures/items/wearable/helmet_steel.png"

silentLogger ∷ IO LoggerState
silentLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

expectOne ∷ String → [α] → IO α
expectOne label xs = case xs of
    [x] → pure x
    _   → expectationFailure
              (label <> ": expected exactly one definition, got "
                     <> show (length xs))
          >> fail (label <> ": wrong definition count")

equipmentClassFromYaml ∷ EquipmentYamlClass → EquipmentClass
equipmentClassFromYaml c = EquipmentClass
    { ecName = eycName c
    , ecSilhouetteTex = TextureHandle 0
    , ecSilhouetteW = eycSilhouetteW c
    , ecSilhouetteH = eycSilhouetteH c
    , ecSlots = map slotFromYaml (eycSlots c)
    }
  where
    slotFromYaml s = EquipmentSlot
        { esId = eysId s
        , esName = if T.null (eysName s) then eysId s else eysName s
        , esKind = eysKind s
        , esX = eysX s, esY = eysY s, esW = eysW s, esH = eysH s
        }

substanceFromYaml ∷ SubstanceYamlDef → SubstanceDef
substanceFromYaml s = SubstanceDef
    { sbsName = syName s
    , sbsDensity = syDensity s
    , sbsTensileStrength = syTensileStrength s
    , sbsYieldStrength = syYieldStrength s
    , sbsShearStrength = syShearStrength s
    , sbsFractureToughness = syFractureToughness s
    , sbsHardness = syHardness s
    , sbsStabResistance = syStabResistance s
    , sbsSlashResistance = sySlashResistance s
    , sbsBluntResistance = syBluntResistance s
    }

minimalAcolyteDef ∷ UnitDef
minimalAcolyteDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Just "humanoid", udStartingEquipment = HM.empty
    , udStartingAccessories = [], udBodyParts = []
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = []
    }

helmetInstance ∷ Float → ItemInstance
helmetInstance condition = ItemInstance
    { iiDefName = "steel_helmet", iiCurrentFill = 0
    , iiQuality = 60, iiCondition = condition
    , iiWeight = 1.5, iiSharpness = 100
    , iiContents = [], iiInstanceId = 1785
    , iiTemp = Nothing, iiBulk = Just 3.0, iiStorage = Nothing
    }

holder ∷ UnitInstance
holder = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = WorldPageId "helmet-test"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [helmetInstance 100]
    , uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = FactionPlayer, uiWounds = [], uiScars = []
    , uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

runOk ∷ LuaBackendState → Text → IO Text
runOk ls src = do
    result ← executeDebugLua (lbsLuaState ls) src
    result `shouldNotSatisfy` ("error:" `T.isPrefixOf`)
    result `shouldNotSatisfy` ("syntax error:" `T.isPrefixOf`)
    pure result

runtimeContent ∷ IO (ItemManager, EquipmentClassManager, SubstanceManager)
runtimeContent = do
    logger ← silentLogger
    itemYaml ← expectOne "steel helmet item"
        =≪ loadItemYaml logger helmetItemPath
    humanoidYaml ← expectOne "humanoid equipment class"
        =≪ loadEquipmentYaml logger "data/equipment/humanoid.yaml"
    steelYaml ← expectOne "steel substance"
        =≪ filter ((≡ "steel") ∘ syName)
            <$> loadSubstanceYaml logger "data/substances/metals.yaml"
    let itemDef = itemDefFromYaml helmetItemPath (TextureHandle 0) (TextureHandle 0) itemYaml
        humanoid = equipmentClassFromYaml humanoidYaml
        steel = substanceFromYaml steelYaml
    pure ( ItemManager (HM.singleton "steel_helmet" itemDef)
         , EquipmentClassManager (HM.singleton "humanoid" humanoid)
         , SubstanceManager (HM.singleton "steel" steel) )

resetRuntime ∷ EngineEnv → ItemManager → EquipmentClassManager → SubstanceManager
             → IO ()
resetRuntime env items equipment substances = do
    writeIORef (itemManagerRef env) items
    writeIORef (equipmentClassManagerRef env) equipment
    writeIORef (substanceManagerRef env) substances
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" minimalAcolyteDef
        , umInstances = HM.singleton (UnitId 1) holder
        }

spec ∷ SpecWith EngineEnv
spec = describe "steel helmet content" $ do
    it "loads the shipped item, humanoid slot, acolyte head, and steel" $
        \_ → do
            logger ← silentLogger
            itemYaml ← expectOne "steel helmet item"
                =≪ loadItemYaml logger helmetItemPath
            iydSprite itemYaml `shouldBe` T.pack helmetTexturePath
            doesFileExist helmetTexturePath `shouldReturn` True
            let itemDef = itemDefFromYaml helmetItemPath
                              (TextureHandle 0) (TextureHandle 0) itemYaml
            ( idName itemDef, idDisplayName itemDef, idWeight itemDef
              , idBulk itemDef, idKind itemDef, idCategory itemDef
              , idMake itemDef, idMaterial itemDef )
                `shouldBe` ( "steel_helmet", "Steel Helmet", 1.5
                           , 3.0, "helmet", "Armor", "acolyte", "steel" )
            idWeightSpec itemDef `shouldBe` Nothing
            idQualitySpec itemDef `shouldBe` Just (50, 75)
            idArmor itemDef `shouldBe` Just (ItemArmor 2 ["head"])
            idContainer itemDef `shouldBe` Nothing
            idStorage itemDef `shouldBe` Nothing
            idFood itemDef `shouldBe` Nothing
            idWeapon itemDef `shouldBe` Nothing
            idUnequippable itemDef `shouldBe` False
            idBuffs itemDef `shouldBe` []
            idInsulation itemDef `shouldBe` 0

            humanoid ← expectOne "humanoid equipment class"
                =≪ loadEquipmentYaml logger "data/equipment/humanoid.yaml"
            [ (eysId s, eysKind s) | s ← eycSlots humanoid
                                      , eysId s ≡ "helmet" ]
                `shouldBe` [("helmet", "helmet")]

            acolyte ← expectOne "acolyte unit"
                =≪ loadUnitYaml logger "data/units/acolyte.yaml"
            uydEquipmentClass acolyte `shouldBe` Just "humanoid"
            "head" `shouldSatisfy` (`elem` map uybpId (uydBodyParts acolyte))

            steel ← expectOne "steel substance"
                =≪ filter ((≡ "steel") ∘ syName)
                    <$> loadSubstanceYaml logger "data/substances/metals.yaml"
            syName steel `shouldBe` "steel"

    it "loads the exact shipped forge recipe" $ \_ → do
        logger ← silentLogger
        recipe ← expectOne "steel helmet recipe"
            =≪ filter ((≡ "forge_steel_helmet") ∘ ryId)
                <$> loadRecipeYaml logger "data/recipes/fabrication.yaml"
        ( ryName recipe, ryStation recipe, rySkill recipe, ryWork recipe )
            `shouldBe` ("Forge Steel Helmet", "forge", Just "smithing", 20)
        ryInputs recipe `shouldBe` [RecipeYamlIngredient "steel_bar" 2]
        ryOutputs recipe `shouldBe` [RecipeYamlIngredient "steel_helmet" 1]
        ryFuel recipe `shouldBe` Nothing
        ryKnowledge recipe `shouldBe` Nothing
        ryRepairAxis recipe `shouldBe` Nothing

    it "equips and unequips through the supported path, and only intact \
       \condition contributes a 2 mm steel head layer" $ \env → do
        (items, equipment, substances) ← runtimeContent
        resetRuntime env items equipment substances
        ls ← newBareLuaBackend env
        equippedOk ← runOk ls
            "return tostring(equipment.equip(1, 'helmet', 'steel_helmet', 1785))"
        equippedOk `shouldBe` "\"true\""

        units ← readIORef (unitManagerRef env)
        equipped ← case HM.lookup (UnitId 1) (umInstances units) of
            Nothing → expectationFailure "holder disappeared during equip"
                       >> fail "missing holder"
            Just inst → pure inst
        map iiDefName (uiInventory equipped) `shouldBe` []
        map iiDefName (HM.elems (uiEquipment equipped))
            `shouldBe` ["steel_helmet"]

        case defenderArmor substances items equipped "head" of
            [(slot, inst, Just steel, thickness)] → do
                slot `shouldBe` "helmet"
                iiDefName inst `shouldBe` "steel_helmet"
                sbsName steel `shouldBe` "steel"
                thickness `shouldBe` 2
            other → expectationFailure
                ("expected one resolved steel head layer, got " <> show other)
        let broken = equipped
                { uiEquipment = HM.adjust
                    (\inst → inst { iiCondition = 0 }) "helmet"
                    (uiEquipment equipped) }
        defenderArmor substances items broken "head" `shouldBe` []

        unequippedOk ← runOk ls
            "return tostring(equipment.unequip(1, 'helmet'))"
        unequippedOk `shouldBe` "\"true\""
        finalUnits ← readIORef (unitManagerRef env)
        case HM.lookup (UnitId 1) (umInstances finalUnits) of
            Nothing → expectationFailure "holder disappeared during unequip"
            Just inst → do
                HM.lookup "helmet" (uiEquipment inst) `shouldBe` Nothing
                map iiDefName (uiInventory inst) `shouldBe` ["steel_helmet"]

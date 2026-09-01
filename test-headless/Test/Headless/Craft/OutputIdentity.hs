{-# LANGUAGE Strict #-}
-- | A craft never disturbs a same-def instance the crafter already
--   carried (#1772).
--
--   The reproduction that opened #1772 was a live-AI probe check: a
--   crafter carrying a @granite_chunk@ finished a bill whose output is
--   @granite_chunk@ and afterwards held nothing. The craft path was not
--   what removed it — the acolyte's @store_materials@ action had
--   auto-deposited the carried Materials item into the very furnace it
--   was standing at, which is deliberate hauling behaviour
--   (@scripts\/unit_ai_logistics.lua@) and which
--   @tools\/craft_bill_probe.py@ now suspends for the length of its
--   assertion window.
--
--   That leaves the underlying contract itself pinned only by a check
--   that has to out-race an AI decision loop. This gate pins it
--   directly instead, through the real production verb against a real
--   engine, with no AI, no station, no bill and no timing:
--
--   1. __The carried instance survives, by id.__ A crafter holding one
--      demanded input plus an unrelated instance of the recipe's
--      OUTPUT definition still holds that exact @iiInstanceId@ after
--      @craft.execute@ returns.
--   2. __The returned ids are all fresh.__ Every id
--      @craft.execute@ hands back is newly minted — the pre-existing id
--      is never among them, so the AI's exact-ID drop
--      (@scripts\/unit_ai_craft.lua@) cannot be handed the carried one
--      to drop.
--   3. __Consumption is by demand, not by resemblance.__ The
--      @steel_bar@ goes and the @granite_chunk@ stays, and a crafter
--      holding SEVERAL pre-existing chunks keeps every one of them —
--      an operation intended for the fresh outputs never reaches a
--      different instance that merely shares a @defName@.
--   4. __The survivor is untouched, not merely present.__ Its quality
--      and condition are the values it carried in, so "kept" cannot be
--      satisfied by a fresh instance that happened to reuse the id.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Craft output identity"'@.
module Test.Headless.Craft.OutputIdentity (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import Craft.Types
    ( RecipeDef(..), RecipeIngredient(..), RecipeManager(..) )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), emptyWorldState, emptyWorldManager )

-- * Fixtures

page ∷ WorldPageId
page = WorldPageId "craft_output_identity_page"

crafterUid ∷ UnitId
crafterUid = UnitId 1

-- | Both defs are shipped shapes reduced to what the craft path reads:
--   a name, a weight and a category. @granite_chunk@ is the recipe's
--   OUTPUT and is also what the crafter already carries — the whole
--   point of the fixture.
itemDef ∷ Text → Text → ItemDef
itemDef name category = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 10.0, idWeightSpec = Nothing, idBulk = 5.0
    , idStorage = Nothing, idKind = "misc"
    , idCategory = category, idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

testItems ∷ ItemManager
testItems = ItemManager $ HM.fromList
    [ ("steel_bar",     itemDef "steel_bar" "Materials")
    , ("granite_chunk", itemDef "granite_chunk" "Materials")
    ]

-- | The probe's own bill recipe (tools\/craft_bill_probe.py's
--   @bill_probe_smelt@): one bar in, two chunks out. Untagged, so no
--   skill or knowledge gate stands between the fixture and the swap.
smeltRecipe ∷ RecipeDef
smeltRecipe = RecipeDef
    { rdId = "identity_smelt", rdName = "Identity Smelt"
    , rdStation = "smelt"
    , rdInputs = [RecipeIngredient "steel_bar" 1]
    , rdFuel = Nothing, rdWork = 2
    , rdOutputs = [RecipeIngredient "granite_chunk" 2]
    , rdKnowledge = Nothing, rdSkill = Nothing
    , rdRepairAxis = Nothing, rdOutputTemp = Nothing, rdPowerDraw = 0
    }

minimalUnitDef ∷ UnitDef
minimalUnitDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts = []
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = []
    }

-- | Distinctive quality/condition so a survivor can be told apart from
--   a freshly minted instance even if ids were ever reused.
mkItem ∷ Text → Word64 → ItemInstance
mkItem name iid = ItemInstance
    { iiDefName = name, iiCurrentFill = 0
    , iiQuality = 37, iiCondition = 61
    , iiWeight = 10.0, iiSharpness = 100
    , iiContents = [], iiInstanceId = iid
    , iiTemp = Nothing, iiBulk = Just 5, iiStorage = Nothing
    }

crafter ∷ [ItemInstance] → UnitInstance
crafter inv = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty
    , uiInventory = inv
    , uiEquipment = HM.empty
    , uiAccessories = []
    , uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- | One visible page, the two item defs, the recipe, and a crafter
--   holding exactly @inv@. Rebuilt per example so no case inherits
--   another's inventory.
resetScene ∷ EngineEnv → [ItemInstance] → IO ()
resetScene env inv = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(page, ws)], wmVisible = [page] }
    writeIORef (itemManagerRef env) testItems
    writeIORef (recipeManagerRef env)
        (RecipeManager (HM.singleton "identity_smelt" smeltRecipe))
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" minimalUnitDef
        , umInstances = HM.singleton crafterUid (crafter inv)
        }

-- * Lua plumbing (the Test.Headless.Item.Condition pattern)

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
    r ← executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` isLuaError
    pure r
  where isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | The debug console is single-line, so a snippet joins with spaces.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- | Instance ids the crafter holds for @name@, read back out of the
--   real unit manager rather than through a second Lua hop.
carriedIds ∷ EngineEnv → Text → IO [Word64]
carriedIds env name = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup crafterUid (umInstances um) of
        Nothing → []
        Just u  → [ iiInstanceId i | i ← uiInventory u, iiDefName i ≡ name ]

carried ∷ EngineEnv → IO [ItemInstance]
carried env = do
    um ← readIORef (unitManagerRef env)
    pure $ maybe [] uiInventory (HM.lookup crafterUid (umInstances um))

-- | @craft.execute@ through the registered verb, returning the ids it
--   handed back. Fails the example on a refused craft, so a broken
--   fixture can never read as "identity preserved".
executeAndReturnIds ∷ LuaBackendState → IO [Word64]
executeAndReturnIds ls = do
    r ← runOk ls $ luaLines
        [ "local ok, ids = craft.execute(1, 'identity_smelt');"
        , "if not ok then return 'ERR:' .. tostring(ids) end;"
        , "local out = {};"
        , "for _, id in ipairs(ids) do out[#out+1] = tostring(id) end;"
        , "return table.concat(out, ',')"
        ]
    let body = T.dropWhileEnd (≡ '"') (T.dropWhile (≡ '"') r)
    when ("ERR:" `T.isPrefixOf` body) $
        expectationFailure ("craft.execute refused: " <> T.unpack body)
    pure [ read (T.unpack t) | t ← T.splitOn "," body, not (T.null t) ]

-- * Spec

spec ∷ SpecWith EngineEnv
spec = describe "Craft output identity" $ do

    it "keeps the exact pre-existing instance that shares the recipe's \
       \output definition, and returns only fresh ids" $ \env → do
        resetScene env [ mkItem "granite_chunk" 700
                       , mkItem "steel_bar" 701 ]
        ls ← newBareLuaBackend env
        fresh ← executeAndReturnIds ls

        -- The carried chunk is still there, by id.
        kept ← carriedIds env "granite_chunk"
        700 `shouldSatisfy` (`elem` kept)

        -- Two fresh outputs, and the carried id is not one of them.
        length fresh `shouldBe` 2
        fresh `shouldNotSatisfy` elem 700
        kept `shouldBe` (700 : fresh)

        -- The demanded input is what went.
        bars ← carriedIds env "steel_bar"
        bars `shouldBe` []

    it "leaves the survivor's own fields untouched, so \"kept\" cannot \
       \be satisfied by a fresh instance wearing its id" $ \env → do
        resetScene env [ mkItem "granite_chunk" 710
                       , mkItem "steel_bar" 711 ]
        ls ← newBareLuaBackend env
        _ ← executeAndReturnIds ls
        inv ← carried env
        case [ i | i ← inv, iiInstanceId i ≡ 710 ] of
            [survivor] → do
                iiQuality survivor `shouldBe` 37
                iiCondition survivor `shouldBe` 61
            other → expectationFailure
                ("expected exactly one instance 710, got " <> show (length other))

    it "keeps EVERY pre-existing same-def instance, not just the first" $ \env → do
        resetScene env [ mkItem "granite_chunk" 720
                       , mkItem "granite_chunk" 721
                       , mkItem "granite_chunk" 722
                       , mkItem "steel_bar" 723 ]
        ls ← newBareLuaBackend env
        fresh ← executeAndReturnIds ls
        kept ← carriedIds env "granite_chunk"
        kept `shouldBe` ([720, 721, 722] <> fresh)

    it "still holds the earlier batch after a second craft, so no cycle \
       \reaches back into what an earlier one produced or preserved" $ \env → do
        resetScene env [ mkItem "granite_chunk" 730
                       , mkItem "steel_bar" 731
                       , mkItem "steel_bar" 732 ]
        ls ← newBareLuaBackend env
        firstIds  ← executeAndReturnIds ls
        secondIds ← executeAndReturnIds ls
        kept ← carriedIds env "granite_chunk"
        kept `shouldBe` (730 : firstIds <> secondIds)
        length secondIds `shouldBe` 2
        secondIds `shouldNotSatisfy` any (`elem` (730 : firstIds))
        bars ← carriedIds env "steel_bar"
        bars `shouldBe` []

    it "changes nothing at all when the craft is refused — a missing \
       \input leaves the carried chunk and its id exactly as they were" $ \env → do
        resetScene env [ mkItem "granite_chunk" 740 ]
        ls ← newBareLuaBackend env
        r ← runOk ls $ luaLines
            [ "local ok, err = craft.execute(1, 'identity_smelt');"
            , "return ok and 'crafted' or 'refused'"
            ]
        r `shouldBe` "\"refused\""
        kept ← carriedIds env "granite_chunk"
        kept `shouldBe` [740]

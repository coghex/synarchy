-- | Shared fixtures for the "Portable container knowledge" gate
--   (#2512): item definitions with real container/storage specs, the
--   crate-and-kit instances every example observes, and the live scene
--   the locator walks.
--
--   Deliberately in one place so the model, locator, persistence,
--   lifecycle and Lua parts all describe the SAME crate — an example
--   proving a record survives a move is only worth anything if the
--   thing that moved is the thing the other parts are talking about.
module Test.Headless.Item.PortableKnowledge.Fixture
    ( pkPageA, pkPageB
    , crateId, kitId, bandageId, looseId, unlocatableId
    , testItems
    , crate, crateEmpty, kit, bandage, loose
    , mkBuilding, mkUnit, groundWith
    , storageDef, plainDef
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Building.Schema (BuildingVisualClass(..), legacyAssets)
import Building.Types (BuildingDef(..), BuildingInstance(..))
import Engine.Asset.Handle (TextureHandle(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Types
    ( ItemContainer(..), ItemDef(..), ItemInstance(..), ItemManager(..)
    , ItemStorage(..) )
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types (UnitInstance(..))
import World.Page.Types (WorldPageId(..))

pkPageA, pkPageB ∷ WorldPageId
pkPageA = WorldPageId "portable_page_a"
pkPageB = WorldPageId "portable_page_b"

-- | Distinct, non-consecutive ids so an off-by-one in a walk cannot
--   coincidentally land on the right instance.
crateId, kitId, bandageId, looseId, unlocatableId ∷ Word64
crateId       = 41
kitId         = 42
bandageId     = 43
looseId       = 57
unlocatableId = 9001   -- ^ never placed anywhere live, in any example

-- | Real defs, so 'Item.Types.itemTotalWeight' has a genuine per-unit
--   fill weight to multiply by instead of falling back to 1.0 kg/L —
--   which is what makes a remembered weight a real recursive measure
--   rather than a number that would come out the same either way.
testItems ∷ ItemManager
testItems = ItemManager $ HM.fromList
    [ ("supply_crate", (bareItemDef "supply_crate" 6.0)
        { idContainer = Just ItemContainer
            { icCapacity = 20, icHolds = "supplies"
            , icFillWeight = 0.5, icDefaultFill = 0 }
        , idStorage = Just (ItemStorage 60 40) })
    , ("first_aid_kit", (bareItemDef "first_aid_kit" 1.25)
        { idContainer = Just ItemContainer
            { icCapacity = 10, icHolds = "supplies"
            , icFillWeight = 0.5, icDefaultFill = 0 }
        , idStorage = Just (ItemStorage 8 6) })
    , ("bandage", bareItemDef "bandage" 0.05)
    , ("pry_bar", bareItemDef "pry_bar" 2.5)
    ]

bareItemDef ∷ Text → Float → ItemDef
bareItemDef name w = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0
    , idIconTexture = TextureHandle 0
    , idWeight = w, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = "misc"
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

-- | A stocked crate: it holds a first-aid KIT, which itself holds a
--   bandage. Two levels of nesting, so "observing the crate does not
--   observe the kit" is a claim with something to be wrong about.
--
--   Every field carries a distinct, non-default value so a remembered
--   copy that dropped or defaulted any one of them is observably
--   different from the original.
crate ∷ ItemInstance
crate = ItemInstance
    { iiDefName     = "supply_crate"
    , iiCurrentFill = 2
    , iiQuality     = 73
    , iiCondition   = 64
    , iiWeight      = 6.0
    , iiSharpness   = 11
    , iiInstanceId  = crateId
    , iiTemp        = Just 18.5
    , iiBulk        = Just 30.0
    , iiStorage     = Just (ItemStorage 60 40)
    , iiContents    = [kit]
    }

-- | The same crate with nothing in it — the known-EMPTY fixture, which
--   must never read as never-inspected.
crateEmpty ∷ ItemInstance
crateEmpty = crate { iiContents = [] }

kit ∷ ItemInstance
kit = ItemInstance
    { iiDefName     = "first_aid_kit"
    , iiCurrentFill = 3
    , iiQuality     = 82
    , iiCondition   = 55
    , iiWeight      = 1.25
    , iiSharpness   = 41
    , iiInstanceId  = kitId
    , iiTemp        = Just 21.5
    , iiBulk        = Just 4.0
    , iiStorage     = Just (ItemStorage 8 6)
    , iiContents    = [bandage]
    }

bandage ∷ ItemInstance
bandage = ItemInstance
    { iiDefName = "bandage", iiCurrentFill = 1, iiQuality = 100
    , iiCondition = 100, iiWeight = 0.05, iiSharpness = 0
    , iiInstanceId = bandageId, iiTemp = Nothing, iiContents = []
    , iiBulk = Just 0.1, iiStorage = Nothing }

-- | A plain non-container item, for the "an ordinary item is locatable
--   too, and simply declares no capacity" cases.
loose ∷ ItemInstance
loose = ItemInstance
    { iiDefName = "pry_bar", iiCurrentFill = 0, iiQuality = 50
    , iiCondition = 90, iiWeight = 2.5, iiSharpness = 30
    , iiInstanceId = looseId, iiTemp = Nothing, iiContents = []
    , iiBulk = Just 2.0, iiStorage = Nothing }

groundWith ∷ [ItemInstance] → GroundItems
groundWith insts = GroundItems
    { gisNextId = length insts
    , gisItems = HM.fromList
        [ (i, GroundItem inst 1 1) | (i, inst) ← zip [0 ..] insts ]
    }

storageDef, plainDef ∷ BuildingDef
storageDef = (bareBuildingDef "cargo_hold_S") { bdStorageCapacity = 200 }
plainDef   = bareBuildingDef "shed"

bareBuildingDef ∷ Text → BuildingDef
bareBuildingDef name = BuildingDef
    { bdName            = name
    , bdDisplayName     = name
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTextures        = legacyAssets (TextureHandle 0)
    , bdIconTexture     = TextureHandle 0
    , bdTileW           = 1
    , bdTileH           = 1
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdRoleAnims       = Map.empty
    , bdVisualClass     = FreestandingInstallation
    , bdPowerDrain      = 0, bdPowerNode = Nothing
    }

-- | A building on @page@ with the given delivered materials and loose
--   storage — the two item containers 'World.Save.Types.pageItemContainers'
--   enumerates for a building.
mkBuilding
    ∷ WorldPageId → Text → HM.HashMap Text [ItemInstance] → [ItemInstance]
    → BuildingInstance
mkBuilding page defName delivered storage = BuildingInstance
    { biDefName = defName, biPage = page, biTexture = TextureHandle 0
    , biAnchorX = 0, biAnchorY = 0, biGridZ = 0, biSpawnedAt = 0
    , biTileW = 1, biTileH = 1, biSpawnRemaining = 0
    , biBuildProgress = 0
    , biMaterialsDelivered = delivered
    , biStorage = storage
    }

-- | A unit on @page@ with the given inventory, equipment and
--   accessories — the three item containers a unit contributes.
mkUnit
    ∷ WorldPageId → [ItemInstance] → HM.HashMap Text ItemInstance
    → [ItemInstance] → UnitInstance
mkUnit page inventory equipment accessories = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inventory
    , uiEquipment = equipment
    , uiAccessories = accessories, uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

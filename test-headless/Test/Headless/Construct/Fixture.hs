-- | Shared structure-catalogue fixture for the construction specs
--   (#1844).
--
--   Since #1844 a structure designation is admitted only when
--   "World.Construct.Plan"'s resolver says its art AND its build
--   metadata are registered, so an engine-backed construction spec has
--   to register a pack the way @scripts\/structures.lua@ does at boot.
--   Registering through 'registerPackArt' — the production entry point,
--   with its own all-or-nothing completeness rule — is what keeps this
--   fixture from certifying a pack the engine would refuse.
module Test.Headless.Construct.Fixture
    ( registerFixturePacks
    , registerArtOnlyPack
    , artOnlyPackName
    , fixturePackName
    , fixtureWirePack
    , fixtureFloorCost
    , fixtureWireCost
      -- * Engine scene
    , fixtureItems
    , fixtureItem
    , payerUnit
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.IORef (atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv, structureArtCatalogRef)
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..))
import Structure.ArtCatalog
import Structure.Facing (WallCaps(..), WallEdge(..))
import Structure.Wire (allWireShapes)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types (UnitInstance(..))
import World.Page.Types (WorldPageId)

-- | The pack every construction fixture designates from — the shipped
--   name, so a spec's descriptors are the ones the game really uses.
fixturePackName ∷ Text
fixturePackName = "dungeon_1"

fixtureWirePack ∷ Text
fixtureWirePack = "wire"

-- | The costs this fixture registers. Named rather than inlined so a
--   receipt assertion can be written against the SAME value the
--   registration used, instead of a second copy that could drift.
fixtureFloorCost, fixtureWireCost ∷ BuildCost
fixtureFloorCost = mkBuildCost 3.0 [("steel_plate", 1)]
fixtureWireCost  = mkBuildCost 1.5 [("wiring", 1)]

-- | Register both fixture packs, complete. Idempotent: 'registerPackArt'
--   treats an identical repeat as success and changes nothing, so a
--   per-example fixture may call this every time.
registerFixturePacks ∷ EngineEnv → IO ()
registerFixturePacks env =
    forM_ [dungeonRegistration, wireRegistration] $ \reg →
        atomicModifyIORef' (structureArtCatalogRef env) $ \cat →
            case registerPackArt reg cat of
                (cat', _) → (cat', ())

-- | A pack with complete floor ART and NO declared cost — the
--   "visible-invalid because it cannot be costed" case, which is
--   deliberately distinct from missing art.
artOnlyPackName ∷ Text
artOnlyPackName = "artonly"

registerArtOnlyPack ∷ EngineEnv → IO ()
registerArtOnlyPack env =
    atomicModifyIORef' (structureArtCatalogRef env) $ \cat →
        case registerPackArt reg cat of
            (cat', _) → (cat', ())
  where
    reg = PackArtRegistration
        { parPack    = artOnlyPackName
        , parKinds   = [(KFloor, False, Nothing)]
        , parEntries = [(AkFloor, art "artonly_floor")]
        }

dungeonRegistration ∷ PackArtRegistration
dungeonRegistration = PackArtRegistration
    { parPack = fixturePackName
    , parKinds =
        [ (KFloor,   True,  Just fixtureFloorCost)
        , (KCeiling, True,  Just (mkBuildCost 3.0 [("steel_plate", 1)]))
        , (KPost,    True,  Just (mkBuildCost 2.0 [("wood_log", 1)]))
        , (KWall,    True,  Just (mkBuildCost 4.0 [("steel_bar", 2)]))
        ]
    , parEntries =
        [ (AkFloor,   art "floor")
        , (AkCeiling, art "ceiling")
        , (AkPost,    art "post")
        ]
        ⧺ [ (AkWall e c, art "wall")
          | e ← [WallNE, WallNW, WallSE, WallSW]
          , c ← [ WallCaps l r | l ← [False, True], r ← [False, True] ] ]
    }

wireRegistration ∷ PackArtRegistration
wireRegistration = PackArtRegistration
    { parPack = fixtureWirePack
    , parKinds = [(KWire, True, Just fixtureWireCost)]
    , parEntries = [ (AkWire s, art "wire") | s ← allWireShapes ]
    }

-- | One art slot. The paths and handles are arbitrary but must be
--   non-empty and positive — 'registerPackArt' refuses anything else,
--   which is the point of going through it.
art ∷ Text → PieceArt
art role = PieceArt
    { paTexture = ArtAsset (role <> ".png") (TextureHandle 41)
    , paFacemap = ArtAsset (role <> "_face.png") (TextureHandle 42)
    }

-- * Engine scene

-- | The item definitions the fixture's build costs name. Registered so
--   a REFUND can actually mint something: 'materializeItem' is the one
--   mint boundary, and it answers nothing for a def that does not exist.
fixtureItems ∷ ItemManager
fixtureItems = ItemManager $ HM.fromList
    [ (n, itemDef n) | n ← ["steel_plate", "steel_bar", "wood_log", "wiring"] ]

itemDef ∷ Text → ItemDef
itemDef name = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0
    , idIconTexture = TextureHandle 0
    , idWeight = 1.0, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = "misc"
    , idCategory = "Materials", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

-- | One unit on @page@ holding exactly @inv@ — the claimant a payment
--   example charges. Distinctive quality/condition so a RESTORED
--   instance can be told apart from a freshly minted one.
payerUnit ∷ WorldPageId → [ItemInstance] → UnitInstance
payerUnit page inv = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = []
    , uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- | An item instance with a recognisable identity, so an example can
--   prove the SAME instance came back rather than an equal-looking one.
fixtureItem ∷ Text → Word64 → ItemInstance
fixtureItem name iid = ItemInstance
    { iiDefName = name, iiCurrentFill = 0
    , iiQuality = 37, iiCondition = 61
    , iiWeight = 1.0, iiSharpness = 100
    , iiContents = [], iiInstanceId = iid
    , iiTemp = Nothing, iiBulk = Just 1, iiStorage = Nothing
    }

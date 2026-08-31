{-# LANGUAGE Strict #-}
-- | Nested item-container contents through the REGISTERED Lua API
--   (#1238, epic #1013 phase 1): the two reads the container-window
--   stack opens an item-container level from.
--
--   Driven through @executeDebugLua@ against a real registered API and
--   real live refs, not against the pure helpers underneath, because
--   what the window stack actually depends on is the VERB: its argument
--   shapes, its refusals, its row fields, and — for the building side —
--   the promise that reading a memory neither reveals nor mutates
--   anything. A test that reached into @crItems@ directly would prove
--   none of that.
--
--   The building half is deliberately filed under the @Container
--   knowledge@ describe so #1087's own gate covers it:
--   @cabal test synarchy-test-headless
--   --test-options='--match "Container knowledge"'@. The unit half runs
--   under @--match "Nested item contents"@.
module Test.Headless.Item.NestedContents (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Building.Knowledge (ContainerKnowledge, ckRecords, crRevealedAt)
import Building.Knowledge.Live (ContainerObserver(..), revealContainer)
import Engine.Core.ReadOnlyRef (toReadOnlyRef)
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types
    ( ItemContainer(..), ItemDef(..), ItemInstance(..), ItemManager(..) )
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )

-- * Fixture identities

page ∷ WorldPageId
page = WorldPageId "nested_contents_page"

cargoBid ∷ BuildingId
cargoBid = BuildingId 1

holderUid ∷ UnitId
holderUid = UnitId 1

-- | The observation clock. Live storage is mutated AFTER the reveal and
--   the clock advanced, so any answer carrying this timestamp — and
--   these contents — provably came from the memory.
revealTime, laterTime ∷ Double
revealTime = 1000
laterTime  = 9999

-- * Item fixtures

mkItem ∷ Text → Word64 → [ItemInstance] → ItemInstance
mkItem name iid contents = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 0.5
    , iiSharpness   = 0
    , iiContents    = contents
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

bareItemDef ∷ Text → Text → ItemDef
bareItemDef name kind = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0
    , idWeight = 0.5, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = kind
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = if kind ≡ "container"
        then Just ItemContainer { icCapacity = 10, icHolds = "supplies"
                                , icFillWeight = 0.5, icDefaultFill = 0 }
        else Nothing
    , idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

testItems ∷ ItemManager
testItems = ItemManager $ HM.fromList
    [ ("first_aid_kit", bareItemDef "first_aid_kit" "container")
    , ("toolbox",       bareItemDef "toolbox"       "container")
    , ("pouch",         bareItemDef "pouch"         "container")
    , ("bandage",       bareItemDef "bandage"       "misc")
    , ("antiseptic",    bareItemDef "antiseptic"    "misc")
    , ("wrench",        bareItemDef "wrench"        "tool")
    ]

-- | Two SAME-DEF kits with distinguishable nested contents. Kit 100
--   additionally holds a container of its own, so a two-step descent
--   has somewhere to go.
kitA, kitB ∷ ItemInstance
kitA = mkItem "first_aid_kit" 100
    [ mkItem "bandage" 101 []
    , mkItem "bandage" 102 []
    , mkItem "toolbox" 103 [ mkItem "wrench" 104 [] ]
    ]
kitB = mkItem "first_aid_kit" 200 [ mkItem "antiseptic" 201 [] ]

-- * Building fixtures

cargoDef ∷ BuildingDef
cargoDef = BuildingDef
    { bdName = "cargo_hold_S", bdDisplayName = "Cargo Hold"
    , bdCategory = "Storage", bdDescription = ""
    , bdTexture = TextureHandle 0
    , bdTileW = 1, bdTileH = 1, bdPlacement = "flat_ground"
    , bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 100
    , bdOperations = [], bdAnimations = HM.empty
    , bdStateAnims = HM.empty, bdPowerDrain = 0, bdPowerNode = Nothing
    }

mkBuilding ∷ [ItemInstance] → BuildingInstance
mkBuilding storage = BuildingInstance
    { biDefName = "cargo_hold_S", biPage = page, biTexture = TextureHandle 0
    , biAnchorX = 0, biAnchorY = 0, biGridZ = 0, biSpawnedAt = 0
    , biTileW = 1, biTileH = 1, biSpawnRemaining = 0
    , biBuildProgress = 0
    , biMaterialsDelivered = HM.empty
    , biStorage = storage
    }

-- * Unit fixtures

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

-- | The holder carries one kit LOOSE, wears another in an equipment
--   slot, and has a pouch as an accessory — the three locations the
--   unit-info inventory list merges and offers \"Contents\" from.
holder ∷ UnitInstance
holder = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty
    , uiInventory = [ mkItem "first_aid_kit" 300
                        [ mkItem "toolbox" 301 [ mkItem "wrench" 302 [] ] ] ]
    , uiEquipment = HM.singleton "backpack"
                        (mkItem "first_aid_kit" 400
                            [ mkItem "antiseptic" 401 [] ])
    , uiAccessories = [ mkItem "pouch" 500 [ mkItem "bandage" 501 [] ] ]
    , uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- * Scene

-- | One page holding the cargo and the unit, with the item registry and
--   game clock the reads consult. Returns the page's 'WorldState' so a
--   scenario can read its knowledge record back.
resetScene ∷ EngineEnv → IO WorldState
resetScene env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(page, ws)], wmVisible = [page] }
    writeIORef (itemManagerRef env) testItems
    writeIORef (gameTimeRef env) revealTime
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs      = HM.singleton "cargo_hold_S" cargoDef
        , bmInstances = HM.singleton cargoBid (mkBuilding [kitA, kitB])
        }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" minimalUnitDef
        , umInstances = HM.singleton holderUid holder
        }
    pure ws

observerFor ∷ EngineEnv → ContainerObserver
observerFor env = ContainerObserver
    { coBuildings = buildingManagerRef env
    , coWorlds    = worldManagerRef env
    , coItems     = toReadOnlyRef (itemManagerRef env)
    , coGameTime  = gameTimeRef env
    }

-- | Take the one observation this whole module reads back, then move
--   the world on underneath it: live storage is emptied and the clock
--   advanced WITHOUT a second reveal, so every remembered answer below
--   is provably the memory rather than a live read that happens to
--   agree.
observeThenDiverge ∷ EngineEnv → IO ()
observeThenDiverge env = do
    ok ← revealContainer (observerFor env) cargoBid
    ok `shouldBe` True
    modifyIORef' (buildingManagerRef env) $ \bm →
        bm { bmInstances = HM.adjust (\i → i { biStorage = [] })
                                     cargoBid (bmInstances bm) }
    writeIORef (gameTimeRef env) laterTime

knowledgeOf ∷ WorldState → IO ContainerKnowledge
knowledgeOf ws = readIORef (wsContainerKnowledgeRef ws)

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    loaded ← executeDebugLua (lbsLuaState ls) formatterLua
    loaded `shouldNotSatisfy` isLuaError
    pure ls

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Both verbs answer the SAME grouped row shape, so one formatter
--   flattens either into a comparable string.
--
--   @__rows(t)@ renders a bare row array as
--   @def:count:kind:instanceId@ entries, sorted so the grouping's
--   hashmap enumeration order cannot decide a test. @__mem(t)@ adds the
--   remembered read's @revealedAt@ in front. @nil@ for a refusal, which
--   is a DIFFERENT answer from an empty container's @|@ /
--   @<time>|@ — the distinction every refusal case below turns on.
formatterLua ∷ Text
formatterLua = T.concat
    [ "_G.__rows = function(t) "
    , "  if t == nil then return 'nil' end; "
    , "  local parts = {}; "
    , "  for _, r in ipairs(t) do "
    , "    parts[#parts+1] = string.format('%s:%d:%s:%d', tostring(r.defName), "
    , "      r.count or 0, tostring(r.kind), r.instanceId or -1) "
    , "  end; "
    , "  table.sort(parts); "
    , "  return table.concat(parts, ',') "
    , "end; "
    , "_G.__mem = function(res) "
    , "  if res == nil then return 'nil' end; "
    , "  return tostring(res.revealedAt) .. '|' .. __rows(res.items) "
    , "end; "
    , "return 'ok'"
    ]

-- | Debug-console return values come back JSON-encoded, so a Lua string
--   arrives quoted.
q ∷ Text → Text
q t = "\"" <> t <> "\""

run ∷ LuaBackendState → Text → IO Text
run ls = executeDebugLua (lbsLuaState ls)

-- * Spec

spec ∷ SpecWith EngineEnv
spec = do

    describe "Container knowledge" $
      describe "the remembered nested-contents read (#1238)" $ do

        it "answers an item-container's REMEMBERED contents and the \
           \parent record's own revealedAt, long after live storage \
           \stopped agreeing" $ \env → do
            _ ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            r ← run ls "return __mem(building.getRememberedItemContents(1, {100}))"
            r `shouldBe` q "1000.0|bandage:2:misc:102,toolbox:1:container:103"

        it "selects by EXACT instance identity: two same-def kits \
           \answer with their own contents, never each other's" $ \env → do
            _ ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            a ← run ls "return __rows(building.getRememberedItemContents(1, {100}).items)"
            b ← run ls "return __rows(building.getRememberedItemContents(1, {200}).items)"
            a `shouldBe` q "bandage:2:misc:102,toolbox:1:container:103"
            b `shouldBe` q "antiseptic:1:misc:201"

        it "descends a multi-step path through nested containers" $ \env → do
            _ ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            r ← run ls "return __rows(building.getRememberedItemContents(1, {100, 103}).items)"
            r `shouldBe` q "wrench:1:tool:104"

        it "a path that mixes two kits' ids does not resolve — it never \
           \falls back to a same-def sibling" $ \env → do
            _ ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            -- 103 is kit A's toolbox; asking for it inside kit B must
            -- be a refusal, not kit A's answer.
            r ← run ls "return __mem(building.getRememberedItemContents(1, {200, 103}))"
            r `shouldBe` q "nil"

        it "refuses an unresolvable, empty, absent or malformed path, \
           \and an unknown building — each nil rather than a prefix's \
           \contents" $ \env → do
            _ ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            let refusal expr = do
                    r ← run ls ("return __mem(" <> expr <> ")")
                    r `shouldBe` q "nil"
            refusal "building.getRememberedItemContents(1, {999})"
            refusal "building.getRememberedItemContents(1, {})"
            refusal "building.getRememberedItemContents(1)"
            refusal "building.getRememberedItemContents(1, 100)"
            refusal "building.getRememberedItemContents(1, {'100'})"
            -- A HOLE would let rawlen report a short border, and a
            -- truncated path names a DIFFERENT container.
            refusal "building.getRememberedItemContents(1, {[1]=100,[3]=103})"
            refusal "building.getRememberedItemContents(77, {100})"

        it "reading reveals NOTHING: the whole knowledge record is \
           \byte-identical before and after, and still describes the \
           \storage that has since been emptied" $ \env → do
            ws ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            before ← knowledgeOf ws
            _ ← run ls "return __mem(building.getRememberedItemContents(1, {100}))"
            _ ← run ls "return __mem(building.getRememberedItemContents(1, {100, 103}))"
            _ ← run ls "return __mem(building.getRememberedItemContents(1, {200}))"
            after ← knowledgeOf ws
            after `shouldBe` before
            -- And the observation time never moved, even though the
            -- clock did.
            map crRevealedAt (HM.elems (ckRecords after))
                `shouldBe` [revealTime]

        it "a live storage change after the observation cannot reach \
           \the answer — the top-level read and the nested read agree \
           \on that" $ \env → do
            _ ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            -- Live storage is empty by now; refill it with something
            -- else entirely and neither read may notice.
            writeIORef (buildingManagerRef env) emptyBuildingManager
                { bmDefs      = HM.singleton "cargo_hold_S" cargoDef
                , bmInstances = HM.singleton cargoBid
                    (mkBuilding [ mkItem "first_aid_kit" 100
                                    [ mkItem "wrench" 999 [] ] ])
                }
            r ← run ls "return __mem(building.getRememberedItemContents(1, {100}))"
            r `shouldBe` q "1000.0|bandage:2:misc:102,toolbox:1:container:103"

        it "every row carries the kind and representative instance id a \
           \deeper level is opened with" $ \env → do
            _ ← resetScene env
            observeThenDiverge env
            ls ← newBareLuaBackend env
            r ← run ls $ T.concat
                [ "local res = building.getRememberedItemContents(1, {100}); "
                , "for _, row in ipairs(res.items) do "
                , "  if row.defName == 'toolbox' then "
                , "    return row.kind .. ':' .. tostring(row.instanceId) "
                , "  end "
                , "end; "
                , "return 'missing'" ]
            r `shouldBe` q "container:103"

    describe "Nested item contents" $
      describe "a unit's own containers (#1238)" $ do

        it "resolves a container the unit is CARRYING, one it has \
           \EQUIPPED, and one worn as an ACCESSORY — the three \
           \locations the unit-info list merges" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            carried ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 300))"
            worn    ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 400))"
            acc     ← run ls "return __rows(unit.getItemContents(1, 'pouch', 500))"
            carried `shouldBe` q "toolbox:1:container:301"
            worn    `shouldBe` q "antiseptic:1:misc:401"
            acc     `shouldBe` q "bandage:1:misc:501"

        it "descends a nested path by exact instance identity, and \
           \refuses a path taken from a DIFFERENT same-def container" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            nested ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 300, {301}))"
            wrong  ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 400, {301}))"
            nested `shouldBe` q "wrench:1:tool:302"
            wrong  `shouldBe` q "nil"

        it "keeps the by-defName fallback answering the FIRST held \
           \match, which is loose inventory before equipment" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            r ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit'))"
            r `shouldBe` q "toolbox:1:container:301"

        it "refuses a malformed path rather than descending a prefix \
           \of it" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            a ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 300, 301))"
            b ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 300, {'301'}))"
            c ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 300, {[1]=301,[3]=1}))"
            a `shouldBe` q "nil"
            b `shouldBe` q "nil"
            c `shouldBe` q "nil"

        it "an existing-but-empty container answers an EMPTY LIST while \
           \a missing one answers nil — the distinction the level's own \
           \close-on-nil rule turns on" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            empty   ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 300, {301, 302}))"
            missing ← run ls "return __rows(unit.getItemContents(1, 'first_aid_kit', 12345))"
            empty   `shouldBe` q ""
            missing `shouldBe` q "nil"

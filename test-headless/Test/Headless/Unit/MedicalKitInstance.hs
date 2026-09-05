-- | Medical supply discovery resolves the container the inventory row
--   NAMES, not the first same-definition item the unit happens to hold
--   (#2302).
--
--   @unit.getItemContents@ takes an optional instance id and, without
--   one, answers for the FIRST held item matching @defName@ (#67,
--   'Engine.Scripting.Lua.API.Units.Inventory'). Both medical supply
--   scans — @scripts/unit_ai_medic@'s own-kit check and
--   @scripts/init_context_menu@'s two enablement predicates — walked the
--   per-instance inventory rows and then threw the identity away, so
--   with two same-definition kits every iteration re-read the first one.
--   An empty kit ordered before a stocked one made the medic believe it
--   carried nothing (fetch, or improvise a tourniquet) and greyed both
--   treatment rows — while @treatBleeding@ / @treatInfection@ scan the
--   real containers and take the first STOCKED one, and would have
--   committed. Two same-definition kits need no malformed state: loot,
--   technomule stocking (#1855) and ordinary transfers all produce them.
--
--   Nothing here stubs an inventory projection. The fixture registers
--   real 'ItemDef's so the REAL @unit.getInventory@ /
--   @unit.getItemContents@ / @unit.transferItemToUnit@ run against live
--   manager refs, and the first case pins the engine's own
--   omitted-id resolution rule so every case below is measured against
--   the aliasing it exists to defeat rather than a restatement of it.
--
--   The medic block drives the SHIPPED @treatExecute@. Only the two
--   things a sibling AI gate already stubs are stubbed — the
--   @scripts.unit_ai@ singleton table the submodules attach to, and
--   @scripts.movement_speed@, whose real pace answer reaches the whole
--   physiology chain — so the kit scan, the holder search, the fetch and
--   the treatment are all production code.
--
--   Fixture shape and the context-menu spy are
--   'Test.Headless.Unit.MedicalReach''s, built from
--   'Test.Headless.Unit.TransferApi''s unit / item constructors.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "medical kit instance targeting"'@.
module Test.Headless.Unit.MedicalKitInstance (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types
    (ItemDef(..), ItemInstance(..), ItemManager(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..), Wound(..)
    , emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)
import Test.Headless.Unit.TransferApi
    (evalDebug, mkItem, mkUnit, minimalDef, newBareLuaBackend)

-- * Fixture identities

-- | One live page is enough: nothing here is a page case, and the
--   medic, its patient and the holder all stand within
--   'Unit.Medical.Reach.treatmentRange' of one another so a greyed row
--   can only be a supply verdict.
fixturePage ∷ WorldPageId
fixturePage = WorldPageId "medical_kit_instance_page"

-- | uid 1 — the medic, carrying an EMPTY kit and then a stocked one of
--   the same definition. Every case that must find supplies finds them
--   in the second.
medicUid ∷ UnitId
medicUid = UnitId 1

-- | uid 2 — the patient, one tile east: bleeding AND infected, so the
--   two menu rows and the two treatment verbs all have something to do.
patientUid ∷ UnitId
patientUid = UnitId 2

-- | uid 3 — the holder (the technomule case), one tile north, stocked
--   in its SECOND kit exactly like the medic.
holderUid ∷ UnitId
holderUid = UnitId 3

-- | uid 4 — the single-kit control: one stocked kit and nothing else,
--   the common case requirement 6 protects. It stands on the patient's
--   FAR side: in range of the patient for its own menu cases, but
--   strictly farther from the medic than 'holderUid', so it can never
--   outrank the holder the fetch cases are about.
soloUid ∷ UnitId
soloUid = UnitId 4

-- | uid 5 — the negative control: two same-definition kits, BOTH empty.
--   Without it every "enabled" assertion below could pass on a scan
--   that simply answered yes. It carries no supplies, so it is never a
--   holder candidate either.
barrenUid ∷ UnitId
barrenUid = UnitId 5

-- * Item fixtures

-- | The kit def. @idKind = "container"@ is load-bearing: it is the one
--   field both supply scans branch on to descend into a row at all.
kitDef ∷ ItemDef
kitDef = ItemDef
    { idName = "first_aid_kit", idDisplayName = "First Aid Kit"
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.5, idWeightSpec = Nothing, idBulk = 4.0
    , idStorage = Nothing, idKind = "container", idCategory = "Medical"
    , idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture" }

supplyDef ∷ Text → Text → ItemDef
supplyDef name display = kitDef
    { idName = name, idDisplayName = display, idWeight = 0.05
    , idBulk = 0.1, idKind = "misc" }

fixtureItems ∷ ItemManager
fixtureItems = ItemManager $ HM.fromList
    [ ("first_aid_kit", kitDef)
    , ("bandage", supplyDef "bandage" "Bandage")
    , ("antibiotics", supplyDef "antibiotics" "Antibiotics")
    , ("tweezers", supplyDef "tweezers" "Tweezers")
    , ("scissors", supplyDef "scissors" "Scissors")
    , ("antiseptic", supplyDef "antiseptic" "Antiseptic") ]

-- | A kit holding nothing at all. Same definition and same weight as
--   the stocked one, so only its CONTENTS distinguish the two.
emptyKit ∷ Word64 → ItemInstance
emptyKit iid = mkItem "first_aid_kit" iid 0.5

-- | A stocked kit: three bandages, both tools, antiseptic, and
--   antibiotics filled to five pills — five times
--   'Engine.Scripting.Lua.API.Units.Medical.antibioticsDose', so the
--   infection row's enablement can never turn on a sub-dose bottle.
stockedKit ∷ Word64 → ItemInstance
stockedKit base = (mkItem "first_aid_kit" base 0.5)
    { iiContents =
        [ mkItem "bandage" (base + 1) 0.05
        , mkItem "bandage" (base + 2) 0.05
        , mkItem "bandage" (base + 3) 0.05
        , mkItem "tweezers" (base + 4) 0.05
        , mkItem "scissors" (base + 5) 0.1
        , (mkItem "antiseptic" (base + 6) 0.12) { iiCurrentFill = 5 }
        , (mkItem "antibiotics" (base + 7) 0.2) { iiCurrentFill = 5 }
        ] }

-- | The instance ids the cases name. The stocked kit is always the
--   SECOND row, so an assertion naming one of these is an assertion
--   that the aliasing did not happen.
medicEmptyId, medicStockedId, holderEmptyId, holderStockedId ∷ Word64
medicEmptyId    = 100
medicStockedId  = 200
holderEmptyId   = 300
holderStockedId = 400

-- * Unit fixtures

acolyteDef ∷ UnitDef
acolyteDef = minimalDef "acolyte" "Acolyte"

-- | One slash that is both seeping and infected past the cure
--   threshold, so one patient drives both menu rows and both verbs.
openInfectedWound ∷ Wound
openInfectedWound = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 0.5
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.6, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

acolyte ∷ (Float, Float) → [(Text, Float)] → [ItemInstance] → [Wound]
        → UnitInstance
acolyte xy knowledge inv wounds =
    (mkUnit "acolyte" FactionPlayer xy 100 inv [])
        { uiPage = fixturePage
        , uiKnowledge = HM.fromList knowledge
        , uiWounds = wounds }

medicKnowledge ∷ [(Text, Float)]
medicKnowledge = [("bleed_control", 100), ("infection_control", 100)]

-- | Rebuild the live page, the item registry and the roster. Every
--   case starts here: a spent bandage or a moved kit from a previous
--   one would read as a silently-passing assertion.
resetFixture ∷ EngineEnv → IO ()
resetFixture env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" acolyteDef
        , umInstances = HM.fromList
            [ (medicUid, acolyte (10, 10) medicKnowledge
                                 [emptyKit medicEmptyId
                                 , stockedKit medicStockedId] [])
            , (patientUid, acolyte (11, 10) [] [] [openInfectedWound])
            , (holderUid, acolyte (10, 11) []
                                  [emptyKit holderEmptyId
                                  , stockedKit holderStockedId] [])
            , (soloUid, acolyte (12, 10) medicKnowledge
                                [stockedKit 500] [])
            , (barrenUid, acolyte (11, 11) medicKnowledge
                                  [emptyKit 600, emptyKit 700] []) ] }

-- * Live-state readers

-- | The instance ids of the kits a unit is holding, in inventory order.
kitIds ∷ EngineEnv → UnitId → IO [Word64]
kitIds env u = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup u (umInstances um) of
        Nothing → []
        Just inst → map iiInstanceId (uiInventory inst)

-- | How many bandages the unit is carrying across every kit it holds.
bandagesOn ∷ EngineEnv → UnitId → IO Int
bandagesOn env u = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup u (umInstances um) of
        Nothing → -1
        Just inst → length
            [ () | it ← uiInventory inst, c ← iiContents it
                 , iiDefName c ≡ "bandage" ]

-- | The patient's worst wound dressing (1.0 = untreated) and infection.
--   A committed treatment moves one or both; the tourniquet fallback
--   moves the dressing without spending a bandage, so both are read.
worstWound ∷ EngineEnv → UnitId → IO (Float, Float)
worstWound env u = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup u (umInstances um) of
        Nothing → (-1, -1)
        Just inst → case uiWounds inst of
            [] → (-1, -1)
            (w : _) → (woundBandage w, woundInfection w)

-- * Lua plumbing

uid ∷ UnitId → Text
uid (UnitId n) = T.pack (show n)

wid ∷ Word64 → Text
wid = T.pack ∘ show

-- | Debug-console returns are JSON-encoded, so a Lua string arrives
--   quoted (the sibling specs' convention).
q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | Run a setup statement and fail loudly if it errors — a broken stub
--   must never masquerade as a disabled row or an absent fetch.
run ∷ LuaBackendState → Text → IO ()
run ls stmt = do
    r ← evalDebug ls stmt
    r `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t
                             ∨ "syntax error:" `T.isPrefixOf` t)

-- * The context-menu surface

-- | Open the real unit context menu over @target@ with @selection@
--   selected, recording every row's label and enabled state. Unlike
--   'Test.Headless.Unit.MedicalReach', the inventory projections are
--   NOT stubbed: supply detection is exactly what is under test here.
menuOn ∷ EngineEnv → UnitId → [UnitId] → IO LuaBackendState
menuOn env target selection = do
    resetFixture env
    ls ← newBareLuaBackend env
    run ls (menuSetup target selection)
    opened ← evalDebug ls
        "return tostring(require('scripts.init_context_menu').tryUnitMenu(10, 20))"
    opened `shouldBe` q "true"
    pure ls

menuSetup ∷ UnitId → [UnitId] → Text
menuSetup target selection = T.concat
    [ "local contextMenu = require('scripts.ui.context_menu'); "
    , "_G.__rows = {}; "
    , "contextMenu.show = function(items) "
    , "  for _, it in ipairs(items) do "
    , "    _G.__rows[it.label] = tostring(it.enabled ~= false) end "
    , "end; "
    , "unit.hitTestAt = function() return ", uid target, " end; "
    , "unit.getSelected = function() return {"
    , T.intercalate ", " (map uid selection), "} end; "
    , "faction.isPlayerCommandable = function() return true end; "
    , "faction.canAttack = function() return true end; "
    ]

-- | One row's enabled state, or @nil@ when the menu never offered it.
rowState ∷ LuaBackendState → Text → IO Text
rowState ls label =
    evalDebug ls ("return tostring(_G.__rows and _G.__rows['" <> label <> "'])")

-- * The medic surface

-- | A Lua state with the shipped medic module loaded and a SPY wrapped
--   around @unit.transferItemToUnit@ that records its fourth argument
--   and then calls the real verb, so a case can assert both the exact
--   instance id the AI named and the item state that resulted.
medicBackend ∷ EngineEnv → IO LuaBackendState
medicBackend env = do
    resetFixture env
    ls ← newBareLuaBackend env
    run ls medicSetup
    pure ls

medicSetup ∷ Text
medicSetup = T.concat
    -- The singleton the AI submodules attach to, and the pace answer
    -- whose real module reaches the whole physiology chain: the two
    -- stubs 'Test.Headless.Lua.UnitAiPageTargets' already makes.
    [ "package.loaded['scripts.unit_ai'] ="
    , "  package.loaded['scripts.unit_ai'] or {}; "
    , "package.loaded['scripts.movement_speed'] = "
    , "  { comfort = function() return 1.0 end, "
    , "    ordered = function() return 1.0 end, "
    , "    sprint  = function() return 1.0 end }; "
    , "_G.__moves, _G.__transfers = 0, {}; "
    , "local realMove = unit.moveTo; "
    , "unit.moveTo = function(...) "
    , "  _G.__moves = _G.__moves + 1; return realMove(...) end; "
    , "local realTransfer = unit.transferItemToUnit; "
    , "unit.transferItemToUnit = function(from, to, defName, instanceId) "
    , "  _G.__transfers[#_G.__transfers + 1] = tostring(instanceId); "
    , "  return realTransfer(from, to, defName, instanceId) end; "
    , "_G.__medic = require('scripts.unit_ai_medic'); "
    , "_G.__params = require('scripts.unit_ai_tunables').acolyte; "
    ]

-- | Drive one @treatExecute@ tick with the claim already locked onto
--   the patient, so the case observes the supply phase rather than
--   re-deriving the utility ranking that reaches it.
treatTick ∷ LuaBackendState → UnitId → UnitId → IO ()
treatTick ls medic patient = run ls $ T.concat
    [ "_G.__state = _G.__state or { treatClaim = { patient = "
    , uid patient, " } }; "
    , "_G.__medic.treatExecute(", uid medic, ", _G.__state, _G.__params)" ]

-- | Every recorded fourth argument, comma-joined — @""@ when the AI
--   never fetched at all.
transfers ∷ LuaBackendState → IO Text
transfers ls =
    evalDebug ls "return table.concat(_G.__transfers, ',')"

-- | Empty a unit's inventory in place: the medic that must go and fetch.
stripKits ∷ EngineEnv → UnitId → IO ()
stripKits env u = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup u (umInstances um) of
        Nothing → pure ()
        Just inst → writeIORef (unitManagerRef env) um
            { umInstances = HM.insert u (inst { uiInventory = [] })
                                      (umInstances um) }

spec ∷ SpecWith EngineEnv
spec = describe "medical kit instance targeting (#2302)" $ do

    -- The premise, measured against the real engine rather than
    -- asserted from the haddock: an omitted instance id resolves to the
    -- FIRST same-definition kit, so a scan that walks the rows and asks
    -- by definition alone reads the empty one every time.
    describe "the engine's own omitted-id resolution" $ do

        it "answers for the first same-definition kit, and only an id reaches the second" $ \env → do
            resetFixture env
            ls ← newBareLuaBackend env
            run ls bandageCounter
            bandageCount ls medicUid Nothing `shouldReturn` q "0"
            bandageCount ls medicUid (Just medicEmptyId) `shouldReturn` q "0"
            bandageCount ls medicUid (Just medicStockedId) `shouldReturn` q "3"

    describe "the context menu" $ do

        it "offers Treat bleeding enabled when the stocked kit is not the first" $ \env → do
            ls ← menuOn env patientUid [medicUid]
            rowState ls "Treat bleeding" `shouldReturn` q "true"

        it "offers Treat infection enabled when the stocked kit is not the first" $ \env → do
            ls ← menuOn env patientUid [medicUid]
            rowState ls "Treat infection" `shouldReturn` q "true"

        -- The negative control for both rows above: with the SAME
        -- fixture and the same two-kit shape, a selection whose kits
        -- are all empty still greys them. Without this a scan that
        -- answered "yes" unconditionally would pass every case here.
        it "greys both rows when every kit the selection carries is empty" $ \env → do
            ls ← menuOn env patientUid [barrenUid]
            rowState ls "Treat bleeding" `shouldReturn` q "false"
            rowState ls "Treat infection" `shouldReturn` q "false"

        -- Requirement 6: the common case is one kit, and it is
        -- unchanged.
        it "still offers both rows for a medic holding a single stocked kit" $ \env → do
            ls ← menuOn env patientUid [soloUid]
            rowState ls "Treat bleeding" `shouldReturn` q "true"
            rowState ls "Treat infection" `shouldReturn` q "true"

        -- A third-party supplier is resolved the same way the medic is,
        -- so the technomule whose stocked kit is second is a kit owner.
        it "resolves a selected supplier whose stocked kit is not its first" $ \env → do
            ls ← menuOn env patientUid [barrenUid, holderUid]
            rowState ls "Treat bleeding" `shouldReturn` q "true"
            rowState ls "Treat infection" `shouldReturn` q "true"

    describe "the autonomous medic" $ do

        -- Requirement 4, through the shipped execution path: the medic
        -- already carries the supplies, so it must neither fetch nor
        -- improvise. A committed dressing is the positive half — the
        -- tourniquet fallback would move the dressing WITHOUT spending
        -- a bandage, so both are read.
        it "treats from a stocked kit it already carries, and never fetches" $ \env → do
            ls ← medicBackend env
            treatTick ls medicUid patientUid
            transfers ls `shouldReturn` q ""
            bandagesOn env medicUid `shouldReturn` 2
            (dressing, _) ← worstWound env patientUid
            dressing `shouldSatisfy` (< 1.0)
            -- The kit it drew from is still the one it chose.
            kitIds env medicUid `shouldReturn` [medicEmptyId, medicStockedId]

        -- Requirement 5: the holder's stocked kit is its SECOND, so a
        -- definition-keyed scan would not have made it a candidate.
        it "finds a holder whose stocked kit is not its first" $ \env → do
            ls ← medicBackend env
            stripKits env medicUid
            found ← evalDebug ls
                (T.concat [ "local h = _G.__medic.findKitHolder("
                          , uid medicUid, ", 10, 10); "
                          , "return tostring(h and h.uid) .. '|' "
                          , ".. tostring(h and h.kit) .. '|' "
                          , ".. tostring(h and h.kitInstance)" ])
            found `shouldBe` q (uid holderUid <> "|first_aid_kit|"
                                <> wid holderStockedId)

        -- Requirement 2: the fetch names the instance discovery chose.
        -- The by-definition form pops the holder's FIRST kit — the
        -- empty one — so the assertion on the arrival is what the
        -- fourth argument is actually for.
        it "fetches the stocked sibling by exact instance id" $ \env → do
            ls ← medicBackend env
            stripKits env medicUid
            treatTick ls medicUid patientUid
            transfers ls `shouldReturn` q (wid holderStockedId)
            kitIds env medicUid `shouldReturn` [holderStockedId]
            kitIds env holderUid `shouldReturn` [holderEmptyId]
            bandagesOn env medicUid `shouldReturn` 3

-- | @unit.getItemContents@ folded to a bandage count, so a case can name
--   the kit it is asking about and read one number back.
bandageCounter ∷ Text
bandageCounter = T.concat
    [ "_G.__bandages = function(u, iid) "
    , "  local n = 0; "
    , "  for _, r in ipairs(unit.getItemContents(u, 'first_aid_kit', iid) "
    , "                     or {}) do "
    , "    if r.defName == 'bandage' then n = n + (r.count or 0) end end; "
    , "  return n end; " ]

bandageCount ∷ LuaBackendState → UnitId → Maybe Word64 → IO Text
bandageCount ls u mIid = evalDebug ls $ T.concat
    [ "return tostring(_G.__bandages(", uid u
    , maybe "" (\i → ", " <> wid i) mIid, "))" ]

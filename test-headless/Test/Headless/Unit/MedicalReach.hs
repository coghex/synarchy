-- | The two medical treatment verbs hold a PAGE and a RANGE floor
--   (#2297), driven through the REAL registered production API against
--   REAL manager refs.
--
--   @unit.treatBleeding@ and @unit.treatInfection@ are item-consuming
--   commits — they drop bandage instances out of a first-aid kit and
--   spend antiseptic / antibiotic doses out of it — and until #2297
--   neither read a page and neither read a position. So a caller could
--   dress a patient anywhere on the map while spending a THIRD unit's
--   supplies from anywhere else, across world pages included: the floor
--   #1673 gave the four lax AI item verbs, and these two never had.
--
--   Every refusal case asserts more than a @false@. It compares the
--   COMPLETE 'UnitManager' before and after — wounds, immune response,
--   knowledge levels, both kits' contents and their order all ride in
--   it — plus the stat RNG, and separately that the medic's
--   @intelligence@ is still UNROLLED. That last one is the ordering
--   assertion: 'Engine.Scripting.Lua.API.Units.Stats.getEffectiveStat'
--   lazily rolls a stat template, caches the value on the instance and
--   advances @statRNGRef@ doing it, so a spatial check that ran after
--   it would leave a fingerprint even though it consumed no supplies.
--   The fixture's acolyte def declares an @intelligence@ template with
--   a real spread precisely so both halves of that are observable.
--
--   The same-page in-range control on each verb is load-bearing in the
--   other direction: it proves the new floor is the ONLY new policy and
--   that these fixtures can treat at all.
--
--   The context-menu block drives the REAL @scripts/init_context_menu@
--   against the same live fixture, so its two medical rows are greyed
--   by the very predicate the verbs refuse on rather than by a
--   restatement of it.
--
--   Two live in-memory pages, no worldgen — the fixture shape
--   'Test.Headless.Unit.CargoApi' uses, built from
--   'Test.Headless.Unit.TransferApi''s own unit / item constructors
--   rather than a third copy free to drift from what the projections
--   actually read.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit medical reach"'@.
module Test.Headless.Unit.MedicalReach (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..), emptyItemManager)
import Unit.Faction (Faction(..))
import Unit.Medical.Reach (treatmentRange)
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..), Wound(..)
    , emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)
import Test.Headless.Unit.TransferApi
    (evalDebug, mkItem, mkUnit, minimalDef, newBareLuaBackend)

-- * Fixture identities

-- | The medic's page, and a second LIVE page: the defect is a
--   cross-page COMMIT, not a dead reference, so every off-page
--   counterpart has to be a genuinely live unit on a genuinely live
--   world.
pageHome, pageAway ∷ WorldPageId
pageHome = WorldPageId "medical_reach_home"
pageAway = WorldPageId "medical_reach_away"

-- | uid 1 — the medic, on 'pageHome' at (10, 10), knowing both medical
--   skills and carrying its own stocked kit.
medicUid ∷ UnitId
medicUid = UnitId 1

-- | uid 2 — the patient, one tile east: bleeding AND infected, so a
--   spatial refusal can never be mistaken for a clinical one.
patientUid ∷ UnitId
patientUid = UnitId 2

-- | uid 3 — a THIRD-party supplier one tile north, carrying its own
--   stocked kit. This is the technomule case the context menu's own
--   comment names, and the endpoint @treatInfection@ never resolved.
supplierUid ∷ UnitId
supplierUid = UnitId 3

-- | uids 4 and 5 — page-away twins of the patient and the supplier, at
--   exactly the coordinates their same-page originals occupy, so
--   nothing but the page distinguishes them.
offPagePatientUid, offPageSupplierUid ∷ UnitId
offPagePatientUid  = UnitId 4
offPageSupplierUid = UnitId 5

-- | uids 6 and 7 — same page, same state, two tiles out: beyond
--   'treatmentRange' and nothing else.
farPatientUid, farSupplierUid ∷ UnitId
farPatientUid  = UnitId 6
farSupplierUid = UnitId 7

-- | uid 8 sits at EXACTLY 'treatmentRange'; uid 9 a hundredth of a tile
--   past it. The pair pins the boundary itself rather than a
--   comfortably-far distance.
edgePatientUid, pastEdgePatientUid ∷ UnitId
edgePatientUid     = UnitId 8
pastEdgePatientUid = UnitId 9

-- | uid 10 — off page AND carrying no wound at all. Its refusal must
--   still name the page: the spatial floor is checked before the
--   wound state, so the two cannot mask each other.
offPageHealthyUid ∷ UnitId
offPageHealthyUid = UnitId 10

-- | A uid nothing was ever spawned under.
missingUid ∷ Int
missingUid = 999

-- * Fixture construction

-- | The acolyte def, with a real @intelligence@ TEMPLATE. Rolling it is
--   the observable that separates "the reach check ran first" from "the
--   reach check ran at all": a template with a spread both caches a
--   value on the instance and advances @statRNGRef@ the first time
--   anything asks for the stat.
acolyteDef ∷ UnitDef
acolyteDef = (minimalDef "acolyte" "Acolyte")
    { udStatTemplates = HM.singleton "intelligence" (1.5, 0.4) }

-- | A stocked first-aid kit: three bandages, both tools at full
--   condition (so the treat capability is high enough that the control
--   cases cannot lose to a bad roll), and a filled antiseptic bottle
--   and antibiotics jar.
--
--   The instance ids are per-kit so the whole-manager comparison below
--   is sensitive to a supply leaving the WRONG kit as well as to one
--   leaving at all.
stockedKit ∷ Word64 → ItemInstance
stockedKit base = (mkItem "first_aid_kit" base 1.0)
    { iiContents =
        [ mkItem "bandage" (base + 1) 0.05
        , mkItem "bandage" (base + 2) 0.05
        , mkItem "bandage" (base + 3) 0.05
        , mkItem "tweezers" (base + 4) 0.05
        , mkItem "scissors" (base + 5) 0.1
        , (mkItem "antiseptic" (base + 6) 0.12) { iiCurrentFill = 5 }
        , (mkItem "antibiotics" (base + 7) 0.2) { iiCurrentFill = 5 }
        ] }

-- | One slash on the torso that is BOTH seeping (bandage 1.0, unclotted)
--   and infected past the cure threshold, so a single patient drives
--   both verbs and every case can show which refusal it got.
openInfectedWound ∷ Wound
openInfectedWound = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 0.5
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.6, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- | A live acolyte on @pg@ at @(x, y)@ with the given knowledge, kit and
--   wounds. Everything else is 'mkUnit''s own shape.
acolyte ∷ WorldPageId → (Float, Float) → [(Text, Float)] → [ItemInstance]
        → [Wound] → UnitInstance
acolyte pg xy knowledge inv wounds =
    (mkUnit "acolyte" FactionPlayer xy 100 inv [])
        { uiPage = pg
        , uiKnowledge = HM.fromList knowledge
        , uiWounds = wounds }

medicKnowledge ∷ [(Text, Float)]
medicKnowledge = [("bleed_control", 100), ("infection_control", 100)]

-- | Reset both live pages and the unit manager. Distances from the
--   medic at (10, 10): patient and supplier 1.0, the far pair 2.0, the
--   edge patient exactly 'treatmentRange', the past-edge patient a
--   hundredth beyond it.
resetPages ∷ EngineEnv → IO ()
resetPages env = do
    wsHome ← emptyWorldState
    wsAway ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageHome, wsHome), (pageAway, wsAway)]
        , wmVisible = [pageHome] }
    writeIORef (itemManagerRef env) emptyItemManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" acolyteDef
        , umInstances = HM.fromList
            [ (medicUid, acolyte pageHome (10, 10) medicKnowledge
                                 [stockedKit 100] [])
            , (patientUid, acolyte pageHome (11, 10) [] []
                                   [openInfectedWound])
            , (supplierUid, acolyte pageHome (10, 11) []
                                    [stockedKit 200] [])
            , (offPagePatientUid, acolyte pageAway (11, 10) [] []
                                          [openInfectedWound])
            , (offPageSupplierUid, acolyte pageAway (10, 11) []
                                           [stockedKit 300] [])
            , (farPatientUid, acolyte pageHome (12, 10) [] []
                                      [openInfectedWound])
            , (farSupplierUid, acolyte pageHome (10, 12) []
                                       [stockedKit 400] [])
            , (edgePatientUid,
                 acolyte pageHome (10 + treatmentRange, 10) [] []
                         [openInfectedWound])
            , (pastEdgePatientUid,
                 acolyte pageHome (10 + treatmentRange + 0.01, 10) [] []
                         [openInfectedWound])
            , (offPageHealthyUid, acolyte pageAway (11, 10) [] [] []) ]
        }

-- * Live-state readers

-- | Everything a refusal must leave alone: the WHOLE unit manager (both
--   kits' ordered contents, the wound list, immune response, knowledge
--   levels and the cached stat map all live in it), the stat RNG, and
--   whether the medic's @intelligence@ has been rolled yet.
data Session = Session
    { sesUnits     ∷ !UnitManager
    , sesStatRNG   ∷ !Text
    , sesIntRolled ∷ !Bool
    } deriving (Eq, Show)

snapshot ∷ EngineEnv → IO Session
snapshot env = do
    um ← readIORef (unitManagerRef env)
    g  ← readIORef (statRNGRef env)
    pure Session
        { sesUnits = um
        , sesStatRNG = T.pack (show g)
        , sesIntRolled = maybe False (HM.member "intelligence" ∘ uiStats)
                               (HM.lookup medicUid (umInstances um)) }

-- * Lua plumbing

-- | @ok|message@ for one treat call, so a case pins WHICH refusal it
--   got rather than only that it was refused.
treatResult ∷ LuaBackendState → Text → IO Text
treatResult ls call = evalDebug ls $ T.concat
    [ "local r = ", call
    , "; return tostring(r and r.ok) .. '|' .. tostring(r and r.message)" ]

-- | @ok|message@ for @unit.canTreat@, in the same shape, so the two can
--   be compared directly.
canTreat ∷ LuaBackendState → Text → IO Text
canTreat ls args = evalDebug ls $ T.concat
    [ "local ok, msg = unit.canTreat(", args
    , "); return tostring(ok) .. '|' .. tostring(msg)" ]

uid ∷ UnitId → Text
uid (UnitId n) = T.pack (show n)

-- | Debug-console returns are JSON-encoded, so a Lua string arrives
--   quoted (the sibling specs' convention).
q ∷ Text → Text
q t = "\"" <> t <> "\""

bleed ∷ UnitId → Maybe UnitId → Text
bleed patient mOwner = T.concat
    [ "unit.treatBleeding(", uid medicUid, ", ", uid patient
    , maybe "" (\o → ", " <> uid o) mOwner, ")" ]

infect ∷ UnitId → Maybe UnitId → Text
infect patient mOwner = T.concat
    [ "unit.treatInfection(", uid medicUid, ", ", uid patient
    , maybe "" (\o → ", " <> uid o) mOwner, ")" ]

-- | Every refusal case is the same shape: snapshot, call, assert the
--   message, assert the session is byte-identical.
refuses ∷ EngineEnv → Text → Text → IO ()
refuses env call message = do
    resetPages env
    ls ← newBareLuaBackend env
    before ← snapshot env
    treatResult ls call `shouldReturn` q ("false|" <> message)
    snapshot env `shouldReturn` before

spec ∷ SpecWith EngineEnv
spec = describe "Unit medical reach (page + range, #2297)" $ do

    describe "unit.treatBleeding refuses, leaving the session untouched" $ do

        it "a cross-page patient" $ \env →
            refuses env (bleed offPagePatientUid Nothing)
                    "patient is on another world page"

        it "a cross-page third-party kit owner" $ \env →
            refuses env (bleed patientUid (Just offPageSupplierUid))
                    "kit owner is on another world page"

        it "an out-of-range patient" $ \env →
            refuses env (bleed farPatientUid Nothing)
                    "patient is out of treatment range"

        it "an out-of-range third-party kit owner" $ \env →
            refuses env (bleed patientUid (Just farSupplierUid))
                    "kit owner is out of treatment range"

        -- The tourniquet fallback is the path with NO supplies to
        -- spend, so a spatial check that only guarded the kit branch
        -- would still dress an off-page wound for free.
        it "a cross-page patient whose medic carries no kit at all" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            stripKit env medicUid
            before ← snapshot env
            treatResult ls (bleed offPagePatientUid Nothing)
                `shouldReturn` q "false|patient is on another world page"
            snapshot env `shouldReturn` before

        -- Spatial invalidity and wound state coexist here: uid 10 is on
        -- another page AND has nothing to dress. The page must be what
        -- comes back, or a caller cannot tell the two apart.
        it "reports the page, not the wound state, when both would refuse" $ \env →
            refuses env (bleed offPageHealthyUid Nothing)
                    "patient is on another world page"

    describe "unit.treatInfection refuses, leaving the session untouched" $ do

        it "a cross-page patient" $ \env →
            refuses env (infect offPagePatientUid Nothing)
                    "patient is on another world page"

        it "a cross-page third-party supplier" $ \env →
            refuses env (infect patientUid (Just offPageSupplierUid))
                    "kit owner is on another world page"

        it "an out-of-range patient" $ \env →
            refuses env (infect farPatientUid Nothing)
                    "patient is out of treatment range"

        it "an out-of-range third-party supplier" $ \env →
            refuses env (infect patientUid (Just farSupplierUid))
                    "kit owner is out of treatment range"

        -- Requirement 3: the verb used to reach straight into
        -- kitHasFill with a uid it had never resolved, so a supplier
        -- that does not exist read as one carrying no antibiotics.
        it "a missing supplier reads as a missing entity, not an empty kit" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before ← snapshot env
            treatResult ls (T.concat
                [ "unit.treatInfection(", uid medicUid, ", ", uid patientUid
                , ", ", T.pack (show missingUid), ")" ])
                `shouldReturn` q "false|medic, patient, or kit owner not found"
            snapshot env `shouldReturn` before

    describe "the same-page, in-range control still commits" $ do

        it "treatBleeding dresses the wound from the medic's own kit" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before ← snapshot env
            treatResult ls (bleed patientUid Nothing)
                `shouldReturn` q "true|treated"
            after ← snapshot env
            -- The very fingerprints every refusal above proves absent.
            sesUnits after `shouldNotBe` sesUnits before
            sesIntRolled after `shouldBe` True
            bandagesIn env medicUid `shouldNotReturn` 3

        it "treatBleeding still draws from an in-range third party's kit" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            treatResult ls (bleed patientUid (Just supplierUid))
                `shouldReturn` q "true|treated"
            -- The supplier paid, and the medic's own kit did not.
            bandagesIn env supplierUid `shouldNotReturn` 3
            bandagesIn env medicUid `shouldReturn` 3

        it "treatInfection knocks the infection down and spends a pill" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            treatResult ls (infect patientUid (Just supplierUid))
                `shouldReturn` q "true|antibiotics administered"
            worstInfection env patientUid `shouldReturn` 0
            (sesIntRolled <$> snapshot env) `shouldReturn` True

    describe "the reach is one number, and it is the boundary" $ do

        it "unit.treatmentRange() reports the engine's own constant" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            evalDebug ls "return unit.treatmentRange()"
                `shouldReturn` T.pack (show treatmentRange)

        it "exactly that far is treated; a hundredth further is refused" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            treatResult ls (bleed edgePatientUid Nothing)
                `shouldReturn` q "true|treated"
            resetPages env
            treatResult ls (bleed pastEdgePatientUid Nothing)
                `shouldReturn` q "false|patient is out of treatment range"

        -- unit.canTreat is what the context menu greys its rows on, so
        -- it has to be the verbs' own answer rather than a second
        -- opinion that can drift from it.
        it "unit.canTreat answers exactly what the verbs refuse on" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            canTreat ls (uid medicUid <> ", " <> uid patientUid)
                `shouldReturn` q "true|"
            canTreat ls (uid medicUid <> ", " <> uid patientUid
                                      <> ", " <> uid supplierUid)
                `shouldReturn` q "true|"
            canTreat ls (uid medicUid <> ", " <> uid offPagePatientUid)
                `shouldReturn` q "false|patient is on another world page"
            canTreat ls (uid medicUid <> ", " <> uid patientUid
                                      <> ", " <> uid offPageSupplierUid)
                `shouldReturn` q "false|kit owner is on another world page"
            canTreat ls (uid medicUid <> ", " <> uid farPatientUid)
                `shouldReturn` q "false|patient is out of treatment range"
            canTreat ls (uid medicUid <> ", " <> uid patientUid
                                      <> ", " <> uid farSupplierUid)
                `shouldReturn` q "false|kit owner is out of treatment range"
            canTreat ls (uid medicUid <> ", " <> uid edgePatientUid)
                `shouldReturn` q "true|"
            canTreat ls (uid medicUid <> ", " <> uid pastEdgePatientUid)
                `shouldReturn` q "false|patient is out of treatment range"
            -- Asking is not doing: the query left nothing behind.
            (sesIntRolled <$> snapshot env) `shouldReturn` False

    -- The rows the player actually clicks. Requirement 5: an
    -- out-of-reach medic, patient or kit owner must leave the entry
    -- DISABLED rather than fire into the refusal above -- and it must
    -- do so by asking the engine, so the menu cannot drift from the
    -- verb. These drive the REAL scripts/init_context_menu against the
    -- same live fixture; only selection, hit-testing and the two
    -- inventory projections are stubbed (supply detection is not what
    -- #2297 changed, and unit.getInventory needs an item registry).
    describe "the context menu greys both rows on the same rule" $ do

        it "same page and in range: both rows are offered enabled" $ \env → do
            ls ← menuOn env patientUid [medicUid, supplierUid] [supplierUid]
            rowState ls "Treat bleeding" `shouldReturn` q "true"
            rowState ls "Treat infection" `shouldReturn` q "true"

        it "a cross-page patient greys both rows" $ \env → do
            ls ← menuOn env offPagePatientUid [medicUid, supplierUid] [supplierUid]
            rowState ls "Treat bleeding" `shouldReturn` q "false"
            rowState ls "Treat infection" `shouldReturn` q "false"

        it "an out-of-range patient greys both rows" $ \env → do
            ls ← menuOn env farPatientUid [medicUid, supplierUid] [supplierUid]
            rowState ls "Treat bleeding" `shouldReturn` q "false"
            rowState ls "Treat infection" `shouldReturn` q "false"

        -- The supplier is the only stocked unit in the selection, so it
        -- is the kit owner both rows resolve -- and it is two tiles out.
        it "an out-of-range selected supplier greys both rows" $ \env → do
            ls ← menuOn env patientUid [medicUid, farSupplierUid] [farSupplierUid]
            rowState ls "Treat bleeding" `shouldReturn` q "false"
            rowState ls "Treat infection" `shouldReturn` q "false"

        it "a cross-page selected supplier greys both rows" $ \env → do
            ls ← menuOn env patientUid [medicUid, offPageSupplierUid]
                         [offPageSupplierUid]
            rowState ls "Treat bleeding" `shouldReturn` q "false"
            rowState ls "Treat infection" `shouldReturn` q "false"

        -- Control for the four above: with the SAME stubbing, an
        -- in-range same-page supplier the medic does not itself stock
        -- from still enables both rows, so a case cannot pass because
        -- the supply stub failed.
        it "an in-range third-party supplier still enables both rows" $ \env → do
            ls ← menuOn env patientUid [medicUid, supplierUid] [supplierUid]
            rowState ls "Treat bleeding" `shouldReturn` q "true"
            rowState ls "Treat infection" `shouldReturn` q "true"

-- * Fixture surgery and readers used by the control cases

-- | Empty a unit's inventory in place — the no-supplies fixture the
--   tourniquet fallback needs.
stripKit ∷ EngineEnv → UnitId → IO ()
stripKit env u = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup u (umInstances um) of
        Nothing → pure ()
        Just inst → writeIORef (unitManagerRef env) um
            { umInstances = HM.insert u (inst { uiInventory = [] })
                                      (umInstances um) }

-- | How many bandages the unit's first kit still holds.
bandagesIn ∷ EngineEnv → UnitId → IO Int
bandagesIn env u = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup u (umInstances um) of
        Nothing → -1
        Just inst → length
            [ () | it ← uiInventory inst, c ← iiContents it
                 , iiDefName c ≡ "bandage" ]

-- | The unit's worst wound infection level, or -1 with no wounds.
worstInfection ∷ EngineEnv → UnitId → IO Float
worstInfection env u = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup u (umInstances um) of
        Nothing → -1
        Just inst → case map woundInfection (uiWounds inst) of
            [] → -1
            xs → maximum xs

-- | Open the real unit context menu over @target@ with @selection@
--   selected and only @stocked@ carrying a usable kit, recording every
--   row's label and enabled state into @_G.__rows@.
--
--   The Lua state is rebuilt per call: each case is one menu, and a
--   stale spy from a previous one would read as a silently-passing
--   assertion.
menuOn ∷ EngineEnv → UnitId → [UnitId] → [UnitId] → IO LuaBackendState
menuOn env target selection stocked = do
    resetPages env
    ls ← newBareLuaBackend env
    run ls (menuSetup target selection stocked)
    opened ← evalDebug ls
        "return tostring(require('scripts.init_context_menu').tryUnitMenu(10, 20))"
    opened `shouldBe` q "true"
    pure ls

-- | Run a setup statement and fail loudly if it errors — a broken stub
--   must never masquerade as a disabled row.
run ∷ LuaBackendState → Text → IO ()
run ls stmt = do
    r ← evalDebug ls stmt
    r `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t
                             ∨ "syntax error:" `T.isPrefixOf` t)

menuSetup ∷ UnitId → [UnitId] → [UnitId] → Text
menuSetup target selection stocked = T.concat
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
    -- Only the named units carry a usable kit, so which unit each row
    -- resolves as its owner is fixed by the fixture, not by luck.
    , "local STOCKED = {"
    , T.intercalate ", " ["[" <> uid u <> "] = true" | u ← stocked], "}; "
    , "unit.getInventory = function(u) "
    , "  if STOCKED[u] then "
    , "    return { { defName = 'first_aid_kit', kind = 'container' } } end "
    , "  return {} end; "
    , "unit.getItemContents = function(u, defName) "
    , "  if STOCKED[u] and defName == 'first_aid_kit' then "
    , "    return { { defName = 'bandage', count = 3 }, "
    , "             { defName = 'antibiotics', fill = 5 } } end "
    , "  return {} end; "
    ]

-- | One row's enabled state, or @nil@ when the menu never offered it.
rowState ∷ LuaBackendState → Text → IO Text
rowState ls label =
    evalDebug ls ("return tostring(_G.__rows and _G.__rows['" <> label <> "'])")

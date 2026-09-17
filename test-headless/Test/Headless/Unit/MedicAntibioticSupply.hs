-- | The autonomous medic fetches the medicine the patient actually
--   needs, and gives up on one it cannot get (#2644).
--
--   The supply phase of @scripts/unit_ai_medic@ used to ask one
--   question — "am I carrying a kit with a BANDAGE in it?" — and ask it
--   of every holder too. An infected patient who is not bleeding needs
--   ANTIBIOTICS, and antibiotics do not have to travel with bandages: a
--   kit whose last bandage has been spent, or one stocked with nothing
--   but pills, was invisible to that scan. The medic walked to the
--   patient empty-handed, @treatBleeding@ correctly reported nothing to
--   dress, @treatInfection@ failed for lack of supplies — and the claim
--   and its lock utility of 8 survived all of it, so the medic
--   re-selected the same impossible treatment on every subsequent
--   decision and ordinary work never ran again.
--
--   Both halves are measured here: the FETCH now asks per medicine and
--   names the exact container, and the futile case is BOUNDED — the
--   claim is released and the patient deferred
--   ('scripts.unit_ai_medic_supply.SUPPLY_RETRY_SECONDS' of game time),
--   after which it becomes eligible again.
--
--   Nothing is stubbed except the two things every sibling AI gate
--   stubs: the @scripts.unit_ai@ singleton the submodules attach to,
--   and @scripts.movement_speed@, whose real pace answer reaches the
--   whole physiology chain. The inventory scan, the holder search, the
--   transfer, @treatBleeding@, @treatInfection@ and the clock are all
--   production code against live manager refs — so a case that claims a
--   dose was spent is reading the engine's own consumption, not a
--   restatement of the Lua that asked for it.
--
--   The off-page supplier belongs to the page-pairing gate rather than
--   here: @unit.getAllIds@ answers for the ACTIVE page only, so a real
--   off-page holder is filtered before @findKitHolder@ ever sees it and
--   a case here could not tell the two guards apart.
--   'Test.Headless.Lua.UnitAiPageTargets' stubs that listing and so can.
--
--   Fixture shape follows 'Test.Headless.Unit.MedicalKitInstance',
--   built from 'Test.Headless.Unit.TransferApi''s constructors.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "medic antibiotic supply"'@.
module Test.Headless.Unit.MedicAntibioticSupply (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import qualified System.Random as Random
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..), Wound(..)
    , emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)
import Test.Headless.Unit.TransferApi
    (evalDebug, mkItem, mkUnit, minimalDef, newBareLuaBackend)

-- * Identities

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "medic_antibiotic_supply_page"

-- | uid 1 — the medic. uid 2 — its patient, one tile east, inside
--   'Unit.Medical.Reach.treatmentRange' so no case here is a reach
--   case. uid 3 — the near holder, one tile north, inside
--   @mule_fetch_arrival@ so a fetch commits on the tick it is chosen.
--   uid 4 — a FAR holder three tiles north, used only where a case is
--   about which holder is picked.
medicUid, patientUid, holderUid, farHolderUid ∷ UnitId
medicUid     = UnitId 1
patientUid   = UnitId 2
holderUid    = UnitId 3
farHolderUid = UnitId 4

-- | uid 5 — a SECOND medic, as capable as the first and standing
--   further away, so it is the first medic's deferral and nothing else
--   that can ever let it rank.
otherMedicUid ∷ UnitId
otherMedicUid = UnitId 5

-- * Items

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
    , ("antibiotics", supplyDef "antibiotics" "Antibiotics") ]

-- | A kit holding @b@ bandages and a bottle of @p@ pills (no bottle at
--   all when @p@ is zero), so a case names exactly the stock that makes
--   it the case it is. Contents ids are derived from the kit's own id,
--   which keeps them unique without a second counter to keep in step.
kit ∷ Word64 → Int → Float → ItemInstance
kit iid b p = (mkItem "first_aid_kit" iid 0.5)
    { iiContents = bandages <> bottle }
  where
    bandages = [ mkItem "bandage" (iid + fromIntegral n) 0.05
               | n ← [1 .. b] ]
    bottle | p ≤ 0 = []
           | otherwise = [ (mkItem "antibiotics" (iid + 90) 0.2)
                             { iiCurrentFill = p } ]

-- | An empty kit of the SAME definition, to sit in front of a stocked
--   one: the shape @medical_supply@'s exact-instance scan exists for.
emptyKit ∷ Word64 → ItemInstance
emptyKit iid = kit iid 0 0

-- * Units

acolyteDef ∷ UnitDef
acolyteDef = minimalDef "acolyte" "Acolyte"

-- | An infected wound that is NOT worth dressing: @bandage@ 0 is below
--   @treat_min_seep@, so the only unmet need is the cure. This is the
--   patient the whole defect was about.
infectedOnly ∷ Wound
infectedOnly = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 0.5
    , woundAt = 0, woundBandage = 0.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.6, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- | The same wound still seeping: both needs unmet at once.
bleedingAndInfected ∷ Wound
bleedingAndInfected = infectedOnly { woundBandage = 1.0 }

-- | Seeping but clean: the bleeding-only control requirement 4 protects.
bleedingOnly ∷ Wound
bleedingOnly = infectedOnly { woundBandage = 1.0, woundInfection = 0.0 }

acolyte ∷ (Float, Float) → [(Text, Float)] → [ItemInstance] → [Wound]
        → UnitInstance
acolyte xy knowledge inv wounds =
    (mkUnit "acolyte" FactionPlayer xy 100 inv [])
        { uiPage = fixturePage
        , uiKnowledge = HM.fromList knowledge
        , uiWounds = wounds }

medicKnowledge ∷ [(Text, Float)]
medicKnowledge = [("bleed_control", 100), ("infection_control", 100)]

-- | 'Test.Headless.Unit.MedicalKitInstance''s pinned roll, and pinned
--   for its reason: @treatBleeding@ draws from an entropy-seeded
--   per-'EngineEnv' generator, so without this every bandage count
--   below would be a probable statement rather than a true one. This
--   seed is a first-attempt success for exactly one bandage.
treatSeed ∷ Int
treatSeed = 20260906

-- | Rebuild the page, the registry, the clock and the roster. Every
--   case names its own roster: a spent pill or a moved kit inherited
--   from the previous one would read as a silently-passing assertion.
resetFixture ∷ EngineEnv → [(UnitId, UnitInstance)] → IO ()
resetFixture env roster = do
    writeIORef (treatRNGRef env) (Random.mkStdGen treatSeed)
    writeIORef (gameTimeRef env) 0
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" acolyteDef
        , umInstances = HM.fromList roster }

-- * Rosters

-- | The medic, carrying whatever a case hands it.
medicRow ∷ [ItemInstance] → (UnitId, UnitInstance)
medicRow inv = (medicUid, acolyte (10, 10) medicKnowledge inv [])

patientRow ∷ Wound → (UnitId, UnitInstance)
patientRow w = (patientUid, acolyte (11, 10) [] [] [w])

-- | A holder carries supplies and no knowledge, so it can never be
--   mistaken for a competing medic.
holderRow ∷ UnitId → (Float, Float) → [ItemInstance] → (UnitId, UnitInstance)
holderRow u xy inv = (u, acolyte xy [] inv [])

-- * Live-state readers

kitIds ∷ EngineEnv → UnitId → IO [Word64]
kitIds env u = withUnit env u [] (map iiInstanceId ∘ uiInventory)

-- | Bandages the unit holds across every kit — what a committed
--   dressing spends.
bandagesOn ∷ EngineEnv → UnitId → IO Int
bandagesOn env u = withUnit env u (-1) $ \inst → length
    [ () | it ← uiInventory inst, c ← iiContents it
         , iiDefName c ≡ "bandage" ]

-- | Antibiotic FILL the unit holds — what a committed cure spends, one
--   'Engine.Scripting.Lua.API.Units.Medical.antibioticsDose' per call.
pillsOn ∷ EngineEnv → UnitId → IO Float
pillsOn env u = withUnit env u (-1) $ \inst → sum
    [ iiCurrentFill c | it ← uiInventory inst, c ← iiContents it
                      , iiDefName c ≡ "antibiotics" ]

-- | The patient's worst wound as (dressing, infection). 1.0 dressing is
--   untreated; the tourniquet moves it without spending a bandage, so a
--   case reads both numbers rather than inferring one from the other.
worstWound ∷ EngineEnv → UnitId → IO (Float, Float)
worstWound env u = withUnit env u (-1, -1) $ \inst → case uiWounds inst of
    []      → (-1, -1)
    (w : _) → (woundBandage w, woundInfection w)

withUnit ∷ EngineEnv → UnitId → α → (UnitInstance → α) → IO α
withUnit env u absent f = do
    um ← readIORef (unitManagerRef env)
    pure $ maybe absent f (HM.lookup u (umInstances um))

-- * Lua plumbing

uid ∷ UnitId → Text
uid (UnitId n) = T.pack (show n)

wid ∷ Word64 → Text
wid = T.pack ∘ show

q ∷ Text → Text
q t = "\"" <> t <> "\""

run ∷ LuaBackendState → Text → IO ()
run ls stmt = do
    r ← evalDebug ls stmt
    r `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t
                             ∨ "syntax error:" `T.isPrefixOf` t)

-- | A Lua state with the shipped medic loaded, a spy around
--   @unit.transferItemToUnit@ recording the exact instance id the AI
--   named (and then calling the real verb), and a move counter — so
--   "fetched nothing" can be told apart from "walked off somewhere".
medicBackend ∷ EngineEnv → [(UnitId, UnitInstance)] → IO LuaBackendState
medicBackend env roster = do
    resetFixture env roster
    ls ← newBareLuaBackend env
    run ls medicSetup
    pure ls

medicSetup ∷ Text
medicSetup = T.concat
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
    -- The cure verb is counted as well as called, because one of the
    -- rules here is that it must NOT be attempted with no antibiotics
    -- in hand: without a counter that rule is invisible, since a
    -- refused cure changes no wound and spends no supply.
    , "_G.__cures = 0; "
    , "local realCure = unit.treatInfection; "
    , "unit.treatInfection = function(...) "
    , "  _G.__cures = _G.__cures + 1; return realCure(...) end; "
    , "_G.__medic = require('scripts.unit_ai_medic'); "
    , "_G.__msupply = require('scripts.unit_ai_medic_supply'); "
    , "_G.__params = require('scripts.unit_ai_tunables').acolyte; "
    , "_G.__state = { treatClaim = { patient = ", uid patientUid, " } }; "
    ]

-- | One @treatExecute@ decision, against the claim the setup locked on.
treatTick ∷ LuaBackendState → IO ()
treatTick ls = run ls $ T.concat
    [ "_G.__medic.treatExecute(", uid medicUid, ", _G.__state, _G.__params)" ]

transfers ∷ LuaBackendState → IO Text
transfers ls = evalDebug ls "return table.concat(_G.__transfers, ',')"

moves ∷ LuaBackendState → IO Text
moves ls = evalDebug ls "return tostring(_G.__moves)"

-- | How many times @unit.treatInfection@ was actually reached.
cures ∷ LuaBackendState → IO Text
cures ls = evalDebug ls "return tostring(_G.__cures)"

-- | @true@ once the claim has been released.
claimReleased ∷ LuaBackendState → IO Text
claimReleased ls = evalDebug ls "return tostring(_G.__state.treatClaim == nil)"

-- | @true@ while this medic is sitting the patient out.
deferred ∷ LuaBackendState → IO Text
deferred ls = evalDebug ls $ T.concat
    [ "return tostring(_G.__state.treatDefer ~= nil "
    , "and _G.__state.treatDefer[", uid patientUid, "] ~= nil)" ]

-- | Score treat_ally for a medic, reported as a bucket rather than a
--   number: @"out"@ is the -inf that lets ordinary work win, @"in"@ is
--   an eligible patient.
scoreFor ∷ LuaBackendState → UnitId → Text → IO Text
scoreFor ls u state = evalDebug ls $ T.concat
    [ "local u = _G.__medic.treatAllyUtility(", uid u, ", ", state
    , ", _G.__params); "
    , "return u == -math.huge and 'out' or (u > 0 and 'in' or 'zero')" ]

-- | Move the world clock forward, the way the deferral's own window is
--   measured: real @engine.gameTime()@, no stub.
advanceClock ∷ EngineEnv → Double → IO ()
advanceClock env = writeIORef (gameTimeRef env)

spec ∷ SpecWith EngineEnv
spec = describe "medic antibiotic supply (#2644)" $ do

    describe "fetching the cure" $ do

        -- The defect itself. The holder's ONLY medicine is antibiotics,
        -- and they sit behind an empty same-definition sibling, so this
        -- is simultaneously the case the old bandage-keyed scan could
        -- not see at all and the exact-instance case #2302 established.
        it "fetches an antibiotics-only kit, spends one dose and releases" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow infectedOnly
                , holderRow holderUid (10, 11)
                    [emptyKit 300, kit 400 0 5] ]
            treatTick ls
            transfers ls `shouldReturn` q (wid 400)
            kitIds env medicUid `shouldReturn` [400]
            kitIds env holderUid `shouldReturn` [300]
            -- The cure itself, on the engine's own counters.
            treatTick ls
            (dressing, infection) ← worstWound env patientUid
            infection `shouldBe` 0
            dressing `shouldBe` 0      -- nothing to dress, nothing dressed
            pillsOn env medicUid `shouldReturn` 4
            cures ls `shouldReturn` q "1"
            claimReleased ls `shouldReturn` q "false"
            -- Nothing left to treat → the claim goes back, and the
            -- cured patient is not attempted again.
            treatTick ls
            claimReleased ls `shouldReturn` q "true"
            deferred ls `shouldReturn` q "false"
            cures ls `shouldReturn` q "1"

        -- Mixed needs, direction one: bandages in hand, the cure on
        -- someone else. The medic must fetch ONLY what it lacks.
        it "fetches antibiotics while already carrying bandages" $ \env → do
            ls ← medicBackend env
                [ medicRow [kit 100 3 0], patientRow bleedingAndInfected
                , holderRow holderUid (10, 11) [kit 400 0 5] ]
            treatTick ls
            transfers ls `shouldReturn` q (wid 400)
            treatTick ls
            (dressing, infection) ← worstWound env patientUid
            dressing `shouldSatisfy` (< 1.0)
            infection `shouldBe` 0
            bandagesOn env medicUid `shouldReturn` 2
            pillsOn env medicUid `shouldReturn` 4

        -- Mixed needs, direction two: the cure in hand, bandages on
        -- someone else. Same rule, and the half that would still pass
        -- on the old code — so it is the control that keeps the
        -- direction above from being the only thing measured.
        it "fetches bandages while already carrying antibiotics" $ \env → do
            ls ← medicBackend env
                [ medicRow [kit 100 0 5], patientRow bleedingAndInfected
                , holderRow holderUid (10, 11) [kit 400 3 0] ]
            treatTick ls
            transfers ls `shouldReturn` q (wid 400)
            treatTick ls
            (dressing, infection) ← worstWound env patientUid
            dressing `shouldSatisfy` (< 1.0)
            infection `shouldBe` 0
            bandagesOn env medicUid `shouldReturn` 2
            pillsOn env medicUid `shouldReturn` 4

        -- Two trips or one. The near holder covers half the need and
        -- the far one covers all of it, so a distance-only search would
        -- pick the near one — correctly, but twice.
        it "prefers a holder stocking both medicines over a nearer partial one" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow bleedingAndInfected
                , holderRow holderUid (10, 11) [kit 300 3 0]
                , holderRow farHolderUid (10, 13) [kit 400 3 5] ]
            found ← evalDebug ls $ T.concat
                [ "local h = _G.__msupply.findKitHolder(", uid medicUid
                , ", 10, 10, { bandage = true, antibiotics = true }); "
                , "return tostring(h and h.uid) .. '|' "
                , ".. tostring(h and h.kitInstance)" ]
            found `shouldBe` q (uid farHolderUid <> "|" <> wid 400)
            -- And with only ONE medicine wanted the nearer holder that
            -- stocks it still wins: coverage breaks the tie, distance
            -- decides everything else.
            near ← evalDebug ls $ T.concat
                [ "local h = _G.__msupply.findKitHolder(", uid medicUid
                , ", 10, 10, { bandage = true }); "
                , "return tostring(h and h.uid)" ]
            near `shouldBe` q (uid holderUid)

    describe "the futile-cure bound" $ do

        -- Requirement 3. No antibiotics exist anywhere, so there is
        -- nothing to walk to and nothing to carry: the claim goes back
        -- on the FIRST decision, having moved and transferred nothing.
        it "releases on the first decision when no antibiotics exist, moving nothing" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow infectedOnly
                , holderRow holderUid (10, 11) [kit 400 3 0] ]
            treatTick ls
            transfers ls `shouldReturn` q ""
            moves ls `shouldReturn` q "0"
            claimReleased ls `shouldReturn` q "true"
            deferred ls `shouldReturn` q "true"
            -- The patient is untouched: no tourniquet improvised on a
            -- wound that was not bleeding, no dose conjured.
            worstWound env patientUid `shouldReturn` (0, 0.6)

        -- The half a bare release would not buy. treat_ally's base and
        -- lock utility are both 8.0, so without the deferral the very
        -- next decision retakes the same impossible patient.
        it "yields to ordinary work while the cure is unavailable, and returns after the window" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow infectedOnly
                , holderRow holderUid (10, 11) [kit 400 3 0] ]
            treatTick ls
            scoreFor ls medicUid "_G.__state" `shouldReturn` q "out"
            -- ...and it is the DEFERRAL saying so, not the patient
            -- having stopped needing treatment: a state with no
            -- deferral scores the same patient as eligible.
            scoreFor ls medicUid "{}" `shouldReturn` q "in"
            -- The window is a wait, not a blacklist.
            advanceClock env 61
            scoreFor ls medicUid "_G.__state" `shouldReturn` q "in"

        -- A deferral is one medic sitting out, not the patient being
        -- written off: a second medic must still rank for them, or a
        -- colleague who later picks up antibiotics could never step in.
        it "does not block a second medic from ranking for the same patient" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow infectedOnly
                , holderRow holderUid (10, 11) [kit 400 3 0]
                , (otherMedicUid, acolyte (16, 10) medicKnowledge [] []) ]
            treatTick ls
            run ls "_G.__ai = require('scripts.unit_ai_core').aiState; \
                   \_G.__ai[1] = _G.__state"
            scoreFor ls otherMedicUid "{}" `shouldReturn` q "in"

        -- Requirement 4 read together with the bound: an unavailable
        -- cure must not cost the patient the stabilization that IS
        -- possible. The holder stocks bandages only, so the medic
        -- fetches and dresses first and only then gives the claim back.
        it "stabilizes the bleeding it can treat before releasing the infection claim" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow bleedingAndInfected
                , holderRow holderUid (10, 11) [kit 400 3 0] ]
            treatTick ls                       -- fetch the bandages
            transfers ls `shouldReturn` q (wid 400)
            treatTick ls                       -- dress the bleeder
            (dressing, infection) ← worstWound env patientUid
            dressing `shouldSatisfy` (< 1.0)
            infection `shouldBe` 0.6           -- still uncured, as it must be
            bandagesOn env medicUid `shouldReturn` 2
            -- And the cure was never ATTEMPTED. Reaching the verb with
            -- an empty kit is what used to report "Infection untreated"
            -- once per tick for as long as the claim was held.
            cures ls `shouldReturn` q "0"
            claimReleased ls `shouldReturn` q "false"
            treatTick ls                       -- nothing left that is possible
            claimReleased ls `shouldReturn` q "true"
            deferred ls `shouldReturn` q "true"

    describe "the unchanged paths" $ do

        -- Requirement 4. A bleeding-only patient with no kit anywhere
        -- still gets the makeshift tourniquet: the dressing moves and
        -- no bandage is spent, which is how the fallback is told apart
        -- from a committed dressing.
        it "still improvises a tourniquet for a bleeder with no kit anywhere" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow bleedingOnly ]
            treatTick ls
            transfers ls `shouldReturn` q ""
            (dressing, _) ← worstWound env patientUid
            dressing `shouldSatisfy` (< 1.0)
            bandagesOn env medicUid `shouldReturn` 0
            deferred ls `shouldReturn` q "false"

        -- And the plain bandage fetch the search has always done is
        -- untouched, including its default question: findKitHolder with
        -- no `wants` still means "who has a bandage".
        it "keeps the bandage-only fetch and the default holder question" $ \env → do
            ls ← medicBackend env
                [ medicRow [], patientRow bleedingOnly
                , holderRow holderUid (10, 11) [emptyKit 300, kit 400 3 0] ]
            dflt ← evalDebug ls $ T.concat
                [ "local h = _G.__msupply.findKitHolder(", uid medicUid
                , ", 10, 10); "
                , "return tostring(h and h.uid) .. '|' "
                , ".. tostring(h and h.kitInstance)" ]
            dflt `shouldBe` q (uid holderUid <> "|" <> wid 400)
            treatTick ls
            transfers ls `shouldReturn` q (wid 400)
            treatTick ls
            (dressing, _) ← worstWound env patientUid
            dressing `shouldSatisfy` (< 1.0)
            bandagesOn env medicUid `shouldReturn` 2

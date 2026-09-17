{-# LANGUAGE Strict, OverloadedStrings #-}
-- | A treatment mutates exactly the wound it selected (#2638).
--
--   @unit.treatBleeding@ and @unit.treatInfection@ picked one worst
--   wound and then re-found it for mutation by the triple
--   @(woundPart, woundKind, woundAt)@. That triple is not an identity:
--   'Combat.Resolution' reads the shared game clock ONCE per attack and
--   stamps every wound of that hit with it, while
--   @Combat.Thread.processAllCommands@ drains the whole attack queue
--   inside one tick — so two attacks landing the same kind of wound on
--   the same part before the clock advances produce two wounds with
--   identical triples, and 'Unit.Types.Wound' documents that same-part
--   wounds are appended, never merged. All three mutators @map@ped over
--   the whole list and rewrote EVERY match.
--
--   Infection treatment was the worst of it: it computed ONE absolute
--   @newInf@ from the worst wound and copied it to every match after
--   spending a single dose, so a lightly infected wound got MORE
--   infected. The issue measured 0.10 → 0.40 on one dose.
--
--   The fix selects the target inside the same atomic manager
--   transaction that mutates it, and carries its POSITION in
--   @uiWounds@ rather than a clinical key. That also discharges the
--   selection-to-commit window structurally: there is no longer an
--   instant between choosing a wound and writing to it for a healing
--   tick or a wound-list change to land in. What remains observable —
--   and is covered below — is that the selection follows the LIVE list
--   and the cure arithmetic reads the live wound.
--
--   Nothing here stubs a treatment. The fixture registers real
--   'ItemDef's and real 'InfectionDef's, and every case calls the
--   REAL registered @unit.treatBleeding@ \/ @unit.treatInfection@
--   against live manager refs. Assertions compare the non-target's
--   COMPLETE 'Wound' record before and after, so a fix that merely
--   swapped one non-unique clinical key for another, or that updated
--   only the first match, fails here.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "treatment wound identity"'@.
module Test.Headless.Unit.MedicalWoundIdentity (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import qualified System.Random as Random
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Infection.Types
    (InfectionDef(..), InfectionManager(..), emptyInfectionManager)
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..), Wound(..)
    , emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)
import Test.Headless.Unit.TransferApi
    (evalDebug, mkItem, mkUnit, minimalDef, newBareLuaBackend)

-- * Fixture identities

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "medical_wound_identity_page"

-- | uid 1 — the medic, carrying one stocked kit and knowing both
--   treatments.
medicUid ∷ UnitId
medicUid = UnitId 1

-- | uid 2 — the patient, one tile east, well inside
--   'Unit.Medical.Reach.treatmentRange'. Its wound list is what each
--   case varies.
patientUid ∷ UnitId
patientUid = UnitId 2

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
    , ("antibiotics", supplyDef "antibiotics" "Antibiotics")
    , ("tweezers", supplyDef "tweezers" "Tweezers")
    , ("scissors", supplyDef "scissors" "Scissors")
    , ("antiseptic", supplyDef "antiseptic" "Antiseptic") ]

-- | Bandages, both tools at full condition, antiseptic and
--   antibiotics: 'Test.Headless.Unit.MedicalKitInstance''s kit shape,
--   and deliberately so. Undamaged tools put @toolFactor@ at 1, which
--   puts @pSucc@ on its 0.99 clamp — the roll 'treatSeed' pins — and
--   drives @seepBase@ to exactly 0, so a dressed wound's seep is an
--   exact 0 rather than a float an assertion has to round.
stockedKit ∷ ItemInstance
stockedKit = (mkItem "first_aid_kit" 100 0.5)
    { iiContents =
        [ mkItem "bandage" 101 0.05
        , mkItem "bandage" 102 0.05
        , mkItem "bandage" 103 0.05
        , mkItem "tweezers" 106 0.05
        , mkItem "scissors" 107 0.1
        , (mkItem "antiseptic" 104 0.12) { iiCurrentFill = 5 }
        , (mkItem "antibiotics" 105 0.2) { iiCurrentFill = 5 }
        ] }

-- | A kit with the tools and the bandages but NO antiseptic and NO
--   antibiotics: the supply shapes the two negative cases need.
bandagesOnlyKit ∷ ItemInstance
bandagesOnlyKit = (mkItem "first_aid_kit" 200 0.5)
    { iiContents = [ mkItem "bandage" 201 0.05
                   , mkItem "tweezers" 202 0.05
                   , mkItem "scissors" 203 0.1 ] }

-- * Infections
--
--   Two REAL defs so the same-key wounds can differ in
--   'woundInfectionType' the way the issue's own reproduction does, and
--   so one case can make the WORST-infected wound an incurable one.

infectionDef ∷ Text → [Text] → Float → InfectionDef
infectionDef name curableBy rate = InfectionDef
    { infId = name, infName = name, infIcon = "", infCategory = "bacterial"
    , infSites = ["surface"], infBaseWeight = 1
    , infTempMin = 0, infTempMax = 50, infMoistMin = 0, infMoistMax = 1
    , infAggressiveness = 1, infInfectability = 1
    , infCurableBy = curableBy, infCureRate = rate
    , infWoundInfectable = True, infEffects = []
    , infTransmissibility = 0, infTransmission = [] }

-- | Antibiotic-curable at full cure rate.
staph ∷ Text
staph = "staph"

-- | Antibiotic-curable too, so a pair of same-key wounds can differ in
--   TYPE without either dropping out of the candidate set.
strep ∷ Text
strep = "strep"

-- | NOT antibiotic-curable. A wound carrying this is skipped by the
--   verb however infected it is.
thrush ∷ Text
thrush = "thrush"

fixtureInfections ∷ InfectionManager
fixtureInfections = InfectionManager $ HM.fromList
    [ (staph,  infectionDef staph  ["antibiotics"] 1.0)
    , (strep,  infectionDef strep  ["antibiotics"] 1.0)
    , (thrush, infectionDef thrush ["antifungal"]  1.0) ]

-- * Wounds
--
--   Every wound below is stamped @woundAt = sameInstant@ on the same
--   part with the same kind. That is the collision the issue is about:
--   under the old triple match a mutation aimed at any one of them hit
--   all of them.

sameInstant ∷ Double
sameInstant = 0

-- | The shape both verbs work on: an open, seeping, infected slash.
--   Callers vary only what they name.
slash ∷ Float → Float → Text → Wound
slash severity infection infType = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = severity
    , woundAt = sameInstant, woundBandage = 1.0, woundClot = 0.0
    , woundHeal = 0.0, woundDressing = "", woundInfection = infection
    , woundClean = False, woundInfectionType = infType
    , woundNecrosis = 0.0 }

-- * Scene

acolyteDef ∷ UnitDef
acolyteDef = minimalDef "acolyte" "Acolyte"

acolyte ∷ (Float, Float) → [(Text, Float)] → [ItemInstance] → [Wound]
        → UnitInstance
acolyte xy knowledge inv wounds =
    (mkUnit "acolyte" FactionPlayer xy 100 inv [])
        { uiPage = fixturePage
        , uiKnowledge = HM.fromList knowledge
        , uiWounds = wounds }

medicKnowledge ∷ [(Text, Float)]
medicKnowledge = [("bleed_control", 100), ("infection_control", 100)]

-- | The dressing roll every exact bandage count here is measured
--   against, pinned for the same reason
--   'Test.Headless.Unit.MedicalKitInstance' pins its own: @pSucc@ is
--   clamped BELOW 1, a failed attempt spends a bandage, and the engine
--   seeds @treatRNGRef@ from system entropy once per 'EngineEnv'. The
--   first example below holds this seed to a first-attempt success
--   through the engine's own counters, so a seed that drifted into a
--   retry fails loudly there rather than quietly turning every count
--   into a statement about a different roll.
treatSeed ∷ Int
treatSeed = 20260906

-- | One medic, one patient with the given wounds, and the given kit.
--   Called at the top of every case: a spent bandage, a moved dose or a
--   generator inherited from the previous one would read as a silently
--   passing assertion.
resetScene ∷ EngineEnv → ItemInstance → [Wound] → IO ()
resetScene env kit wounds = do
    writeIORef (treatRNGRef env) (Random.mkStdGen treatSeed)
    writeIORef (infectionManagerRef env) fixtureInfections
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" acolyteDef
        , umInstances = HM.fromList
            [ (medicUid, acolyte (10, 10) medicKnowledge [kit] [])
            , (patientUid, acolyte (11, 10) [] [] wounds) ] }

-- * Live-state readers

-- | Round to four decimals. The cure subtracts a Float from a Float,
--   so @0.9 - 0.8@ arrives as @9.9999964e-2@; an assertion should state
--   the clinical value, not that last bit.
r4 ∷ Float → Float
r4 x = fromIntegral (round (x * 10000) ∷ Int) / 10000

-- | Every wound's infection, rounded — the reading most cases assert.
infectionsOf ∷ EngineEnv → IO [Float]
infectionsOf env = map (r4 . woundInfection) <$> woundsOf env

woundsOf ∷ EngineEnv → IO [Wound]
woundsOf env = do
    um ← readIORef (unitManagerRef env)
    pure $ maybe [] uiWounds (HM.lookup patientUid (umInstances um))

-- | Bandage count and the antiseptic / antibiotic fills the medic
--   still carries, as one comparable tuple. Every "one dose" assertion
--   reads this before and after.
suppliesOf ∷ EngineEnv → IO (Int, Float, Float)
suppliesOf env = do
    um ← readIORef (unitManagerRef env)
    let cs = case HM.lookup medicUid (umInstances um) of
            Nothing   → []
            Just inst → [ c | it ← uiInventory inst, c ← iiContents it ]
        fillOf n = sum [ iiCurrentFill c | c ← cs, iiDefName c ≡ n ]
    pure ( length [ () | c ← cs, iiDefName c ≡ "bandage" ]
         , fillOf "antiseptic", fillOf "antibiotics" )

-- * Lua plumbing

uid ∷ UnitId → Text
uid (UnitId n) = T.pack (show n)

q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | Call the real verb and flatten its result table to
--   @"<ok>|<number>|<message>"@ — @number@ being @seep@ for bleeding
--   and the new infection level for infection, which is the field each
--   verb reuses.
treat ∷ LuaBackendState → Text → IO Text
treat ls verb = evalDebug ls $ T.concat
    [ "local r = unit.", verb, "(", uid medicUid, ", ", uid patientUid, "); "
    , "return tostring(r and r.ok) .. '|' "
    , ".. string.format('%.4f', (r and r.infection) or (r and r.seep) or -1) "
    , ".. '|' .. tostring(r and r.message)" ]

-- | The message alone, for the refusal cases.
treatMessage ∷ LuaBackendState → Text → IO Text
treatMessage ls verb = evalDebug ls $ T.concat
    [ "local r = unit.", verb, "(", uid medicUid, ", ", uid patientUid, "); "
    , "return tostring(r and r.ok) .. '|' .. tostring(r and r.message)" ]

spec ∷ SpecWith EngineEnv
spec = describe "treatment wound identity (#2638)" $ do

    -- §0 The premise, stated against the engine rather than assumed:
    -- this seed dresses on the FIRST attempt for exactly one bandage.
    -- Every bandage count below depends on it.
    describe "the pinned dressing roll (§0)" $
        it "is a FIRST-attempt success: one attempt, one bandage" $ \env → do
            resetScene env stockedKit [slash 0.5 0 ""]
            ls ← newBareLuaBackend env
            r ← evalDebug ls $ T.concat
                [ "local r = unit.treatBleeding(", uid medicUid, ", "
                , uid patientUid, "); "
                , "return tostring(r and r.ok) .. '|' "
                , ".. tostring(r and r.attempts) .. '|' "
                , ".. tostring(r and r.bandagesUsed)" ]
            r `shouldBe` q "true|1|1"

    -- §1 The defect. Two wounds sharing part, kind AND woundAt; one
    -- treatment; the non-target must come back byte-for-byte unchanged.
    describe "two same-instant wounds on the same part (§1)" $ do

        it "treatInfection cures only its target and never re-infects the \
           \other" $ \env → do
            -- The issue's own reproduction: 0.9 and 0.1 infections, one
            -- dose. Pre-fix the second read back 0.40 -- MORE infected
            -- than it started -- because one absolute newInf was copied
            -- to every triple match.
            let target = slash 0.5 0.9 staph
                other  = slash 0.5 0.1 strep
            resetScene env stockedKit [target, other]
            ls ← newBareLuaBackend env
            before ← suppliesOf env
            r ← treat ls "treatInfection"
            r `shouldBe` q "true|0.1000|antibiotics administered"

            after ← woundsOf env
            map (r4 . woundInfection) after `shouldBe` [0.1, 0.1]
            -- The non-target is the SAME RECORD, not merely the same
            -- infection: cleanliness is written by this verb too.
            drop 1 after `shouldBe` [other]
            -- Exactly one pill, and the bleeding supplies untouched.
            (b0, s0, a0) ← pure before
            suppliesOf env `shouldReturn` (b0, s0, a0 - 1)

        it "treatBleeding dresses only its target and leaves the other \
           \undressed" $ \env → do
            -- Identical part/kind/at; the first is the worse bleeder
            -- because its severity is higher, so selection is
            -- unambiguous and only the MUTATION is under test.
            let target = slash 0.8 0 ""
                other  = slash 0.2 0 ""
            resetScene env stockedKit [target, other]
            ls ← newBareLuaBackend env
            (b0, s0, a0) ← suppliesOf env
            r ← treat ls "treatBleeding"
            r `shouldBe` q "true|0.0000|treated"

            after ← woundsOf env
            -- The target is dressed, sealed and disinfected by the
            -- antiseptic half of the same commit.
            map woundDressing after `shouldBe` ["bandage", ""]
            map woundClean after `shouldBe` [True, False]
            map woundBandage after `shouldBe` [0, 1.0]
            drop 1 after `shouldBe` [other]
            -- One bandage, one antiseptic dose, no antibiotics.
            suppliesOf env `shouldReturn` (b0 - 1, s0 - 0.05, a0)

        -- The hardest shape for any clinical key: the two wounds are
        -- equal in EVERY field, so nothing but position can tell them
        -- apart. Exactly one must change.
        it "separates two wounds identical in every clinical field" $
          \env → do
            let w = slash 0.5 0.7 staph
            resetScene env stockedKit [w, w]
            ls ← newBareLuaBackend env
            _ ← treat ls "treatInfection"
            after ← woundsOf env
            length after `shouldBe` 2
            -- One cured, one untouched -- and the untouched one is the
            -- original record entire.
            length (filter (≡ w) after) `shouldBe` 1
            map (r4 . woundInfection) after `shouldSatisfy`
                (\infs → length (filter (≡ 0.7) infs) ≡ 1
                       ∧ length (filter (< 0.7) infs) ≡ 1)

    -- §2 Selection is not "the first match". The target is deliberately
    -- placed later in the list, behind same-key wounds that would
    -- absorb a first-match write.
    describe "a target that is not first in the list (§2)" $ do

        it "treatInfection cures the later, worse wound and no other" $
          \env → do
            let mild   = slash 0.5 0.2 staph
                worst  = slash 0.5 0.9 strep
                middle = slash 0.5 0.3 staph
            resetScene env stockedKit [mild, middle, worst]
            ls ← newBareLuaBackend env
            r ← treat ls "treatInfection"
            r `shouldBe` q "true|0.1000|antibiotics administered"
            after ← woundsOf env
            map (r4 . woundInfection) after
                `shouldBe` [0.2, 0.3, 0.1]
            -- Both non-targets survive entire, cleanliness included.
            take 2 after `shouldBe` [mild, middle]

        it "treatBleeding dresses the later, worse bleeder and no other" $
          \env → do
            let light = slash 0.2 0 ""
                heavy = slash 0.9 0 ""
            resetScene env stockedKit [light, heavy]
            ls ← newBareLuaBackend env
            r ← treat ls "treatBleeding"
            r `shouldBe` q "true|0.0000|treated"
            after ← woundsOf env
            map woundDressing after `shouldBe` ["", "bandage"]
            take 1 after `shouldBe` [light]

        -- Curability is part of selection, and it must not be confused
        -- with identity: the WORST-infected wound here cannot be cured
        -- by antibiotics, so the verb treats the next one down and
        -- leaves the incurable one exactly as it found it.
        it "skips an incurable wound without touching it" $ \env → do
            let fungal = slash 0.5 0.95 thrush
                target = slash 0.5 0.6 staph
            resetScene env stockedKit [fungal, target]
            ls ← newBareLuaBackend env
            r ← treat ls "treatInfection"
            r `shouldBe` q "true|0.0000|antibiotics administered"
            after ← woundsOf env
            take 1 after `shouldBe` [fungal]
            map (r4 . woundInfection) after `shouldBe` [0.95, 0]

    -- §3 The no-supplies path. The tourniquet is the branch that
    -- consumes nothing at all, and it went through the same aliasing
    -- mutator.
    describe "the no-supplies tourniquet (§3)" $
        it "improvises on exactly one wound and consumes nothing" $ \env → do
            let target = slash 0.8 0 ""
                other  = slash 0.2 0 ""
            -- An empty kit is no kit at all to the bandage scan.
            resetScene env (mkItem "first_aid_kit" 300 0.5) [target, other]
            ls ← newBareLuaBackend env
            before ← suppliesOf env
            r ← treatMessage ls "treatBleeding"
            r `shouldBe` q "true|makeshift tourniquet"
            after ← woundsOf env
            map woundDressing after `shouldBe` ["tourniquet", ""]
            drop 1 after `shouldBe` [other]
            -- A tourniquet is improvised, never drawn: nothing moved.
            suppliesOf env `shouldReturn` before

    -- §4 The selection reads the LIVE wound list. The fix selects
    -- inside the mutating transaction, so there is no window for a
    -- change to land in; what a caller can still observe is that a
    -- changed list redirects the selection rather than a remembered
    -- index or key surviving it.
    describe "selection follows the live wound list (§4)" $ do

        it "follows a reordering: the same wound is treated wherever it \
           \sits" $ \env → do
            let light = slash 0.2 0 ""
                heavy = slash 0.9 0 ""
            -- Heavy first.
            resetScene env stockedKit [heavy, light]
            ls1 ← newBareLuaBackend env
            _ ← treat ls1 "treatBleeding"
            a1 ← woundsOf env
            map woundDressing a1 `shouldBe` ["bandage", ""]
            -- Heavy second: the dressing moves with it.
            resetScene env stockedKit [light, heavy]
            ls2 ← newBareLuaBackend env
            _ ← treat ls2 "treatBleeding"
            a2 ← woundsOf env
            map woundDressing a2 `shouldBe` ["", "bandage"]

        it "follows a removal and an insertion" $ \env → do
            let mild = slash 0.5 0.3 staph
                bad  = slash 0.5 0.8 strep
            -- With `bad` removed, the mild wound becomes the target.
            resetScene env stockedKit [mild]
            ls1 ← newBareLuaBackend env
            _ ← treat ls1 "treatInfection"
            infectionsOf env `shouldReturn` [0]
            -- With `bad` inserted AHEAD of it, the target moves there.
            resetScene env stockedKit [bad, mild]
            ls2 ← newBareLuaBackend env
            _ ← treat ls2 "treatInfection"
            infectionsOf env `shouldReturn` [0, 0.3]

        it "follows clinical progression: a wound healed below the \
           \threshold is no longer the target" $ \env → do
            -- 0.05 is the verb's own eligibility floor. A wound sitting
            -- ON it is out, so the second wound is treated even though
            -- it is listed later.
            let healed = slash 0.5 0.05 staph
                live   = slash 0.5 0.4 staph
            resetScene env stockedKit [healed, live]
            ls ← newBareLuaBackend env
            r ← treat ls "treatInfection"
            r `shouldBe` q "true|0.0000|antibiotics administered"
            after ← woundsOf env
            take 1 after `shouldBe` [healed]
            map (r4 . woundInfection) after `shouldBe` [0.05, 0]

        -- The refusal half: with no eligible wound the verb must spend
        -- nothing at all -- no dose, no bandage, and not even the
        -- treatment generator, which is what makes a refusal replayable.
        it "refuses with no wound to treat, spending no supplies and no \
           \randomness" $ \env → do
            resetScene env stockedKit []
            ls ← newBareLuaBackend env
            before ← suppliesOf env
            gen0 ← readIORef (treatRNGRef env)

            treatMessage ls "treatBleeding"
                `shouldReturn` q "false|no bleeding wound to treat"
            treatMessage ls "treatInfection"
                `shouldReturn` q "false|no infected wound to treat"

            suppliesOf env `shouldReturn` before
            woundsOf env `shouldReturn` []
            gen1 ← readIORef (treatRNGRef env)
            show gen1 `shouldBe` show gen0

        it "refuses an incurable-only patient without touching the wound" $
          \env → do
            let fungal = slash 0.5 0.8 thrush
            resetScene env stockedKit [fungal]
            ls ← newBareLuaBackend env
            before ← suppliesOf env
            treatMessage ls "treatInfection"
                `shouldReturn`
                    q "false|infection not treatable with antibiotics"
            woundsOf env `shouldReturn` [fungal]
            suppliesOf env `shouldReturn` before

        it "refuses when the kit holds no antibiotics, leaving both \
           \same-key wounds alone" $ \env → do
            let target = slash 0.5 0.9 staph
                other  = slash 0.5 0.1 strep
            resetScene env bandagesOnlyKit [target, other]
            ls ← newBareLuaBackend env
            before ← suppliesOf env
            treatMessage ls "treatInfection"
                `shouldReturn` q "false|no antibiotics in kit"
            woundsOf env `shouldReturn` [target, other]
            suppliesOf env `shouldReturn` before

    -- §5 The single-wound common case, unchanged. Without this every
    -- assertion above could hold on a verb that had stopped treating
    -- anything.
    describe "the single-wound case is unchanged (§5)" $ do

        it "still dresses and disinfects a lone bleeding wound" $ \env → do
            resetScene env stockedKit [slash 0.6 0 ""]
            ls ← newBareLuaBackend env
            (b0, s0, _) ← suppliesOf env
            treat ls "treatBleeding" `shouldReturn` q "true|0.0000|treated"
            after ← woundsOf env
            map woundDressing after `shouldBe` ["bandage"]
            map woundClean after `shouldBe` [True]
            (b1, s1, _) ← suppliesOf env
            b1 `shouldBe` b0 - 1
            s1 `shouldBe` s0 - 0.05

        it "still cures a lone infected wound for one pill" $ \env → do
            resetScene env stockedKit [slash 0.6 0.5 staph]
            ls ← newBareLuaBackend env
            (_, _, a0) ← suppliesOf env
            treat ls "treatInfection"
                `shouldReturn` q "true|0.0000|antibiotics administered"
            infectionsOf env `shouldReturn` [0]
            (_, _, a1) ← suppliesOf env
            a1 `shouldBe` a0 - 1

        -- And the infection manager still matters: with no defs
        -- registered an untyped wound is treated as bacterial, which is
        -- the shipped default every other case here relies on.
        it "still treats an untyped wound with no infection defs loaded" $
          \env → do
            resetScene env stockedKit [slash 0.6 0.5 ""]
            writeIORef (infectionManagerRef env) emptyInfectionManager
            ls ← newBareLuaBackend env
            treat ls "treatInfection"
                `shouldReturn` q "true|0.0000|antibiotics administered"

{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The Haskell consumers of the physiology registry: the wound tick's
--   healing, blood recovery and fever reads (#2830 S5 moved them behind
--   'physioConsumerView'; #2831 S4 activated it).
--
--   Before S4 this module also held a Lua twin-run gate that proved the S5
--   migration changed NOTHING while the registry was inactive (the facade,
--   the contributions, the AI needs and the status rows against the
--   pre-migration statements). With the boundary gone that comparison has no
--   subject: a Nominal pool now reads healthy, an Unsupported one reads
--   absent, and the consumers' behavior under every mode is asserted by the
--   headless group @physiology stat modes@ over the real shipped modules
--   (Test.Headless.Unit.PhysiologyModes). What stays here is the PURE half:
--   'tickOneUnitWith' against 'tickOneUnit' and the closed-form formulas.
module Test.Headless.Unit.PhysiologyConsumerMigration (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.Random as Random
import Combat.Wounds (WoundTickOutcome(..), tickOneUnit, tickOneUnitWith)
import Combat.Wounds.Healing (bloodRecoveryDelta, calorieHealMultiplier)
import Combat.Wounds.Infection
    ( feverSuppressFloor, feverSuppressK, immuneClearRate, immuneRampRate
    , immuneSeed )
import Engine.Asset.Handle (TextureHandle(..))
import Infection.Types (emptyInfectionManager)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.TransferApi (minimalDef)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Physiology.Types
import Unit.Types
import World.Page.Types (WorldPageId(..))

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "physiology_consumer_migration"

-- | A definition that resolves EVERY v1 system to Nominal: the resolver
--   answers healthy values for each owned key.
nominalPhysiology ∷ UnitPhysiology
nominalPhysiology = UnitPhysiology True Nothing $ HM.fromList
    [ (s, PhysioNominal) | s ← ["thermo", "salt", "hydration", "nutrition"] ]

-- | Thermo and salt Nominal; a pool system Simulated exactly when the
--   unit stores its pool, so the stored hydration and calories ARE the
--   unit's and a pool the fixture leaves out reads as the healthy pool.
poolsSimulated ∷ [(Text, Float)] → UnitPhysiology
poolsSimulated stats = UnitPhysiology True Nothing $ HM.fromList
    [ ("thermo", PhysioNominal), ("salt", PhysioNominal)
    , ("hydration", ifStored "hydration"), ("nutrition", ifStored "calories") ]
  where
    ifStored k | k `elem` map fst stats = PhysioSimulated
               | otherwise              = PhysioNominal

-- | The same, all Simulated: the stored values ARE the unit's physiology.
simulatedPhysiology ∷ UnitPhysiology
simulatedPhysiology = UnitPhysiology True Nothing $ HM.fromList
    [ (s, PhysioSimulated) | s ← ["thermo", "salt", "hydration", "nutrition"] ]

mkInst ∷ Text → HM.HashMap Text Float → UnitInstance
mkInst defName stats = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = []
    , uiFaction = resolveLegacyFaction [] FactionPlayer
    , uiWounds = [], uiScars = [], uiImmuneResponse = 0
    , uiImmunities = HM.empty, uiBlood = 70 * 0.075
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiPhysioOverrides = emptyPhysioOverrides
    , uiTrailState = Nothing
    }

-- | @set k v@ replaces or adds; @unset k@ removes (an ABSENT pool is a
--   different state from a zero one, and several consumers branch on it).
set ∷ Text → Float → [(Text, Float)] → [(Text, Float)]
set k v xs = (k, v) : filter ((≢ k) ∘ fst) xs

unset ∷ Text → [(Text, Float)] → [(Text, Float)]
unset k = filter ((≢ k) ∘ fst)


spec ∷ Spec
spec = describe "physiology consumer migration" woundSpec

-- | The Haskell consumers: wound healing, blood recovery, fever.
woundSpec ∷ Spec
woundSpec = describe "the wound tick (Haskell consumers)" $ do

    it "is the identical tick under the shipped registry as with none for a \
       \SIMULATED definition (its stored values ARE its physiology), \
       \populated, depleted and zero, wounded and unwounded" $ do
        reg ← readShippedPhysiologyRegistry
        forM_ (filter ((≢ "pools absent") ∘ fst) woundStates) $ \(label, stats) →
            forM_ [woundedUnit, unwoundedUnit] $ \mk → do
                let u = mk stats
                    summarize (i, o, _) = (observe i, outcomeText o)
                    viaReg = tickOneUnitWith reg 1000 simulatedWound 1
                        emptyInfectionManager Nothing (Random.mkStdGen 1) u False
                    viaNone = tickOneUnit 1000 simulatedWound 1
                        emptyInfectionManager Nothing (Random.mkStdGen 1) u False
                (label, summarize viaReg) `shouldBe` (label, summarize viaNone)

    it "reads a Nominal unit as healthy whatever its stored pools say, and \
       \an Unsupported (bodyless) unit as having no pools at all" $ do
        reg ← readShippedPhysiologyRegistry
        let sick = HM.fromList (set "calories" 100 (set "core_temp" 41 healthyWound))
            fed  = HM.fromList healthyWound
            tickOf d st = firstWound (tickedWith reg d (woundedUnit st))
            nominalSick  = tickOf nominalWound sick
            fullyFed     = tickOf nominalWound fed
            simulatedSick = tickOf simulatedWound sick
            bodyless     = tickOf woundDef sick
            noPools      = tickOf woundDef HM.empty
        -- Nominal: healthy values, so the sick unit behaves as the fed one
        woundHeal nominalSick `shouldSatisfy`
            (\h → abs (h - woundHeal fullyFed) < 1e-6)
        woundInfection nominalSick `shouldSatisfy`
            (\i → abs (i - woundInfection fullyFed) < 1e-6)
        -- Simulated: the same stats are gated and feverish
        woundHeal simulatedSick `shouldSatisfy` (< woundHeal fullyFed)
        woundInfection simulatedSick `shouldSatisfy` (< woundInfection nominalSick)
        -- Unsupported: the stored pools are not read, as if absent
        woundHeal bodyless `shouldSatisfy` (\h → abs (h - woundHeal noPools) < 1e-6)
        woundInfection bodyless `shouldSatisfy`
            (\i → abs (i - woundInfection noPools) < 1e-6)

    it "gates healing on the calorie store exactly as calorieHealMultiplier \
       \says, and scales infection growth by the fever formula" $ do
        reg ← readShippedPhysiologyRegistry
        let tickW w s = firstWound (tickedWith reg simulatedWound (unitWith w (HM.fromList s)))
            -- a clean, uninfected wound: heal advance is linear in the multiplier
            healOf s = woundHeal (tickW cleanWound s) - woundHeal cleanWound
            full = healOf healthyWound
            part = healOf (set "calories" 500 healthyWound)
            mult = calorieHealMultiplier (HM.fromList (set "calories" 500 healthyWound))
        part / full `shouldSatisfy` (\r → abs (r - mult) < 1e-3)
        -- a dirty infected wound: growth before the immune clearance, which
        -- is the same at either core temperature (dt = 1, constitution 1)
        let clearance = immuneClearRate * (immuneRampRate * immuneSeed)
            growth c = woundInfection (tickW dirtyWound (set "core_temp" c healthyWound))
                       - woundInfection dirtyWound + clearance
            want = max feverSuppressFloor (1 - feverSuppressK * (41 - 38))
        growth 38 `shouldSatisfy` (\g → abs (g - growth 37) < 1e-6)   -- no fever below 38
        growth 41 / growth 37 `shouldSatisfy` (\r → abs (r - want) < 1e-3)

    it "recovers blood by the nutrition product, a present pool with no \
       \usable maximum blocking it and an absent pool reading 1.0" $ do
        reg ← readShippedPhysiologyRegistry
        let maxBlood = 70 * bloodMassRatio
            start = uiBlood (unwoundedUnit HM.empty)
            bloodAfter d s = uiBlood (tickedWith reg d (unwoundedUnit (HM.fromList s)))
            expected s = min maxBlood (start + bloodRecoveryDelta (HM.fromList s) maxBlood 1)
            close a b = abs (a - b) < 2e-6
        -- every real body carries both maxima; a pool the case leaves out is
        -- a Nominal one, whose healthy value is its maximum
        let base = healthyBlood ⧺ [("max_hydration", 100), ("max_calories", 2000)]
            with kvs = foldr (\(k, v) acc → set k v acc) base kvs
        forM_ [ ("healthy pools", with [("calories", 2000), ("hydration", 100)])
              , ("calories empty", with [("calories", 0)])
              , ("calories half", with [("calories", 1000)])
              , ("calories present, no max", with [("calories", 1500), ("max_calories", 0)])
              , ("hydration at floor", with [("hydration", 25)])
              , ("hydration below floor", with [("hydration", 24)]) ] $ \(label, s) →
            -- a Simulated definition: its stored pools are what the formula sees
            (label, close (bloodAfter (poolsWound s) s) (expected s)) `shouldBe` (label, True)
        -- an Unsupported definition has no pools: the absent-pool branch
        (close (bloodAfter woundDef (base ⧺ [("calories", 0)]))
               (expected healthyBlood)) `shouldBe` True

    it "reads a NEGATIVE hydration modifier through Healing: a Simulated pool is gated by it, \
       \a Nominal one is not (I1's activated path)" $ do
        reg ← readShippedPhysiologyRegistry
        let stats = HM.fromList (healthyBlood ⧺ [ ("hydration", 100), ("max_hydration", 100)
                                               , ("calories", 2000), ("max_calories", 2000) ])
            drained = (unwoundedUnit stats)
                { uiModifiers = HM.singleton "hydration" [StatModifier (-80) "t" Nothing 0] }
            start = uiBlood (unwoundedUnit stats)
            bloodAfter d = uiBlood (tickedWith reg d drained)
        -- Simulated: the pool reads 20 of 100, below the recovery floor
        bloodAfter (woundDef { udPhysiology = poolsSimulated (HM.toList stats) })
            `shouldSatisfy` (\b → abs (b - start) < 1e-9)
        -- Nominal: the modifier on the key is ignored, so recovery runs
        bloodAfter nominalWound `shouldSatisfy` (> start)

    it "never reads an invariant violation as healthy or absent: a Simulated \
       \pool with no stored value, or a healthy-rule input that is absent or \
       \not positive, returns the unit untouched with the violation" $ do
        reg ← readShippedPhysiologyRegistry
        let simulatedNutrition = woundDef { udPhysiology =
                UnitPhysiology True Nothing
                    (HM.fromList [("nutrition", PhysioSimulated)]) }
            nominalHydration = woundDef { udPhysiology =
                UnitPhysiology True Nothing
                    (HM.fromList [("hydration", PhysioNominal)]) }
            run d st =
                let u = woundedUnit (HM.fromList st)
                    (i, o, _) = tickOneUnitWith reg 1000 d 1 emptyInfectionManager
                                    Nothing (Random.mkStdGen 1) u False
                in (observe i ≡ observe u, outcomeText o)
            violation (untouched, o) =
                untouched ∧ "invariant " `T.isPrefixOf` o
        -- a Simulated pool with no stored value
        violation (run simulatedNutrition (unset "calories" healthyWound)) `shouldBe` True
        -- a healthy-rule input absent, then zero
        violation (run nominalHydration (unset "max_hydration" healthyWound)) `shouldBe` True
        violation (run nominalHydration (set "max_hydration" 0 healthyWound)) `shouldBe` True
        -- and a valid unit is not a violation
        violation (run simulatedNutrition healthyWound) `shouldBe` False

    it "reads a pool against its EFFECTIVE capacity, so a capacity modifier \
       \cannot make a healthy pool look depleted" $ do
        reg ← readShippedPhysiologyRegistry
        let shrink k delta = HM.singleton k [StatModifier delta "test" Nothing 0]
            -- Nominal pools: healthy = effective capacity, so fraction 1.0
            healthyUnder d capKey capVal delta u0 =
                let st = HM.fromList (set capKey capVal healthyWound)
                in tickedWith reg d (u0 st) { uiModifiers = shrink capKey delta }
            plain d capKey capVal u0 =
                tickedWith reg d (u0 (HM.fromList (set capKey capVal healthyWound)))
            close a b = abs (a - b) < 2e-6
        -- blood recovery: stored max_hydration 100, effective 20
        let withMod = healthyUnder nominalWound "max_hydration" 100 (-80) unwoundedUnit
            bare    = plain nominalWound "max_hydration" 20 unwoundedUnit
        (close (uiBlood withMod) (uiBlood bare)) `shouldBe` True
        -- healing: stored max_calories 2000, effective 400
        let healMod = healthyUnder nominalWound "max_calories" 2000 (-1600) woundedUnit
            healBare = plain nominalWound "max_calories" 400 woundedUnit
        woundHeal (firstWound healMod) `shouldSatisfy`
            (\h → abs (h - woundHeal (firstWound healBare)) < 1e-6)
  where
    nominalWound   = woundDef { udPhysiology = nominalPhysiology }
    simulatedWound = woundDef { udPhysiology = simulatedPhysiology }
    poolsWound st  = woundDef { udPhysiology = poolsSimulated st }


tickedWith ∷ PhysiologyRegistry → UnitDef → UnitInstance → UnitInstance
tickedWith reg d u =
    let (i, _, _) = tickOneUnitWith reg 1000 d 1 emptyInfectionManager
                        Nothing (Random.mkStdGen 1) u False
    in i

firstWound ∷ UnitInstance → Wound
firstWound i = case uiWounds i of
    w : _ → w
    []    → error "PhysiologyConsumerMigration: unit has no wound"

outcomeText ∷ WoundTickOutcome → Text
outcomeText NoChange = "no change"
outcomeText (UnconsciousNow p) = "unconscious " <> p
outcomeText (DiedNow p c) = "died " <> p <> " " <> c
outcomeText (PhysioInvariant w) = "invariant " <> w

-- | The wound tick's observable result.
observe ∷ UnitInstance → (Float, [(Float, Float, Float)], Double)
observe i = ( uiBlood i
            , [ (woundHeal w, woundInfection w, woundClot w) | w ← uiWounds i ]
            , realToFrac (uiImmuneResponse i) )

woundDef ∷ UnitDef
woundDef = (minimalDef "t" "T")
    { udBodyParts = [ BodyPart
        { bpId = "l_thigh", bpName = "l_thigh", bpParent = Nothing
        , bpVital = False, bpAreaWeight = 0.1, bpTacticalValue = 0.5
        , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
        , bpLayers = [], bpTargetable = True, bpDepth = 0.0
        , bpAffectsLocomotion = False, bpAffectsBalance = False } ] }

-- | A clotted, dressed wound that heals and (when dirty) festers.
healthyWound ∷ [(Text, Float)]
healthyWound =
    [ ("body_mass", 70), ("constitution", 1.0)
    , ("calories", 2000), ("max_calories", 2000)
    , ("hydration", 100), ("max_hydration", 100), ("core_temp", 37) ]

healthyBlood ∷ [(Text, Float)]
healthyBlood = [("body_mass", 70), ("constitution", 1.0)]

woundStates ∷ [(Text, HM.HashMap Text Float)]
woundStates =
    [ ("healthy", HM.fromList healthyWound)
    , ("depleted", HM.fromList (set "calories" 300 (set "hydration" 30 healthyWound)))
    , ("fever", HM.fromList (set "core_temp" 41 healthyWound))
    , ("present zero", HM.fromList (set "calories" 0 (set "hydration" 0 healthyWound)))
    , ("pools absent", HM.fromList (unset "calories" (unset "hydration"
                           (unset "core_temp" healthyWound)))) ]

unwoundedUnit, woundedUnit ∷ HM.HashMap Text Float → UnitInstance
unwoundedUnit st = (mkInst "t" st) { uiBlood = 4.0, uiWounds = [] }
woundedUnit = unitWith dirtyWound

unitWith ∷ Wound → HM.HashMap Text Float → UnitInstance
unitWith w st = (mkInst "t" st) { uiBlood = 5.25, uiWounds = [w] }

-- | A clotted, dressed slash that is dirty and already infected.
dirtyWound ∷ Wound
dirtyWound = Wound
    { woundPart = "l_thigh", woundKind = "slash", woundSeverity = 0.4
    , woundAt = 0, woundBandage = 0.05, woundClot = 1.0, woundHeal = 0.1
    , woundDressing = "bandage", woundInfection = 0.3, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

-- | The same wound, disinfected and uninfected: it only heals.
cleanWound ∷ Wound
cleanWound = dirtyWound { woundInfection = 0, woundClean = True }


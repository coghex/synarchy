{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The @Unit.Physiology@ gate (#2828, S2 of the physiology stat-modes
--   epic #2827): the system registry, the pure mode resolver and the
--   owned-key read contract, plus (in
--   "Test.Headless.Unit.PhysiologyEngine") the real loader, the unit
--   registration boundary and the registered Lua bindings.
--
--   __This half is pure.__ It decodes and validates the SHIPPED
--   @data\/physiology\/systems.yaml@ and malformed variants through the
--   production decoder, then drives 'resolveMode' and 'physioReadPure'
--   over explicit, fixed inputs: no engine, no generator, no wall clock.
--   Override intent is an explicit resolver input here because the live
--   field is S3's.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit.Physiology"'@.
module Test.Headless.Unit.Physiology (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Engine.Asset.YamlPhysiology
    (PhysiologyRefusal(..), PhysiologyYamlDoc, admitPhysiologyDoc)
import Test.Headless.Unit.PhysiologyEngine (engineSpec)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Types (StatModifier(..))

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

bodied ∷ UnitPhysiology
bodied = noUnitPhysiology { upBodyAuthored = True }

withDefaults ∷ [(Text, PhysioMode)] → UnitPhysiology
withDefaults ds = bodied { upDefaults = HM.fromList ds }

allSimulated, allNominal ∷ UnitPhysiology
allSimulated = withDefaults [ (s, PhysioSimulated) | s ← systemIds ]
allNominal   = withDefaults [ (s, PhysioNominal)   | s ← systemIds ]

systemIds ∷ [Text]
systemIds = ["hydration", "salt", "thermo", "nutrition", "stamina"
            , "exhaustion", "sleep"]

-- | The reference 70 kg unit's INPUTS: the stats a healthy rule reads.
inputs ∷ HM.HashMap Text Float
inputs = HM.fromList
    [ ("body_mass", 70), ("max_hydration", 42)
    , ("max_hunger", 700), ("max_calories", 1400), ("endurance", 1) ]

-- | Its stored OWNED values, as a Simulated unit would carry them.
owned ∷ HM.HashMap Text Float
owned = HM.fromList
    [ ("hydration", 21), ("salt", 35), ("salt_store", 100)
    , ("core_temp", 38), ("hunger", 300), ("calories", 900), ("stamina", 6)
    , ("exhaustion", 4), ("sleep_pressure", 7) ]

mkCtx ∷ PhysiologyRegistry → UnitPhysiology → HM.HashMap Text Float
      → HM.HashMap Text [StatModifier] → Double → PhysioContext
mkCtx reg up stats mods now = PhysioContext
    { pcRegistry = reg, pcPhysiology = up, pcOverrides = HM.empty
    , pcNow = now, pcStats = stats, pcModifiers = mods }

simCtx ∷ PhysiologyRegistry → PhysioContext
simCtx reg = mkCtx reg allSimulated (HM.union owned inputs) HM.empty 0

nomCtx ∷ PhysiologyRegistry → PhysioContext
nomCtx reg = mkCtx reg allNominal inputs HM.empty 0

mkMod ∷ Float → Maybe Double → StatModifier
mkMod d e = StatModifier { smDelta = d, smSource = "spec", smExpiry = e
                         , smPercent = 0 }

eff, base ∷ PhysioContext → Text → PhysioOutcome
eff = physioReadPure ReadEffective
base = physioReadPure ReadBase

value ∷ ResolvedMode → Float → PhysioOutcome
value = OutcomeValue

near ∷ Float → PhysioOutcome → Expectation
near want got = case got of
    OutcomeValue _ v → abs (v - want) `shouldSatisfy` (< 1e-4)
    other → expectationFailure ("expected ~" ⧺ show want ⧺ ", got "
                                ⧺ show other)

isInvariant ∷ PhysioOutcome → Bool
isInvariant (OutcomeInvariant _) = True
isInvariant _                    = False

-- | Decode a registry document from text and admit it.
admitText ∷ String → Either String (Either PhysiologyRefusal PhysiologyRegistry)
admitText txt = case Yaml.decodeEither' (BC.pack txt) of
    Left e → Left (show e)
    Right (doc ∷ PhysiologyYamlDoc) →
        Right (admitPhysiologyDoc "spec.yaml" doc)

refusedWith ∷ Text → Text → String → Expectation
refusedWith reason detail txt = case admitText txt of
    Right (Left r) → (refusalReason r, refusalDetail r) `shouldBe` (reason, detail)
    Right (Right _) → expectationFailure "the registry was admitted"
    Left e → expectationFailure ("the registry did not decode: " ⧺ e)

-- | A small valid registry, assembled from parts so each refusal test
--   changes exactly one thing.
registryText ∷ [String] → [String] → [String] → String
registryText consts systems profiles = unlines $
    ["constants:"] ⧺ consts ⧺ ["systems:"] ⧺ systems ⧺ ["profiles:"] ⧺ profiles

okConsts ∷ [String]
okConsts = ["  K: 2.0"]

alpha ∷ [String]
alpha =
    [ "  - id: alpha"
    , "    requires: [body]"
    , "    owns:"
    , "      - key: a_pool"
    , "        healthy: { from: max_a, scale: [K] }"
    , "    meters: [a_meter]"
    , "    provides:"
    , "      - { name: a_value, substitute: { value: K } }"
    , "    effects:"
    , "      - { name: a_kill, substitute: none }" ]

okProfiles ∷ [String]
okProfiles = ["  tier:", "    alpha: simulated"]

-----------------------------------------------------------------------
-- The gate
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "Unit.Physiology" $ do
    beforeAll readShippedPhysiologyRegistry $ do
        shippedRegistrySpec
        resolverSpec
        ownedReadSpec
        saltConcentrationSpec
    registryRefusalSpec
    engineSpec

-----------------------------------------------------------------------
-- Requirement 1: the shipped registry
-----------------------------------------------------------------------

shippedRegistrySpec ∷ SpecWith PhysiologyRegistry
shippedRegistrySpec = describe "the shipped registry (requirement 1)" $ do

    it "declares exactly the seven systems, in tick order (thermo \
       \before salt, design D-7), each requiring a body" $ \reg → do
        map psId (prSystems reg)
            `shouldBe` [ "thermo", "salt", "hydration", "nutrition", "stamina"
                       , "exhaustion", "sleep" ]
        forM_ (prSystems reg) $ \s → psRequires s `shouldBe` ["body"]

    it "owns, derives and meters exactly the v1 keys" $ \reg → do
        let table s = ( map okKey (psOwns s), map dkKey (psDerived s)
                      , psMeters s )
            byId = Map.fromList [ (psId s, table s) | s ← prSystems reg ]
        byId `shouldBe` Map.fromList
            [ ("hydration", (["hydration"], [], []))
            , ("salt", (["salt", "salt_store"], ["salt_conc"]
                       , ["salt_imbalance"]))
            , ("thermo", (["core_temp"], [], ["hypothermia", "hyperthermia"]))
            , ("nutrition", (["hunger", "calories"], [], []))
            , ("stamina", (["stamina"], [], []))
            , ("exhaustion", (["exhaustion"], [], []))
            , ("sleep", (["sleep_pressure"], [], [])) ]

    it "keeps the constants the healthy rules need as authoritative data" $
        \reg → prConstants reg `shouldBe` Map.fromList
            [ ("SALT_PER_KG", 1.0), ("SALT_STORE_CAP_FRAC", 2.0)
            , ("THERMO_BASELINE", 37.0) ]

    it "declares each healthy rule as the design states it" $ \reg → do
        let rule k = case classifyKey reg k of
                Just (_, RoleOwned o) → Just (okHealthy o)
                _                     → Nothing
        rule "hydration" `shouldBe` Just (HealthyFrom "max_hydration" [])
        rule "salt" `shouldBe`
            Just (HealthyFrom "body_mass" [QConst "SALT_PER_KG"])
        rule "salt_store" `shouldBe` Just
            (HealthyFrom "body_mass"
                [QConst "SALT_PER_KG", QConst "SALT_STORE_CAP_FRAC"])
        rule "core_temp" `shouldBe` Just (HealthyConst (QConst "THERMO_BASELINE"))
        rule "hunger" `shouldBe` Just (HealthyFrom "max_hunger" [])
        rule "calories" `shouldBe` Just (HealthyFrom "max_calories" [])
        -- stamina's seed target is the EFFECTIVE max_stamina, which is
        -- derived and never stored, so its rule is the named one (#2834)
        rule "stamina" `shouldBe` Just HealthyMaxStamina
        -- exhaustion and sleep (#2835): their maxima are Lua-only derived
        -- stats, never stored, so the rule restates the derivation
        -- (effective endurance x 10) in the existing grammar -- a `from:` of
        -- the max_* key would be an input that is never stored and would
        -- refuse every spawn.
        rule "exhaustion" `shouldBe` Just (HealthyFrom "endurance" [QLit 10])
        rule "sleep_pressure" `shouldBe`
            Just (HealthyFrom "endurance" [QLit 10])
        case classifyKey reg "salt_conc" of
            Just (_, RoleDerived d) → do
                dkRule d `shouldBe` DerivedSaltConcentration
                dkHealthy d `shouldBe` 1.0
            other → expectationFailure ("salt_conc is not derived: "
                                        ⧺ show other)

    it "evaluates every healthy rule against effective inputs (a Nominal \
       \read of the reference 70 kg unit)" $ \reg → do
        let c = nomCtx reg
        forM_ [ ("hydration", 42), ("salt", 70), ("salt_store", 140)
              , ("core_temp", 37), ("hunger", 700), ("calories", 1400)
              , ("stamina", 10), ("exhaustion", 10), ("sleep_pressure", 10)
              , ("salt_conc", 1) ] $ \(k, want) →
            near want (eff c k)

    it "records every Nominal substitute (D-1), including the two S4 \
       \executes" $ \reg → do
        let entries section s =
                [ (ieName e, ieSubstitute e) | e ← section s ]
            sys i = fromMaybe (error "no such system")
                              (registrySystem reg i)
        entries psProvides (sys "hydration") `shouldBe`
            [("body_water_fraction", SubValue (QLit 1.0))]
        entries psProvides (sys "salt") `shouldBe`
            [("salt_conc", SubValue (QLit 1.0))
            ,("sweat_factor", SubValue (QLit 1.0))]
        entries psProvides (sys "thermo") `shouldBe`
            [("core_temp", SubValue (QConst "THERMO_BASELINE"))]
        entries psContributes (sys "thermo") `shouldBe`
            [("sweat_debit", SubNone), ("shiver_debit", SubNone)]
        entries psProvides (sys "nutrition") `shouldBe`
            [("cal_factor", SubValue (QLit 1.0)), ("strength_discount", SubNone)]
        entries psProvides (sys "stamina") `shouldBe`
            [("stamina_fraction", SubValue (QLit 1.0))]
        -- S8b (#2835): constants, never ratios
        entries psProvides (sys "exhaustion") `shouldBe`
            [("restedness_fraction", SubValue (QLit 1.0))
            ,("movement_multiplier", SubValue (QLit 1.0))]
        entries psContributes (sys "exhaustion") `shouldBe`
            [("sleep_exhaustion_deficit", SubValue (QLit 0.0))]
        entries psProvides (sys "sleep") `shouldBe`
            [("sleep_deficit", SubValue (QLit 0.0))]
        entries psContributes (sys "nutrition") `shouldBe`
            [("meal_salt_credit", SubVirtualMeal)
            ,("tissue_maintenance", SubTissueRecovery)]
        -- every effect is absent while its system is Nominal
        forM_ (prSystems reg) $ \s →
            all ((≡ SubNone) ∘ ieSubstitute) (psEffects s) `shouldBe` True

    it "defines the four profiles, each a mode for EVERY system" $ \reg → do
        let modes p = fmap ppModes (registryProfile reg p)
            m = Map.fromList
        modes "rich" `shouldBe` Just (m
            [(s, PhysioSimulated) | s ← systemIds])
        modes "reduced" `shouldBe` Just (m
            [ ("hydration", PhysioNominal), ("salt", PhysioSimulated)
            , ("thermo", PhysioSimulated), ("nutrition", PhysioNominal)
            , ("stamina", PhysioSimulated), ("exhaustion", PhysioSimulated)
            , ("sleep", PhysioSimulated) ])
        modes "simple" `shouldBe` Just (m
            [ ("hydration", PhysioNominal), ("salt", PhysioNominal)
            , ("thermo", PhysioSimulated), ("nutrition", PhysioNominal)
            , ("stamina", PhysioSimulated), ("exhaustion", PhysioSimulated)
            , ("sleep", PhysioNominal) ])
        modes "exempt" `shouldBe` Just (m
            [(s, PhysioNominal) | s ← systemIds])
        map ppId (prProfiles reg) `shouldMatchList`
            ["rich", "reduced", "simple", "exempt"]

-----------------------------------------------------------------------
-- Requirement 1: malformed registries are diagnosed, never accepted
-----------------------------------------------------------------------

registryRefusalSpec ∷ Spec
registryRefusalSpec = describe "registry validation (requirement 1)" $ do

    it "admits the minimal control registry the refusals below mutate, \
       \so none of them passes for the wrong reason" $
        fmap (fmap (map psId ∘ prSystems))
             (admitText (registryText okConsts alpha okProfiles))
            `shouldBe` Right (Right ["alpha"])

    it "refuses an empty registry" $
        refusedWith "empty physiology registry" "spec.yaml"
            (registryText okConsts [] [])

    it "refuses a system declared twice, naming it" $
        refusedWith "duplicate physiology system" "alpha"
            (registryText okConsts (alpha ⧺ alpha) [])

    it "refuses a key owned by two systems, naming it" $
        refusedWith "duplicate physiology key" "a_pool"
            (registryText okConsts
                (alpha ⧺
                    [ "  - id: beta"
                    , "    owns:"
                    , "      - { key: a_pool, healthy: { const: 1.0 } }" ])
                [])

    it "refuses a capability no species can have" $
        refusedWith "unknown physiology capability" "alpha: wings"
            (registryText okConsts
                [ "  - id: alpha", "    requires: [wings]"
                , "    owns:"
                , "      - { key: a_pool, healthy: { const: 1.0 } }" ] [])

    it "refuses a system that owns nothing" $
        refusedWith "incomplete physiology system" "alpha owns nothing"
            (registryText okConsts ["  - id: alpha", "    requires: [body]"] [])

    it "refuses an owned key with no healthy rule" $
        refusedWith "owned physiology key without a healthy rule" "alpha.a_pool"
            (registryText okConsts
                [ "  - id: alpha", "    owns:", "      - key: a_pool" ] [])

    it "refuses a rule naming a constant nothing declares" $
        refusedWith "unknown physiology constant" "alpha.a_pool: NOPE"
            (registryText okConsts
                [ "  - id: alpha", "    owns:"
                , "      - { key: a_pool, healthy: { from: x, scale: NOPE } }" ]
                [])

    it "refuses a non-finite constant" $
        refusedWith "non-finite physiology constant" "K"
            (registryText ["  K: 1.0e999"] alpha [])

    it "refuses a non-finite literal anywhere it can be written: a healthy \
       \constant, a scale factor, a literal substitute, a derived healthy \
       \value" $ do
        let one extra = registryText okConsts
                (["  - id: alpha", "    owns:"] ⧺ extra) []
        refusedWith "non-finite physiology value" "alpha.a_pool: Infinity"
            (one ["      - { key: a_pool, healthy: { const: 1.0e999 } }"])
        refusedWith "non-finite physiology value" "alpha.a_pool: Infinity"
            (one [ "      - { key: a_pool, healthy: { from: x, scale: 1.0e999 } }" ])
        refusedWith "non-finite physiology value" "alpha.a_pool: Infinity"
            (one [ "      - { key: a_pool, healthy: { from: x, scale: [K, 1.0e999] } }" ])
        refusedWith "non-finite physiology value" "alpha.provides.v: Infinity"
            (one [ "      - { key: a_pool, healthy: { const: 1.0 } }"
                 , "    provides:"
                 , "      - { name: v, substitute: { value: 1.0e999 } }" ])
        refusedWith "non-finite physiology value" "alpha.d: Infinity"
            (one [ "      - { key: a_pool, healthy: { const: 1.0 } }"
                 , "    derived:"
                 , "      - { key: d, rule: salt_concentration, healthy: 1.0e999 }" ])

    it "refuses an interface entry with no substitute" $
        refusedWith "physiology interface entry without a substitute"
            "alpha.provides.a_value"
            (registryText okConsts
                [ "  - id: alpha", "    owns:"
                , "      - { key: a_pool, healthy: { const: 1.0 } }"
                , "    provides:", "      - { name: a_value }" ] [])

    it "refuses a substitute that is not recognized" $
        refusedWith "unknown physiology substitute" "alpha.effects.boom: explode"
            (registryText okConsts
                [ "  - id: alpha", "    owns:"
                , "      - { key: a_pool, healthy: { const: 1.0 } }"
                , "    effects:", "      - { name: boom, substitute: explode }" ]
                [])

    it "refuses a derived key whose rule is not a known pure function" $
        refusedWith "unknown physiology derived rule" "alpha.d: nonsense"
            (registryText okConsts
                [ "  - id: alpha", "    owns:"
                , "      - { key: a_pool, healthy: { const: 1.0 } }"
                , "    derived:"
                , "      - { key: d, rule: nonsense, healthy: 1.0 }" ] [])

    it "refuses the salt concentration rule without the keys it reads" $
        refusedWith "unresolved physiology derived rule input"
            "alpha.d needs owned key salt"
            (registryText okConsts
                [ "  - id: alpha", "    owns:"
                , "      - { key: a_pool, healthy: { const: 1.0 } }"
                , "    derived:"
                , "      - { key: d, rule: salt_concentration, healthy: 1.0 }" ]
                [])

    it "refuses a healthy rule that reads a registry key" $
        refusedWith "physiology healthy rule reads a registry key"
            "alpha.a_pool <- a_pool"
            (registryText okConsts
                [ "  - id: alpha", "    owns:"
                , "      - { key: a_pool, healthy: { from: a_pool } }" ] [])

    it "refuses a profile naming an unknown system" $
        refusedWith "unknown physiology system in profile" "tier: ghost"
            (registryText okConsts alpha
                ["  tier:", "    alpha: simulated", "    ghost: nominal"])

    it "refuses a profile with a mode that is not nominal or simulated" $
        refusedWith "invalid physiology mode" "tier: alpha: maybe"
            (registryText okConsts alpha ["  tier:", "    alpha: maybe"])

    it "refuses a profile that leaves a system out" $
        refusedWith "incomplete physiology profile" "tier: beta"
            (registryText okConsts
                (alpha ⧺
                    [ "  - id: beta", "    owns:"
                    , "      - { key: b_pool, healthy: { const: 1.0 } }" ])
                okProfiles)

    it "fails to decode a system carrying a key it does not know" $
        either (const True) (const False)
               (admitText (registryText okConsts
                   (alpha ⧺ ["    surprise: true"]) []))
            `shouldBe` True

-----------------------------------------------------------------------
-- Requirement 3: the resolver
-----------------------------------------------------------------------

resolverSpec ∷ SpecWith PhysiologyRegistry
resolverSpec = describe "the pure mode resolver (requirement 3)" $ do

    it "resolves override, then species default, then Nominal — all nine \
       \combinations" $ \reg → do
        let sys = fromMaybe (error "no thermo") (registrySystem reg "thermo")
            expectedFor dflt ovr = case (ovr, dflt) of
                (Just o, _)       → toResolved o
                (Nothing, Just d) → toResolved d
                (Nothing, Nothing) → ModeNominal
            toResolved PhysioNominal   = ModeNominal
            toResolved PhysioSimulated = ModeSimulated
        forM_ [Nothing, Just PhysioNominal, Just PhysioSimulated] $ \dflt →
            forM_ [Nothing, Just PhysioNominal, Just PhysioSimulated] $ \ovr →
                resolveMode sys (bodied { upDefaults = maybe HM.empty
                                              (HM.singleton "thermo") dflt })
                                False ovr
                    `shouldBe` expectedFor dflt ovr

    it "tells an ABSENT override from one that merely equals the default: \
       \only the absent one follows the default" $ \reg → do
        let sys = fromMaybe (error "no thermo") (registrySystem reg "thermo")
            resolveWith dflt ovr = resolveMode sys
                (withDefaults [("thermo", dflt)]) False ovr
        -- same default, same OUTCOME …
        resolveWith PhysioSimulated Nothing
            `shouldBe` resolveWith PhysioSimulated (Just PhysioSimulated)
        -- … but move the default and the two inputs part ways.
        resolveWith PhysioNominal Nothing `shouldBe` ModeNominal
        resolveWith PhysioNominal (Just PhysioSimulated)
            `shouldBe` ModeSimulated

    it "yields Unsupported whenever a capability is missing, whatever \
       \the default, the override or the stored keys say" $ \reg →
        forM_ (prSystems reg) $ \sys → do
            let bodyless = noUnitPhysiology
                    { upDefaults = HM.fromList
                        [ (psId sys, PhysioSimulated) ] }
            forM_ [Nothing, Just PhysioNominal, Just PhysioSimulated] $ \ovr →
                resolveMode sys bodyless False ovr `shouldBe` ModeUnsupported
            -- stored keys never rescue it
            modeOf (mkCtx reg bodyless (HM.union owned inputs) HM.empty 0) sys
                `shouldBe` ModeUnsupported

    it "never lets a stored key choose the mode: inserting or removing \
       \owned keys changes nothing" $ \reg → do
        let up = withDefaults [("thermo", PhysioSimulated)]
            modeWith stats = map (modeOf (mkCtx reg up stats HM.empty 0))
                                 (prSystems reg)
            full = HM.union owned inputs
        modeWith full `shouldBe` modeWith HM.empty
        modeWith (HM.delete "core_temp" full) `shouldBe` modeWith full
        modeWith (HM.insert "core_temp" 12 inputs) `shouldBe` modeWith full

    it "derives the body capability from what the YAML authored" $ \_ → do
        unitCapabilities noUnitPhysiology `shouldBe` mempty
        unitCapabilities bodied `shouldBe` unitCapabilities
            (bodied { upProfile = Just "rich" })

-----------------------------------------------------------------------
-- Requirement 4: owned reads
-----------------------------------------------------------------------

ownedReadSpec ∷ SpecWith PhysiologyRegistry
ownedReadSpec = describe "owned-key reads (requirement 4)" $ do

    it "reads a Nominal key through its healthy rule against EFFECTIVE \
       \inputs, and ignores modifiers on the key itself" $ \reg → do
        let mods = HM.fromList
                [ ("max_hydration", [mkMod 8 Nothing])   -- an input: counts
                , ("hydration", [mkMod 100 Nothing])     -- the key: ignored
                , ("core_temp", [mkMod (-5) Nothing]) ]
            c = mkCtx reg allNominal inputs mods 0
        eff c "hydration" `shouldBe` value ModeNominal 50
        eff c "core_temp" `shouldBe` value ModeNominal 37
        -- a Nominal key has no base distinct from its healthy value
        base c "hydration" `shouldBe` eff c "hydration"

    it "reads a Simulated key as stored base plus active modifiers, and \
       \the base read as the stored base" $ \reg → do
        let mods = HM.fromList [("hydration", [mkMod 5 (Just 100)])]
            at now = mkCtx reg allSimulated (HM.union owned inputs) mods now
        eff (at 50) "hydration" `shouldBe` value ModeSimulated 26
        base (at 50) "hydration" `shouldBe` value ModeSimulated 21
        -- the modifier expires on its own, with no tick
        eff (at 150) "hydration" `shouldBe` value ModeSimulated 21
        eff (at 100) "hydration" `shouldBe` value ModeSimulated 21

    it "reads a failure meter as 0 when Nominal, and as its stored value \
       \(zero included) when Simulated" $ \reg → do
        let stale = HM.fromList [("hypothermia", 0.7), ("salt_imbalance", 0)]
        -- Nominal ignores a stale legacy value outright
        eff (mkCtx reg allNominal (HM.union stale inputs) HM.empty 0)
            "hypothermia" `shouldBe` value ModeNominal 0
        let sim = mkCtx reg allSimulated
                      (HM.unions [stale, owned, inputs]) HM.empty 0
        eff sim "hypothermia" `shouldBe` value ModeSimulated 0.7
        eff sim "salt_imbalance" `shouldBe` value ModeSimulated 0
        -- an absent Simulated meter reads 0 and is not an error (D-5)
        eff sim "hyperthermia" `shouldBe` value ModeSimulated 0

    it "reads every key of a bodyless unit as Unsupported" $ \reg → do
        let c = mkCtx reg noUnitPhysiology (HM.union owned inputs) HM.empty 0
        forM_ (Map.keys (Map.fromList
                [ (k, ()) | k ← ["hydration", "salt", "salt_store", "core_temp"
                               , "hunger", "calories", "salt_conc"
                               , "salt_imbalance", "hypothermia"
                               , "hyperthermia", "stamina", "exhaustion"
                               , "sleep_pressure"] ])) $ \k → do
            eff c k `shouldBe` OutcomeUnsupported
            base c k `shouldBe` OutcomeUnsupported

    it "hands an unowned key back for the caller to read as getStat does" $
        \reg → forM_ ["heart_rate", "max_stamina", "max_sleep_pressure", "strength"
              , "nonesuch"] $ \k →
            eff (simCtx reg) k `shouldBe` OutcomeUnowned

    it "raises an invariant error, never a fallback, for a Simulated \
       \owned key with no stored value" $ \reg →
        forM_ ["hydration", "salt", "salt_store", "core_temp", "hunger"
              , "calories", "stamina", "exhaustion", "sleep_pressure"] $ \k → do
            let c = mkCtx reg allSimulated
                        (HM.delete k (HM.union owned inputs)) HM.empty 0
            eff c k `shouldSatisfy` isInvariant
            base c k `shouldSatisfy` isInvariant

    it "raises an invariant error for a healthy-rule input that is absent, \
       \non-finite or non-positive" $ \reg → do
        let nom stats = mkCtx reg allNominal stats HM.empty 0
        forM_ [ ("hydration", "max_hydration"), ("salt", "body_mass")
              , ("salt_store", "body_mass"), ("hunger", "max_hunger")
              , ("calories", "max_calories") ] $ \(key, input) → do
            eff (nom (HM.delete input inputs)) key `shouldSatisfy` isInvariant
            eff (nom (HM.insert input 0 inputs)) key `shouldSatisfy` isInvariant
            eff (nom (HM.insert input (-3) inputs)) key `shouldSatisfy` isInvariant
            eff (nom (HM.insert input (0 / 0) inputs)) key
                `shouldSatisfy` isInvariant
            eff (nom (HM.insert input (1 / 0) inputs)) key
                `shouldSatisfy` isInvariant
        -- the same for a stored input a positive modifier would rescue
        let lifted input stored = mkCtx reg allNominal
                (HM.insert input stored inputs)
                (HM.fromList [(input, [mkMod 50 Nothing])]) 0
        forM_ [ ("hydration", "max_hydration"), ("salt", "body_mass")
              , ("hunger", "max_hunger"), ("calories", "max_calories") ] $
            \(key, input) → do
                eff (lifted input 0) key `shouldSatisfy` isInvariant
                eff (lifted input (-3)) key `shouldSatisfy` isInvariant
        eff (lifted "max_hydration" 42) "hydration"
            `shouldBe` value ModeNominal 92
        -- the constant-rule key needs no input at all
        eff (nom HM.empty) "core_temp" `shouldBe` value ModeNominal 37

-----------------------------------------------------------------------
-- Requirement 4: salt concentration
-----------------------------------------------------------------------

saltConcentrationSpec ∷ SpecWith PhysiologyRegistry
saltConcentrationSpec = describe "on-demand salt concentration (D-5a)" $ do

    let up salt hyd = withDefaults [("salt", salt), ("hydration", hyd)
                                   , ("thermo", PhysioSimulated)
                                   , ("nutrition", PhysioSimulated)]
        stats = HM.insert "salt" 35 (HM.insert "hydration" 31.5
                    (HM.union owned inputs))

    it "is the shared pure rule: (salt/maxSalt) / clamp(hydration \
       \fraction, 0.05, 1)" $ \_ → do
        saltConcentration 0.5 0.75 `shouldSatisfy` (\v → abs (v - 0.6667) < 1e-3)
        saltConcentration 0.5 0.0 `shouldBe` 10    -- clamped at 0.05
        saltConcentration 0.5 5.0 `shouldBe` 0.5   -- clamped at 1

    it "covers all four salt × hydration combinations" $ \reg → do
        let at salt hyd = eff (mkCtx reg (up salt hyd) stats HM.empty 0)
                              "salt_conc"
        -- Nominal salt reads 1.0 whatever hydration is
        at PhysioNominal PhysioNominal `shouldBe` value ModeNominal 1
        at PhysioNominal PhysioSimulated `shouldBe` value ModeNominal 1
        -- Simulated salt over Nominal hydration: water fraction 1.0 (F-1)
        near 0.5 (at PhysioSimulated PhysioNominal)
        -- Simulated over Simulated: 0.5 / (31.5 / 42)
        near (0.5 / 0.75) (at PhysioSimulated PhysioSimulated)

    it "never reads a stored legacy salt_conc, and follows its inputs the \
       \moment they change — no tick between reads" $ \reg → do
        let legacy = HM.insert "salt_conc" 0.123 stats
            at s = eff (mkCtx reg (up PhysioSimulated PhysioSimulated) s
                                  HM.empty 0) "salt_conc"
        near (0.5 / 0.75) (at legacy)
        near (0.5 / 0.75) (at stats)
        -- salt up, then water down: each read reflects the new inputs
        near (0.8 / 0.75) (at (HM.insert "salt" 56 legacy))
        near (0.5 / 0.5) (at (HM.insert "hydration" 21 legacy))
        -- and the base read of a derived key is the same on-demand value
        base (mkCtx reg (up PhysioSimulated PhysioSimulated) legacy HM.empty 0)
            "salt_conc"
            `shouldBe` at legacy

    it "applies active modifiers on its inputs (they are effective reads)" $
        \reg → do
            let mods = HM.fromList [("salt", [mkMod 21 Nothing])]
            near (0.8 / 0.75)
                (eff (mkCtx reg (up PhysioSimulated PhysioSimulated) stats mods 0)
                     "salt_conc")

    it "is an invariant error when a Simulated input is absent or the \
       \capacity is unusable" $ \reg → do
        let sim = up PhysioSimulated PhysioSimulated
            at s = eff (mkCtx reg sim s HM.empty 0) "salt_conc"
        at (HM.delete "salt" stats) `shouldSatisfy` isInvariant
        at (HM.delete "hydration" stats) `shouldSatisfy` isInvariant
        at (HM.delete "max_hydration" stats) `shouldSatisfy` isInvariant
        at (HM.delete "body_mass" stats) `shouldSatisfy` isInvariant
        at (HM.insert "max_hydration" 0 stats) `shouldSatisfy` isInvariant
        -- a Nominal hydration needs no pool, so its absence is fine
        near 0.5 (eff (mkCtx reg (up PhysioSimulated PhysioNominal)
                               (HM.delete "hydration"
                                   (HM.delete "max_hydration" stats))
                               HM.empty 0)
                      "salt_conc")

    it "raises an invariant error, not a clamped reading, for a Simulated \
       \hydration pool that is negative, NaN or infinite, and reads a present \
       \ZERO pool through the 0.05 clamp — the clamp and effectiveStat must \
       \not hide an invalid dependency, and an empty pool is not one" $ \reg → do
        let sim = up PhysioSimulated PhysioSimulated
            at s mods = eff (mkCtx reg sim s mods 0) "salt_conc"
        forM_ [-3, 0 / 0, 1 / 0, -1 / 0] $ \bad →
            at (HM.insert "hydration" bad stats) HM.empty
                `shouldSatisfy` isInvariant
        -- a positive modifier must not lift a NEGATIVE stored pool into a
        -- plausible effective one
        at (HM.insert "hydration" (-3) stats)
           (HM.fromList [("hydration", [mkMod 24 Nothing])])
            `shouldSatisfy` isInvariant
        -- an EMPTY pool is a valid reading (a dehydrated unit's): the
        -- concentration is salt over the clamped 0.05 floor (#2831)
        let empty = at (HM.insert "hydration" 0 stats) HM.empty
        near (0.5 / 0.05) empty
        -- a modifier that drives the EFFECTIVE pool to zero reads the same
        near (0.5 / 0.05)
            (at stats (HM.fromList [("hydration", [mkMod (-100) Nothing])]))
        -- the lowest valid pools still read, through the 0.05 clamp
        near 10 (at (HM.insert "hydration" 0.001 stats) HM.empty)
        -- and the base read of the derived key judges it the same way
        base (mkCtx reg sim (HM.insert "hydration" (1 / 0) stats) HM.empty 0)
            "salt_conc" `shouldSatisfy` isInvariant

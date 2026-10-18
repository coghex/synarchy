{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Loading the SHIPPED physiology registry into a headless engine
--   (#2828), for every spec that registers real unit definitions.
--
--   Unit registration refuses to run with no registry loaded — a
--   definition cannot be validated against nothing, and skipping the
--   check would let a templated physiology key pass vacuously — so a
--   spec that feeds @registerUnitDefs@ or @engine.loadUnitYaml@ a real
--   file loads the registry first, exactly as @startup_loader.lua@
--   queues @data\/physiology@ before @data\/units@.
--
--   The registry goes in through the production decoder and validator
--   ('admitPhysiologyDoc'), not a hand-built value, so a spec that
--   needs it also proves the shipped file is admissible.
module Test.Headless.Unit.PhysiologyFixture
    ( shippedPhysiologyPath
    , readShippedPhysiologyRegistry
    , ensurePhysiologyRegistry
    , profilePhysiology
    , withProfile
    , richPhysiology
    , exemptPhysiology
    , nutritionOnlyPhysiology
    , staminaOnlyPhysiology
    , mindSeedStats
    , hydrationBrainPhysiology
    , hydrationBrainStats
    , oxygenForConsciousness
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef)
import System.FilePath ((</>))
import Engine.Asset.YamlPhysiology
    (admitPhysiologyDoc, loadPhysiologyYamlOutcome)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Log
    (LogBackend(..), LogConfig(..), defaultLogConfig, initLogger)
import Engine.Core.State (EngineEnv)
import Unit.Physiology (resolveUnitPhysiology)
import Unit.Physiology.Types
import Unit.Types.Def (UnitDef(..))

shippedPhysiologyPath ∷ FilePath
shippedPhysiologyPath = "data" </> "physiology" </> "systems.yaml"

-- | The shipped registry, decoded and admitted. Fails the spec loudly if
--   the shipped file stops being admissible. Needs no engine.
readShippedPhysiologyRegistry ∷ IO PhysiologyRegistry
readShippedPhysiologyRegistry = do
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\_ → pure ()) }
    mDoc ← loadPhysiologyYamlOutcome logger shippedPhysiologyPath
    case mDoc of
        Nothing → error (shippedPhysiologyPath ⧺ " did not decode")
        Just doc → case admitPhysiologyDoc shippedPhysiologyPath doc of
            Left r  → error (shippedPhysiologyPath ⧺ " was refused: "
                             ⧺ show r)
            Right reg → pure reg

-- | Register the shipped registry into this engine unless one is
--   already there. Guarded rather than unconditional because a shared
--   engine (an @aroundAll@ fixture) is handed to many examples.
ensurePhysiologyRegistry ∷ EngineEnv → IO ()
ensurePhysiologyRegistry env = do
    let ref = crPhysiologyRegistryRef (toContentRegistriesCapability env)
    current ← readIORef ref
    unless (physiologyRegistryLoaded current) $
        readShippedPhysiologyRegistry ≫= writeIORef ref

-- | A definition's physiology under a SHIPPED profile (@rich@, @reduced@,
--   @simple@, @exempt@), with a @body:@ authored: what the profile block
--   in @data\/units\/*.yaml@ resolves to. Fails the spec loudly for a name
--   the shipped registry does not declare.
profilePhysiology ∷ Text → IO UnitPhysiology
profilePhysiology profile = do
    reg ← readShippedPhysiologyRegistry
    case resolveUnitPhysiology reg "fixture" True (Just profile) [] [] of
        Left r   → error ("profilePhysiology " ⧺ show profile ⧺ ": " ⧺ show r)
        Right up → pure up

-- | A bodied fixture definition: the given one, carrying the profile.
withProfile ∷ Text → UnitDef → IO UnitDef
withProfile profile def = do
    up ← profilePhysiology profile
    pure def { udPhysiology = up }

-- | The mind keys (#2838, S11a) a Simulated mind is stored with when the unit
--   was seeded at game time 0 and is healthy: mood at its drift target (1.0),
--   the roll cadence one @MENTAL_CHECK_INTERVAL@ ahead, the thought clock at
--   the seeding sample, everything else at rest. Consciousness,
--   concentration and state_of_mind are DERIVED: a fixture stores none.
mindSeedStats ∷ [(Text, Float)]
mindSeedStats =
    [ ("mood", 1), ("emotional_pain", 0), ("caffeine", 0), ("mental_state", 0)
    , ("mental_since", 0), ("mental_until", 0), ("mental_next_roll_at", 8)
    , ("mental_cooldown_until", 0), ("mental_high_since", 0)
    , ("mental_break_behavior", 0), ("thought_next_at", 0) ]

-- | Hydration, cardio, consciousness and mind Simulated, every other system
--   Nominal: for a fixture that drives the hydration pool and the brain's
--   derived consciousness (#2838) and nothing else (the source-drinking
--   gates). Cardio is Simulated because it owns the blood oxygen a fixture
--   moves to reach a consciousness band.
hydrationBrainPhysiology ∷ UnitPhysiology
hydrationBrainPhysiology = UnitPhysiology True Nothing
    (HM.fromList
        [ (s, if s `elem` ["hydration", "cardio", "consciousness", "mind"]
                 then PhysioSimulated else PhysioNominal)
        | s ← ["thermo", "salt", "hydration", "nutrition", "stamina"
             , "exhaustion", "sleep", "cardio", "blood", "consciousness"
             , "mind"] ])
    HM.empty

-- | The stored keys 'hydrationBrainPhysiology' needs besides the pool: the
--   cardio integrators at their healthy reading and the seeded mind.
hydrationBrainStats ∷ [(Text, Float)]
hydrationBrainStats = [("heart_rate", 70), ("blood_oxygen", 1)] ⧺ mindSeedStats

-- | The blood oxygen that drives a unit's derived consciousness to @c@ when
--   the temperature and salt factors are 1 ((oxygen − 0.5) / 0.3): how a
--   fixture reaches the delirious, confused or unconscious band now that
--   consciousness cannot be stored.
oxygenForConsciousness ∷ Float → Float
oxygenForConsciousness c = 0.5 + 0.3 * c

-- | The @rich@ profile as a pure value (every system Simulated), for a
--   fixture definition that is not built in IO. A spec in the physiology
--   group pins it equal to the shipped profile's expansion.
richPhysiology ∷ UnitPhysiology
richPhysiology = UnitPhysiology True (Just "rich")
    (HM.fromList
        [ (s, PhysioSimulated)
        | s ← ["thermo", "salt", "hydration", "nutrition", "stamina"
             , "exhaustion", "sleep", "cardio", "blood", "consciousness"
             , "mind"] ])
    HM.empty

-- | The @exempt@ profile as a pure value: every system Nominal except
--   cardio and blood, which #2754 kept live (S9 #2836, S10 #2837), and
--   consciousness (S11a #2838); its mind is Nominal (SD-23, S11b #2840).
exemptPhysiology ∷ UnitPhysiology
exemptPhysiology = UnitPhysiology True (Just "exempt")
    (HM.fromList
        [ (s, if s `elem` ["cardio", "blood", "consciousness"]
                 then PhysioSimulated else PhysioNominal)
        | s ← ["thermo", "salt", "hydration", "nutrition", "stamina"
             , "exhaustion", "sleep", "cardio", "blood", "consciousness"
             , "mind"] ])
    HM.empty

-- | Nutrition and stamina Simulated, the other three Nominal: for a fixture
--   that exercises nutrition's effects (the fat-floor organ failure, which
--   drains the STAMINA pool and so needs it Simulated, #2834) on a unit that
--   carries no salt, hydration or temperature state, so no other system's
--   Simulated invariants are in play.
nutritionOnlyPhysiology ∷ UnitPhysiology
nutritionOnlyPhysiology = UnitPhysiology True Nothing
    (HM.fromList
        [ ("thermo", PhysioNominal), ("salt", PhysioNominal)
        , ("hydration", PhysioNominal), ("nutrition", PhysioSimulated)
        , ("stamina", PhysioSimulated), ("exhaustion", PhysioNominal)
        , ("sleep", PhysioNominal), ("cardio", PhysioNominal)
        -- blood keeps its wound-fold behavior in these isolation fixtures
        , ("blood", PhysioSimulated) ])
    HM.empty

-- | Only stamina Simulated, every other system Nominal: for a fixture that
--   drives the stamina pool (#2834) and nothing else.
staminaOnlyPhysiology ∷ UnitPhysiology
staminaOnlyPhysiology = UnitPhysiology True Nothing
    (HM.fromList
        [ ("thermo", PhysioNominal), ("salt", PhysioNominal)
        , ("hydration", PhysioNominal), ("nutrition", PhysioNominal)
        , ("stamina", PhysioSimulated), ("exhaustion", PhysioNominal)
        , ("sleep", PhysioNominal), ("cardio", PhysioNominal)
        -- blood keeps its wound-fold behavior in these isolation fixtures
        , ("blood", PhysioSimulated) ])
    HM.empty

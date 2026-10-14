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
    case resolveUnitPhysiology reg "fixture" True (Just profile) [] of
        Left r   → error ("profilePhysiology " ⧺ show profile ⧺ ": " ⧺ show r)
        Right up → pure up

-- | A bodied fixture definition: the given one, carrying the profile.
withProfile ∷ Text → UnitDef → IO UnitDef
withProfile profile def = do
    up ← profilePhysiology profile
    pure def { udPhysiology = up }

-- | The @rich@ profile as a pure value (every system Simulated), for a
--   fixture definition that is not built in IO. A spec in the physiology
--   group pins it equal to the shipped profile's expansion.
richPhysiology ∷ UnitPhysiology
richPhysiology = UnitPhysiology True (Just "rich") $ HM.fromList
    [ (s, PhysioSimulated) | s ← ["thermo", "salt", "hydration", "nutrition"] ]

-- | The @exempt@ profile as a pure value (every system Nominal).
exemptPhysiology ∷ UnitPhysiology
exemptPhysiology = UnitPhysiology True (Just "exempt") $ HM.fromList
    [ (s, PhysioNominal) | s ← ["thermo", "salt", "hydration", "nutrition"] ]

-- | Only nutrition Simulated, the other three Nominal: for a fixture that
--   exercises nutrition's effects (the fat-floor organ failure) on a unit
--   that carries no salt, hydration or temperature state, so no other
--   system's Simulated invariants are in play.
nutritionOnlyPhysiology ∷ UnitPhysiology
nutritionOnlyPhysiology = UnitPhysiology True Nothing $ HM.fromList
    [ ("thermo", PhysioNominal), ("salt", PhysioNominal)
    , ("hydration", PhysioNominal), ("nutrition", PhysioSimulated) ]

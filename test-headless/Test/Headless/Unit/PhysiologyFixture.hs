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
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef)
import System.FilePath ((</>))
import Engine.Asset.YamlPhysiology
    (admitPhysiologyDoc, loadPhysiologyYamlOutcome)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Log
    (LogBackend(..), LogConfig(..), defaultLogConfig, initLogger)
import Engine.Core.State (EngineEnv)
import Unit.Physiology.Types

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

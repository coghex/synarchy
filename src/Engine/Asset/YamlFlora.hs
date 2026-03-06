{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlFlora
    ( FloraYamlDef(..)
    , FloraYamlFile(..)
    , FloraYamlPhase(..)
    , FloraYamlCycleStage(..)
    , FloraYamlCycleOverride(..)
    , FloraYamlWorldGen(..)
    , loadFloraYaml
    , parsePhaseTag
    , parseCycleTag
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject, withText)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))
import World.Flora.Types (LifePhaseTag(..), AnnualStageTag(..))

-----------------------------------------------------------
-- YAML Sub-Structures
-----------------------------------------------------------

data FloraYamlPhase = FloraYamlPhase
    { fypTag     ∷ Text
    , fypTexture ∷ Text    -- relative to texDir
    , fypAge     ∷ Float
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlPhase where
    parseJSON = withObject "FloraYamlPhase" $ \v → FloraYamlPhase
        ⊚ v .: "tag"
        ⊛ v .: "texture"
        ⊛ v .: "age"

data FloraYamlCycleStage = FloraYamlCycleStage
    { fycsTag      ∷ Text
    , fycsStartDay ∷ Int
    , fycsTexture  ∷ Text
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlCycleStage where
    parseJSON = withObject "FloraYamlCycleStage" $ \v → FloraYamlCycleStage
        ⊚ v .: "tag"
        ⊛ v .: "startDay"
        ⊛ v .: "texture"

data FloraYamlCycleOverride = FloraYamlCycleOverride
    { fycoPhase   ∷ Text
    , fycoCycle   ∷ Text
    , fycoTexture ∷ Text
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlCycleOverride where
    parseJSON = withObject "FloraYamlCycleOverride" $ \v → FloraYamlCycleOverride
        ⊚ v .: "phase"
        ⊛ v .: "cycle"
        ⊛ v .: "texture"

data FloraYamlWorldGen = FloraYamlWorldGen
    { fywCategory     ∷ Text
    , fywMinTemp      ∷ Float
    , fywMaxTemp      ∷ Float
    , fywIdealTemp    ∷ Float
    , fywMinPrecip    ∷ Float
    , fywMaxPrecip    ∷ Float
    , fywIdealPrecip  ∷ Float
    , fywMinAlt       ∷ Maybe Int
    , fywMaxAlt       ∷ Maybe Int
    , fywIdealAlt     ∷ Maybe Int
    , fywMinHumidity  ∷ Maybe Float
    , fywMaxHumidity  ∷ Maybe Float
    , fywIdealHumidity ∷ Maybe Float
    , fywMaxSlope     ∷ Maybe Int
    , fywDensity      ∷ Maybe Float
    , fywFootprint    ∷ Maybe Float
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlWorldGen where
    parseJSON = withObject "FloraYamlWorldGen" $ \v → FloraYamlWorldGen
        ⊚ v .:  "category"
        ⊛ v .:  "minTemp"
        ⊛ v .:  "maxTemp"
        ⊛ v .:  "idealTemp"
        ⊛ v .:  "minPrecip"
        ⊛ v .:  "maxPrecip"
        ⊛ v .:  "idealPrecip"
        ⊛ v .:? "minAlt"
        ⊛ v .:? "maxAlt"
        ⊛ v .:? "idealAlt"
        ⊛ v .:? "minHumidity"
        ⊛ v .:? "maxHumidity"
        ⊛ v .:? "idealHumidity"
        ⊛ v .:? "maxSlope"
        ⊛ v .:? "density"
        ⊛ v .:? "footprint"

-----------------------------------------------------------
-- Top-Level Species Definition
-----------------------------------------------------------

data FloraYamlDef = FloraYamlDef
    { fydName           ∷ Text
    , fydType           ∷ Text
    , fydTexDir         ∷ Text
    , fydLifecycle      ∷ Text            -- "evergreen", "perennial", "annual", "biennial"
    , fydMinLife        ∷ Maybe Float
    , fydMaxLife        ∷ Maybe Float
    , fydDeathChance    ∷ Maybe Float
    , fydPhases         ∷ [FloraYamlPhase]
    , fydAnnualCycle    ∷ [FloraYamlCycleStage]
    , fydCycleOverrides ∷ [FloraYamlCycleOverride]
    , fydWorldGen       ∷ FloraYamlWorldGen
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlDef where
    parseJSON = withObject "FloraYamlDef" $ \v → FloraYamlDef
        ⊚ v .:  "name"
        ⊛ v .:  "type"
        ⊛ v .:  "texDir"
        ⊛ v .:  "lifecycle"    .!= "evergreen"
        ⊛ v .:? "minLife"
        ⊛ v .:? "maxLife"
        ⊛ v .:? "deathChance"
        ⊛ v .:? "phases"       .!= []
        ⊛ v .:? "annualCycle"  .!= []
        ⊛ v .:? "cycleOverrides" .!= []
        ⊛ v .:  "worldGen"

data FloraYamlFile = FloraYamlFile
    { fyfFlora ∷ [FloraYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlFile where
    parseJSON = withObject "FloraYamlFile" $ \v → FloraYamlFile
        ⊚ v .: "flora"

-----------------------------------------------------------
-- YAML Parsing
-----------------------------------------------------------

loadFloraYaml ∷ LoggerState → FilePath → IO [FloraYamlDef]
loadFloraYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse flora YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right ff → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (fyfFlora ff)))
                <> " flora species from " <> T.pack path
            return (fyfFlora ff)

-----------------------------------------------------------
-- Tag Parsers (moved from Flora.hs Lua API — now shared)
-----------------------------------------------------------

parsePhaseTag ∷ Text → Maybe LifePhaseTag
parsePhaseTag "sprout"     = Just PhaseSprout
parsePhaseTag "seedling"   = Just PhaseSeedling
parsePhaseTag "vegetating" = Just PhaseVegetating
parsePhaseTag "budding"    = Just PhaseBudding
parsePhaseTag "flowering"  = Just PhaseFlowering
parsePhaseTag "ripening"   = Just PhaseRipening
parsePhaseTag "matured"    = Just PhaseMatured
parsePhaseTag "withering"  = Just PhaseWithering
parsePhaseTag "dead"       = Just PhaseDead
parsePhaseTag _            = Nothing

parseCycleTag ∷ Text → Maybe AnnualStageTag
parseCycleTag "dormant"   = Just CycleDormant
parseCycleTag "budding"   = Just CycleBudding
parseCycleTag "flowering" = Just CycleFlowering
parseCycleTag "fruiting"  = Just CycleFruiting
parseCycleTag "senescing" = Just CycleSenescing
parseCycleTag _           = Nothing

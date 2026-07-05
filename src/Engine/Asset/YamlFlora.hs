{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlFlora
    ( FloraYamlDef(..)
    , FloraYamlFile(..)
    , FloraYamlPhase(..)
    , FloraYamlCycleStage(..)
    , FloraYamlCycleOverride(..)
    , FloraYamlHarvest(..)
    , FloraYamlYield(..)
    , FloraYamlWorldGen(..)
    , loadFloraYaml
    , parsePhaseTag
    , parseCycleTag
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))
import World.Flora.Types (LifePhaseTag(..), AnnualStageTag(..))

-- * YAML sub-structures

data FloraYamlPhase = FloraYamlPhase
    { fypTag     ∷ Text
    , fypTexture ∷ Text    -- ^ Relative to the species @texDir@
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

-- | One yield entry of a harvestable plant: item id + count range.
--   @count@ reads as a two-element list @[min, max]@; a bare int also
--   works (@count: 2@ = exactly two). Absent = exactly one.
data FloraYamlYield = FloraYamlYield
    { fyyId  ∷ Text
    , fyyMin ∷ Int
    , fyyMax ∷ Int
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlYield where
    parseJSON = withObject "FloraYamlYield" $ \v → do
        iid ← v .: "id"
        mCnt ← v .:? "count"
        (lo, hi) ← case mCnt of
            Nothing  → pure (1, 1)
            Just val →
                (do xs ← parseJSON val
                    case xs of
                        [lo, hi] → pure (lo, hi)
                        _ → fail "yield count list must be [min, max]")
                <|> ((\n → (n, n)) ⊚ parseJSON val)
        pure (FloraYamlYield iid lo hi)

-- | Optional @harvestable:@ block (#94). Plants without it are
--   decorative only. @regrowth_time@ is in GAME seconds (86400 = one
--   game-day ≈ 24 real-minutes at timeScale 1).
data FloraYamlHarvest = FloraYamlHarvest
    { fyhTags             ∷ [Text]
    , fyhYield            ∷ [FloraYamlYield]
    , fyhRegrowthTime     ∷ Float
    , fyhHarvestedTexture ∷ Maybe Text   -- ^ Relative to @texDir@; absent
                                         --   = plant hidden while regrowing
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlHarvest where
    parseJSON = withObject "FloraYamlHarvest" $ \v → FloraYamlHarvest
        ⊚ v .:? "tags" .!= []
        ⊛ v .:? "yield" .!= []
        ⊛ v .:  "regrowth_time"
        ⊛ v .:? "harvested_texture"

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
    , fywSoils        ∷ [Text]
      -- ^ Preferred soil material NAMES (data/materials/*.yaml's
      --   @name@ field, e.g. "loam"), resolved to raw material ids at
      --   registration time (World.Material.materialIdByName) — kept
      --   as Text here since this is a pure Aeson parse, no registry
      --   access. Empty = no soil gating (speciesFitness's existing
      --   convention: @null soils@ passes unconditionally).
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
        ⊛ v .:? "soils" .!= []

-- * Top-level species definition

data FloraYamlDef = FloraYamlDef
    { fydName           ∷ Text
    , fydType           ∷ Text
    , fydTexDir         ∷ Text
    , fydLifecycle      ∷ Text            -- ^ @"evergreen"@, @"perennial"@, @"annual"@, or @"biennial"@
    , fydMinLife        ∷ Maybe Float
    , fydMaxLife        ∷ Maybe Float
    , fydDeathChance    ∷ Maybe Float
    , fydPhases         ∷ [FloraYamlPhase]
    , fydAnnualCycle    ∷ [FloraYamlCycleStage]
    , fydCycleOverrides ∷ [FloraYamlCycleOverride]
    , fydHarvest        ∷ Maybe FloraYamlHarvest
    , fydWorldGen       ∷ FloraYamlWorldGen
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlDef where
    parseJSON = withObject "FloraYamlDef" $ \v → FloraYamlDef
        ⊚ v .:  "name"
        ⊛ v .:  "type"
        ⊛ v .:  "texDir"
        ⊛ v .:? "lifecycle"    .!= "evergreen"
        ⊛ v .:? "minLife"
        ⊛ v .:? "maxLife"
        ⊛ v .:? "deathChance"
        ⊛ v .:? "phases"       .!= []
        ⊛ v .:? "annualCycle"  .!= []
        ⊛ v .:? "cycleOverrides" .!= []
        ⊛ v .:? "harvestable"
        ⊛ v .:  "worldGen"

data FloraYamlFile = FloraYamlFile
    { fyfFlora ∷ [FloraYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlFile where
    parseJSON = withObject "FloraYamlFile" $ \v → FloraYamlFile
        ⊚ v .: "flora"

-- * YAML parsing

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

-- * Tag parsers

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

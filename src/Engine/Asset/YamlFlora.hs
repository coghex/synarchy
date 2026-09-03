{-# LANGUAGE Strict, DeriveGeneric #-}
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
    , loadFloraYamlOutcome
    , parsePhaseTag
    , parseCycleTag
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlListOutcome)
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
--   game-day ≈ 24 real-minutes at timeScale 1) and must be a finite,
--   strictly positive number — see 'requireRegrowthTime' (#1711).
data FloraYamlHarvest = FloraYamlHarvest
    { fyhTags             ∷ [Text]
    , fyhYield            ∷ [FloraYamlYield]
    , fyhRegrowthTime     ∷ Float
    , fyhHarvestedTexture ∷ Maybe Text   -- ^ Relative to @texDir@; absent
                                         --   = plant hidden while regrowing
    } deriving (Show, Eq, Generic)

-- | Read a @harvestable:@ block’s REQUIRED @regrowth_time@ as a
--   finite, strictly positive number of GAME seconds, diagnosing every
--   rejection BY SPECIES NAME (#1711).
--
--   The domain check has to live HERE, at the authoring boundary, and
--   not at any action site. @regrowth_time@ is the only thing standing
--   between a harvested wild plant and being harvestable again:
--   'Engine.Scripting.Lua.API.Forage.Harvest' gates a harvest on the
--   live timer being @≤ 0@ and then reinserts this value unchanged, so a
--   non-positive one is immediately “expired” and the very next call on
--   the same tile spawns the full yield again — an unbounded item source
--   needing no tick in between. The regrowth tick does not close it
--   either: 'World.Flora.Harvest.tickFloraHarvests' DROPS an entry that
--   is already @≤ 0@, and no entry is the harvestable state, so the tick
--   reopens the tile rather than retiring it. Zero cannot be repurposed
--   as a one-shot harvest, because wild flora has no persistent
--   per-instance “permanently harvested” record to carry that meaning.
--
--   Naming the SPECIES is the whole reason this is a named parser
--   rather than a @v .: "regrowth_time"@ plus a check, exactly as
--   'Engine.Asset.YamlItems.requirePositiveQuantity' is:
--   'Engine.Asset.YamlList.loadYamlList' supplies the failing FILE path
--   in its warning, but an ordinary Aeson field error only reaches for
--   a JSON path like @$.flora[2].harvestable.regrowth_time@ — an index
--   nobody can map back to a species without counting entries. The two
--   halves together name the file AND the species.
--
--   Taking the whole 'Aeson.Value' rather than decoding to 'Float'
--   first is deliberate for the same reason it is there: YAML’s
--   @.nan@/@.inf@ resolve to STRINGS (the yaml package’s scalar
--   resolver only recognizes ordinary numeric syntax), so decoding
--   first would surface those as a type error naming neither the
--   species nor what was actually wrong. The finiteness check still has
--   to run AFTER narrowing, because a perfectly ordinary @1.0e+100@ is
--   a valid 'Scientific' that becomes 'Infinity' in the engine’s
--   32-bit 'Float' field — and an infinite timer never expires, so it
--   would reach gameplay as a silently one-shot plant.
requireRegrowthTime ∷ Text → Aeson.Object → Aeson.Parser Float
requireRegrowthTime species v = do
    mval ← v .:? "regrowth_time"
    case mval of
        Nothing  → bad "is required and has no default"
        Just val → case val of
            Aeson.Number s →
                let f = realToFrac s ∷ Float
                in if isNaN f ∨ isInfinite f
                     then bad ("must be finite, got " <> tshow val)
                     else if f ≤ 0
                       then bad ("must be strictly positive, got " <> tshow f)
                       else pure f
            _ → bad ("must be a number of game seconds, got " <> tshow val)
  where
    bad why = fail ∘ T.unpack $
        "flora species '" <> species <> "': harvestable regrowth_time (key \
        \'regrowth_time', game seconds) " <> why

-- | Parse a @harvestable:@ block, threading the OWNING species’ name
--   through so a bad @regrowth_time@ is diagnosed by species rather
--   than by list index. There is deliberately no 'FromJSON' instance:
--   the name is not reachable from inside one, which is the whole point
--   (see 'requireRegrowthTime').
--
--   The object check is spelled out rather than delegated to
--   'withObject' for the same reason: @harvestable: 23@ would otherwise
--   fail with aeson’s own “expected Object, but encountered Number”,
--   which names neither the species nor the block.
parseFloraYamlHarvest ∷ Text → Aeson.Value → Aeson.Parser FloraYamlHarvest
parseFloraYamlHarvest species val = case val of
    Aeson.Object v → FloraYamlHarvest
        ⊚ v .:? "tags" .!= []
        ⊛ v .:? "yield" .!= []
        ⊛ requireRegrowthTime species v
        ⊛ v .:? "harvested_texture"
    _ → fail ∘ T.unpack $
        "flora species '" <> species <> "': harvestable must be a block \
        \authoring a finite, strictly positive regrowth_time (game \
        \seconds), got " <> tshow val

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
    parseJSON = withObject "FloraYamlDef" $ \v → do
        -- `name` is read FIRST, monadically, because the `harvestable:`
        -- block below is parsed by a named parser that carries it into
        -- every diagnostic — the applicative chain cannot pass an
        -- already-parsed field to a later one.
        name ← v .: "name"
        -- Looked up rather than read with `.:?` only so a present-but-
        -- null key keeps meaning exactly what it meant before (#1711 is
        -- about the block’s CONTENT, not its presence): aeson’s `.:?`
        -- reads `harvestable: null` as absent, and this reproduces that.
        harvest ← case KM.lookup "harvestable" v of
            Nothing         → pure Nothing
            Just Aeson.Null → pure Nothing
            Just hv         → Just <$> parseFloraYamlHarvest name hv
        FloraYamlDef name
            ⊚ v .:  "type"
            ⊛ v .:  "texDir"
            ⊛ v .:? "lifecycle"    .!= "evergreen"
            ⊛ v .:? "minLife"
            ⊛ v .:? "maxLife"
            ⊛ v .:? "deathChance"
            ⊛ v .:? "phases"       .!= []
            ⊛ v .:? "annualCycle"  .!= []
            ⊛ v .:? "cycleOverrides" .!= []
            ⊛ pure harvest
            ⊛ v .:  "worldGen"

data FloraYamlFile = FloraYamlFile
    { fyfFlora ∷ [FloraYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlFile where
    parseJSON = withObject "FloraYamlFile" $ \v → FloraYamlFile
        ⊚ v .: "flora"

-- * YAML parsing

-- | 'loadFloraYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadFloraYaml'.
loadFloraYamlOutcome ∷ LoggerState → FilePath → IO (Maybe [FloraYamlDef])
loadFloraYamlOutcome logger =
    loadYamlListOutcome logger "flora" "flora species" fyfFlora

loadFloraYaml ∷ LoggerState → FilePath → IO [FloraYamlDef]
loadFloraYaml logger path = fromMaybe [] ⊚ loadFloraYamlOutcome logger path

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

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
    , FloraLifecycle(..)
    , loadFloraYaml
    , loadFloraYamlOutcome
    , parsePhaseTag
    , parseCycleTag
    , parseLifecycleTag
    , lifecycleText
    , lifePhaseVocabulary
    , annualStageVocabulary
    , lifecycleVocabulary
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Vector as V
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlListOutcome)
import World.Flora.Types (LifePhaseTag(..), AnnualStageTag(..))
import World.Flora.Growth (lifePhaseText, annualStageText)

-- * Closed vocabularies (#2315)
--
--   This schema has THREE closed vocabularies — lifecycle, life phase
--   and annual stage — authored at FIVE distinct positions:
--   @lifecycle@, @phases[].tag@, @annualCycle[].tag@, and the
--   @cycleOverrides[].phase@ / @cycleOverrides[].cycle@ pair, which
--   reuse the phase and stage vocabularies rather than adding two more.
--   Every one of those positions used to decode as unrestricted 'Text'
--   and be resolved — or quietly dropped — at registration.
--
--   The whole point of checking them HERE is that a dropped token is
--   not a cosmetic loss. 'World.Flora.Growth.harvestOpen' gates the
--   seasonal harvest window on the species declaring a @fruiting@
--   stage; a species whose @annualCycle@ misspells it has no fruiting
--   stage at all, falls into the documented “no fruiting stage → open
--   year-round” branch, and is silently harvestable in every season. A
--   misspelled @lifecycle@ is the same defect pointed the other way: an
--   annual becomes an evergreen.

-- | The closed @lifecycle:@ vocabulary, as a type rather than the raw
--   'Text' this field used to hold.
--
--   Holding the PARSED value is what makes 'registerFloraSpecies'’s old
--   @_ → Evergreen@ catch-all unreachable rather than merely unused:
--   with four constructors and no fifth, there is nothing left for an
--   unrecognized spelling to fall through to, and the authoring gate
--   below is the only policy in the codebase.
data FloraLifecycle
    = LifecycleEvergreen
    | LifecyclePerennial
    | LifecycleAnnual
    | LifecycleBiennial
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

parseLifecycleTag ∷ Text → Maybe FloraLifecycle
parseLifecycleTag "evergreen" = Just LifecycleEvergreen
parseLifecycleTag "perennial" = Just LifecyclePerennial
parseLifecycleTag "annual"    = Just LifecycleAnnual
parseLifecycleTag "biennial"  = Just LifecycleBiennial
parseLifecycleTag _           = Nothing

lifecycleText ∷ FloraLifecycle → Text
lifecycleText LifecycleEvergreen = "evergreen"
lifecycleText LifecyclePerennial = "perennial"
lifecycleText LifecycleAnnual    = "annual"
lifecycleText LifecycleBiennial  = "biennial"

-- | The three vocabularies as an author writes them, derived from the
--   types themselves so a diagnostic can never advertise a token the
--   matching parser would reject.
lifecycleVocabulary ∷ [Text]
lifecycleVocabulary = map lifecycleText [minBound .. maxBound]

lifePhaseVocabulary ∷ [Text]
lifePhaseVocabulary = map lifePhaseText [minBound .. maxBound]

annualStageVocabulary ∷ [Text]
annualStageVocabulary = map annualStageText [minBound .. maxBound]

-- | The offending scalar as its author would recognize it: a YAML
--   string as its bare quoted token and @null@ by that spelling rather
--   than aeson’s @String "…"@ / @Null@, with everything else falling
--   back to aeson’s own 'Show' (so @23@ still reads as @Number 23.0@,
--   exactly as 'requireRegrowthTime' reports it).
authoredToken ∷ Aeson.Value → Text
authoredToken (Aeson.String t) = "'" <> t <> "'"
authoredToken Aeson.Null       = "null"
authoredToken val              = tshow val

-- | One rejection message in the shape 'requireRegrowthTime'
--   established, carrying everything an author needs to find the fix
--   without reading this module: the SPECIES, the authored PATH, and
--   the authored KEY. The FILE is supplied by
--   'Engine.Asset.YamlList.loadYamlList', which owns the warning.
--
--   The path is spelled out separately from the key because the key
--   alone is ambiguous: @tag@ names two different vocabularies and
--   @phase@ appears both as an override selector and as the thing a
--   @phases[]@ entry declares. @annualCycle[].tag@ tells an author
--   which list to open; @tag@ does not.
vocabularyFailure ∷ Text → Text → Text → Text → Aeson.Parser α
vocabularyFailure species path key why = fail ∘ T.unpack $
    "flora species '" <> species <> "': " <> path <> " (key '" <> key
    <> "') " <> why

-- | Read a REQUIRED closed-vocabulary token, rejecting anything the
--   vocabulary does not contain. The rejection names the vocabulary, so
--   a typo’s fix is in the message.
requireVocabularyToken ∷ Text → Text → Text → (Text → Maybe α) → [Text]
                       → Aeson.Object → Aeson.Parser α
requireVocabularyToken species path key parse vocabulary v =
    case KM.lookup (Key.fromText key) v of
        Nothing  → bad "is required and has no default"
        Just val → case val of
            Aeson.String t → case parse t of
                Just parsed → pure parsed
                Nothing     → bad (unrecognized val)
            _ → bad (unrecognized val)
  where
    bad = vocabularyFailure species path key
    unrecognized val = "must be one of " <> T.intercalate ", " vocabulary
                       <> ", got " <> authoredToken val

-- | Require an already-parsed token to be one this species actually
--   DECLARES, naming the declared set it failed against (#2315
--   requirement 3). The empty declared set rejects everything, which is
--   correct: a species declaring no phases has no state an override
--   could ever select.
requireDeclared ∷ Eq α ⇒ Text → Text → Text → Text → (α → Text) → [α] → α
                → Aeson.Parser ()
requireDeclared species path key declaredIn render declared tag
    | tag `elem` declared = pure ()
    | otherwise = vocabularyFailure species path key $
        "names '" <> render tag <> "', which this species does not \
        \declare in its " <> declaredIn <> " — an override can only \
        \select a state this species can actually be in. Declared "
        <> declaredIn <> ": " <> declaredList
  where
    declaredList | null declared = "(none)"
                 | otherwise     = T.intercalate ", " (map render declared)

-- | Read an OPTIONAL list field, parsing each entry with a
--   species-aware parser. Absent and explicitly null both read as the
--   empty list, exactly as the @.:? … .!= []@ this replaces did.
parseFloraList ∷ Text → Text → (Aeson.Value → Aeson.Parser α)
               → Aeson.Object → Aeson.Parser [α]
parseFloraList species key item v = case KM.lookup (Key.fromText key) v of
    Nothing               → pure []
    Just Aeson.Null       → pure []
    Just (Aeson.Array xs) → traverse item (V.toList xs)
    Just val              → fail ∘ T.unpack $
        "flora species '" <> species <> "': " <> key
        <> " must be a list, got " <> authoredToken val

-- * YAML sub-structures

-- | One authored life phase. The tag is the PARSED 'LifePhaseTag'
--   rather than the raw token, because #2315 rejects an unrecognized
--   one here at the authoring boundary; holding the parsed value is
--   what leaves 'registerFloraSpecies' with no unrecognized case to
--   silently drop.
data FloraYamlPhase = FloraYamlPhase
    { fypTag     ∷ LifePhaseTag
    , fypTexture ∷ Text    -- ^ Relative to the species @texDir@
    , fypAge     ∷ Float
    } deriving (Show, Eq, Generic)

-- | Parse one @phases:@ entry, threading the OWNING species’ name
--   through so a bad @tag@ is diagnosed by species rather than by list
--   index. There is deliberately no 'FromJSON' instance, for exactly
--   the reason 'parseFloraYamlHarvest' has none: the name is not
--   reachable from inside one.
parseFloraYamlPhase ∷ Text → Aeson.Value → Aeson.Parser FloraYamlPhase
parseFloraYamlPhase species val = case val of
    Aeson.Object v → FloraYamlPhase
        ⊚ requireVocabularyToken species "phases[].tag" "tag"
              parsePhaseTag lifePhaseVocabulary v
        ⊛ v .: "texture"
        ⊛ v .: "age"
    _ → fail ∘ T.unpack $
        "flora species '" <> species <> "': every phases[] entry must be \
        \a block authoring tag, texture and age, got " <> authoredToken val

data FloraYamlCycleStage = FloraYamlCycleStage
    { fycsTag      ∷ AnnualStageTag
    , fycsStartDay ∷ Int
    , fycsTexture  ∷ Text
    } deriving (Show, Eq, Generic)

-- | Parse one @annualCycle:@ entry. Same shape, same reason, as
--   'parseFloraYamlPhase'.
parseFloraYamlCycleStage ∷ Text → Aeson.Value
                         → Aeson.Parser FloraYamlCycleStage
parseFloraYamlCycleStage species val = case val of
    Aeson.Object v → FloraYamlCycleStage
        ⊚ requireVocabularyToken species "annualCycle[].tag" "tag"
              parseCycleTag annualStageVocabulary v
        ⊛ v .: "startDay"
        ⊛ v .: "texture"
    _ → fail ∘ T.unpack $
        "flora species '" <> species <> "': every annualCycle[] entry \
        \must be a block authoring tag, startDay and texture, got "
        <> authoredToken val

data FloraYamlCycleOverride = FloraYamlCycleOverride
    { fycoPhase   ∷ LifePhaseTag
    , fycoCycle   ∷ AnnualStageTag
    , fycoTexture ∷ Text
    } deriving (Show, Eq, Generic)

-- | Parse one @cycleOverrides:@ entry against the species’ OWN
--   declared phase and annual-cycle sets (#2315 requirement 3).
--
--   Parsing is not enough here, and this is the one place in the schema
--   where that is true. An override is selected by
--   'World.Flora.Types.AnnualCycleKey' — the plant’s live phase paired
--   with its live annual stage — and both of those come from THIS
--   species’ @phases:@ and @annualCycle:@ lists. A perfectly
--   well-spelled @flowering@ override on a species that never declares
--   a @flowering@ phase therefore registers a texture no plant can ever
--   select: a silent authoring dead end, which is why it is rejected
--   rather than dropped.
parseFloraYamlCycleOverride ∷ Text → [LifePhaseTag] → [AnnualStageTag]
                            → Aeson.Value
                            → Aeson.Parser FloraYamlCycleOverride
parseFloraYamlCycleOverride species declaredPhases declaredStages val =
  case val of
    Aeson.Object v → do
        pTag ← requireVocabularyToken species "cycleOverrides[].phase" "phase"
                   parsePhaseTag lifePhaseVocabulary v
        requireDeclared species "cycleOverrides[].phase" "phase"
            "phases[]" lifePhaseText declaredPhases pTag
        cTag ← requireVocabularyToken species "cycleOverrides[].cycle" "cycle"
                   parseCycleTag annualStageVocabulary v
        requireDeclared species "cycleOverrides[].cycle" "cycle"
            "annualCycle[]" annualStageText declaredStages cTag
        FloraYamlCycleOverride pTag cTag ⊚ v .: "texture"
    _ → fail ∘ T.unpack $
        "flora species '" <> species <> "': every cycleOverrides[] entry \
        \must be a block authoring phase, cycle and texture, got "
        <> authoredToken val

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
    , fydLifecycle      ∷ FloraLifecycle  -- ^ Absent = 'LifecycleEvergreen'
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
        -- The five vocabulary positions (#2315) are read monadically
        -- for the same reason `name` is, and then for one more: the
        -- overrides are validated against THIS species' own declared
        -- phase and annual-cycle sets, which only exist once those two
        -- lists have been parsed. An applicative chain cannot hand one
        -- field to a later one.
        lifecycle ← requireLifecycle name v
        phases ← parseFloraList name "phases" (parseFloraYamlPhase name) v
        cycleStages ← parseFloraList name "annualCycle"
                          (parseFloraYamlCycleStage name) v
        overrides ← parseFloraList name "cycleOverrides"
                        (parseFloraYamlCycleOverride name
                            (map fypTag phases) (map fycsTag cycleStages)) v
        FloraYamlDef name
            ⊚ v .:  "type"
            ⊛ v .:  "texDir"
            ⊛ pure lifecycle
            ⊛ v .:? "minLife"
            ⊛ v .:? "maxLife"
            ⊛ v .:? "deathChance"
            ⊛ pure phases
            ⊛ pure cycleStages
            ⊛ pure overrides
            ⊛ pure harvest
            ⊛ v .:  "worldGen"

-- | Read the optional @lifecycle:@ key (#2315 requirement 2).
--
--   An ABSENT key keeps the documented default; a PRESENT one must name
--   a lifecycle. The two are told apart by an explicit lookup rather
--   than by @.:?@ on purpose, and this is the deliberate opposite of
--   what @harvestable:@ one field over does: aeson reads
--   @lifecycle: null@ as absent, and silently defaulting an authored
--   null to evergreen is precisely the present-but-malformed
--   substitution #1191 rules out.
requireLifecycle ∷ Text → Aeson.Object → Aeson.Parser FloraLifecycle
requireLifecycle species v = case KM.lookup "lifecycle" v of
    Nothing → pure LifecycleEvergreen
    Just _  → requireVocabularyToken species "lifecycle" "lifecycle"
                  parseLifecycleTag lifecycleVocabulary v

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

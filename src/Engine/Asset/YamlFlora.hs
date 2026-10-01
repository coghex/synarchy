{-# LANGUAGE Strict, DeriveGeneric #-}
-- | The flora definition schema and its authoring-boundary checks.
--
--   The selector vocabularies — context, life phase, annual stage,
--   condition and cause — and the @textureVariants@ / @corpsePolicy@
--   schemas follow @docs/flora_visual_state_contract.md@ (#2530), which
--   is their authority; this module only decodes and validates them.
module Engine.Asset.YamlFlora
    ( FloraYamlDef(..)
    , FloraYamlTextureVariant(..)
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
    , contextText
    , conditionText
    , deathCauseText
    , successorText
    , contextVocabulary
    , conditionVocabulary
    , deathCauseVocabulary
    , successorVocabulary
    , variantSelectorText
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser, parseMaybe)
import qualified Data.Vector as V
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlListOutcome)
import World.Flora.Types
    ( LifePhaseTag(..), AnnualStageTag(..), FloraContext(..)
    , FloraCondition(..), FloraDeathCause(..), FloraVariantSelector(..)
    , CorpseSuccessor(..), CorpseOutcome(..), CorpseOverrideSelector(..)
    , CorpsePolicyProvenance(..), FloraCorpsePolicy(..)
    , legacyCorpsePolicy )
import World.Flora.Growth (lifePhaseText, annualStageText)

-- * Closed vocabularies (#2315)
--
--   This schema has THREE closed vocabularies — lifecycle, life phase
--   and annual stage — authored at SIX distinct positions:
--   @lifecycle@, @phases[].tag@, @annualCycle[].tag@, the
--   @cycleOverrides[].phase@ / @cycleOverrides[].cycle@ pair, which
--   reuse the phase and stage vocabularies rather than adding two more,
--   and (since #2212) the KEYS of @harvestable.phase_yield@, which
--   reuse the phase vocabulary a third time. Every one of those
--   positions used to decode as unrestricted 'Text' and be resolved —
--   or quietly dropped — at registration.
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
requireDeclared = requireDeclaredBy "an override"

-- | 'requireDeclared' naming what kind of declaration did the
--   selecting, so a @textureVariants@ refusal does not call itself an
--   override.
requireDeclaredBy ∷ Eq α ⇒ Text → Text → Text → Text → Text → (α → Text)
                  → [α] → α → Aeson.Parser ()
requireDeclaredBy what species path key declaredIn render declared tag
    | tag `elem` declared = pure ()
    | otherwise = vocabularyFailure species path key $
        "names '" <> render tag <> "', which this species does not \
        \declare in its " <> declaredIn <> " — " <> what <> " can only \
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
    , fyhUngatedTags      ∷ [Text]
      -- ^ @ungated_tags:@ (#2212) — the subset of @tags:@ whose harvest
      --   may take this plant outside the #332 growth window. Absent =
      --   the empty list = growth-gated, so a tagged call is refused in
      --   exactly the states a bare one is. Every entry must appear in
      --   @tags:@ ('requireUngatedTags').
    , fyhYield            ∷ [FloraYamlYield]
    , fyhPhaseYield       ∷ HM.HashMap LifePhaseTag [FloraYamlYield]
      -- ^ @phase_yield:@ (#2212) — per-life-phase overrides of
      --   @yield:@. An ABSENT phase inherits @yield:@; a phase mapped
      --   to the empty list yields nothing. The two are deliberately
      --   distinguishable here, which is the only reason a felled
      --   sprout can be authored to drop no logs.
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
parseFloraYamlHarvest ∷ Text → [LifePhaseTag] → Aeson.Value
                      → Aeson.Parser FloraYamlHarvest
parseFloraYamlHarvest species declaredPhases val = case val of
    Aeson.Object v → do
        tags ← v .:? "tags" .!= []
        ungated ← requireUngatedTags species tags v
        phaseYield ← requirePhaseYield species declaredPhases v
        FloraYamlHarvest tags ungated
            ⊚ v .:? "yield" .!= []
            ⊛ pure phaseYield
            ⊛ requireRegrowthTime species v
            ⊛ v .:? "harvested_texture"
    _ → fail ∘ T.unpack $
        "flora species '" <> species <> "': harvestable must be a block \
        \authoring a finite, strictly positive regrowth_time (game \
        \seconds), got " <> tshow val

-- | Read the optional @ungated_tags:@ list (#2212), requiring every
--   entry to be one this block’s own @tags:@ declares.
--
--   The declared-set check is the same rule 'requireDeclared' applies
--   to a @cycleOverrides@ selector, and it is here for the same reason:
--   an exemption for a tag the species does not carry can never be
--   selected, because 'World.Flora.Growth.floraHarvestAdmits' checks
--   tag membership first. Dropping it silently would leave a
--   misspelled @wodo@ looking authored while the chop stayed
--   growth-gated — the exact silent re-gating this schema exists to
--   make impossible.
--
--   Absent and @null@ both read as the empty list, matching @tags:@ one
--   key over: for THIS field the two mean the same thing (no tag is
--   exempt), so there is nothing for #1191’s present-but-malformed
--   rule to protect.
requireUngatedTags ∷ Text → [Text] → Aeson.Object → Aeson.Parser [Text]
requireUngatedTags species tags v = do
    ungated ← v .:? "ungated_tags" .!= []
    forM_ ungated $ \t → when (t `notElem` tags) $
        vocabularyFailure species "harvestable.ungated_tags" "ungated_tags" $
            "names '" <> t <> "', which this species does not declare in \
            \its harvestable tags — an ungated tag can only exempt a tag \
            \this species can actually be harvested by. Declared tags: "
            <> (if null tags then "(none)" else T.intercalate ", " tags)
    pure ungated

-- | Read the optional @phase_yield:@ block (#2212): a mapping from
--   life-phase name to that phase’s own yield list.
--
--   Three rejections, each closing a way a misspelling would silently
--   restore the inherited roll rather than override it:
--
--     * a @phase_yield:@ that is not a block (@null@ included — unlike
--       @ungated_tags@, absent and empty-block are NOT the same
--       statement here, so an authored null has no defensible reading);
--     * a key outside the 'LifePhaseTag' vocabulary; and
--     * a well-spelled key naming a phase this species never declares,
--       which is unreachable for the same reason a @cycleOverrides@
--       selector on an undeclared phase is.
--
--   An entry’s VALUE is an ordinary yield list, so the empty list
--   is a legal authored statement and is exactly how a species declares
--   that a phase yields nothing.
requirePhaseYield ∷ Text → [LifePhaseTag] → Aeson.Object
                  → Aeson.Parser (HM.HashMap LifePhaseTag [FloraYamlYield])
requirePhaseYield species declaredPhases v =
    case KM.lookup "phase_yield" v of
        Nothing → pure HM.empty
        Just (Aeson.Object entries) →
            HM.fromList ⊚ traverse phaseEntry (KM.toList entries)
        Just other → fail ∘ T.unpack $
            "flora species '" <> species <> "': harvestable phase_yield \
            \(key 'phase_yield') must be a block mapping life-phase \
            \names to yield lists, got " <> authoredToken other
  where
    phaseEntry (key, entryVal) = do
        let token = Key.toText key
        tag ← case parsePhaseTag token of
            Just t  → pure t
            Nothing → vocabularyFailure species "harvestable.phase_yield[]"
                          token $
                          "must be one of "
                          <> T.intercalate ", " lifePhaseVocabulary
                          <> ", got " <> authoredToken (Aeson.String token)
        requireDeclared species "harvestable.phase_yield[]" token
            "phases[]" lifePhaseText declaredPhases tag
        yields ← case entryVal of
            Aeson.Array xs → traverse parseJSON (V.toList xs)
            _ → fail ∘ T.unpack $
                "flora species '" <> species <> "': harvestable \
                \phase_yield (key '" <> token <> "') must be a list of \
                \yield entries, got " <> authoredToken entryVal
        pure (tag, yields)

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

-- * Visual-state selectors and corpse policy (#2539)
--
--   Both blocks follow @docs/flora_visual_state_contract.md@ (#2530):
--   the vocabularies below are its §1 axes and §7.1 fields, and every
--   rule enforced here is one it states. Nothing here is consumed yet —
--   the resolver (EFM-3) and retention (EFM-10) read what this loads.

contextText ∷ FloraContext → Text
contextText ContextWild       = "wild"
contextText ContextCultivated = "cultivated"

conditionText ∷ FloraCondition → Text
conditionText ConditionAlive = "alive"
conditionText ConditionDead  = "dead"

deathCauseText ∷ FloraDeathCause → Text
deathCauseText CauseNatural = "natural"
deathCauseText CauseDrought = "drought"
deathCauseText CauseFrost   = "frost"
deathCauseText CauseFire    = "fire"
deathCauseText CauseDisease = "disease"
deathCauseText CauseDamage  = "damage"
deathCauseText CauseUnknown = "unknown"

successorText ∷ CorpseSuccessor → Text
successorText SuccessorReseed = "reseed"
successorText SuccessorAbsent = "absent"

contextVocabulary ∷ [Text]
contextVocabulary = map contextText [minBound .. maxBound]

conditionVocabulary ∷ [Text]
conditionVocabulary = map conditionText [minBound .. maxBound]

deathCauseVocabulary ∷ [Text]
deathCauseVocabulary = map deathCauseText [minBound .. maxBound]

-- | @await_replanting@ is deliberately absent: it names the CULTIVATED
--   outcome, which is a rule of the render context, and a species
--   declaring it is refused like any other unknown token (contract
--   §7.2).
successorVocabulary ∷ [Text]
successorVocabulary = map successorText [minBound .. maxBound]

-- | Parse a token by rendering every constructor and comparing, so the
--   parser is the exact inverse of its renderer by construction.
parseByText ∷ (Enum α, Bounded α) ⇒ (α → Text) → Text → Maybe α
parseByText render t =
    lookup t [ (render x, x) | x ← [minBound .. maxBound] ]

data CorpseVisibility = VisibilityTransient | VisibilityPersistent
    deriving (Eq, Enum, Bounded)

visibilityText ∷ CorpseVisibility → Text
visibilityText VisibilityTransient  = "transient"
visibilityText VisibilityPersistent = "persistent"

-- | A selector as one readable token: the axes it NAMES, in contract
--   order, as @axis=value@ joined by commas, or @*@ when it names none.
--   It keys a variant's texture-registry name and matches the
--   @variant:<selector>@ label @tools/texture_subset_audit.py@ prints.
variantSelectorText ∷ FloraVariantSelector → Text
variantSelectorText (FloraVariantSelector c p st cd ca) =
    case named of
        [] → "*"
        xs → T.intercalate "," xs
  where
    named = catMaybes
        [ ("context=" <>)   ∘ contextText    ⊚ c
        , ("phase=" <>)     ∘ lifePhaseText  ⊚ p
        , ("stage=" <>)     ∘ annualStageText ⊚ st
        , ("condition=" <>) ∘ conditionText  ⊚ cd
        , ("cause=" <>)     ∘ deathCauseText ⊚ ca ]

-- | Read an OPTIONAL closed-vocabulary token. Absent is 'Nothing' — a
--   wildcard — but a PRESENT key must name a token, so an authored
--   @null@ is refused rather than read as absent (#1191).
optionalVocabularyToken ∷ Text → Text → Text → (Text → Maybe α) → [Text]
                        → Aeson.Object → Aeson.Parser (Maybe α)
optionalVocabularyToken species path key parse vocabulary v =
    case KM.lookup (Key.fromText key) v of
        Nothing → pure Nothing
        Just _  → Just ⊚ requireVocabularyToken species path key parse
                             vocabulary v

-- | Refuse any key outside @allowed@. In these blocks an omitted axis
--   is a WILDCARD, so a misspelled key would otherwise widen a selector
--   silently instead of failing.
rejectUnknownKeys ∷ Text → Text → [Text] → Aeson.Object → Aeson.Parser ()
rejectUnknownKeys species path allowed v =
    forM_ (map Key.toText (KM.keys v)) $ \k → when (k `notElem` allowed) $
        vocabularyFailure species path k $
            "is not a field of this block. Fields: "
            <> T.intercalate ", " allowed

-- | Refuse @phase: dead@ at a selector position. @dead@ is a legacy
--   @phases[]@ / @cycleOverrides[].phase@ token only; no selector ever
--   carries it, so a declaration naming it is unreachable (contract
--   §1.1, §2.1).
refuseDeadPhase ∷ Text → Text → LifePhaseTag → Aeson.Parser ()
refuseDeadPhase species path PhaseDead = vocabularyFailure species path
    "phase" "names 'dead', a legacy phases[] token that no selector \
    \carries — death is declared with condition: dead"
refuseDeadPhase _ _ _ = pure ()

-- | Read a selector's optional @phase@: in the vocabulary, not @dead@,
--   and declared by this species.
optionalSelectorPhase ∷ Text → Text → Text → [LifePhaseTag] → Aeson.Object
                      → Aeson.Parser (Maybe LifePhaseTag)
optionalSelectorPhase what species path declared v = do
    mp ← optionalVocabularyToken species path "phase" parsePhaseTag
             lifePhaseVocabulary v
    forM_ mp $ \p → do
        refuseDeadPhase species path p
        requireDeclaredBy what species path "phase" "phases[]" lifePhaseText
            declared p
    pure mp

-- | One authored @textureVariants@ entry: its semantic selector and its
--   path relative to the species' @texDir@.
data FloraYamlTextureVariant = FloraYamlTextureVariant
    { fytvSelector ∷ FloraVariantSelector
    , fytvTexture  ∷ Text
    } deriving (Show, Eq, Generic)

-- | Parse one @textureVariants[i]@ entry (contract §2). Every axis is
--   optional (a wildcard), present axes must be non-null tokens of
--   their vocabulary, a phase or stage must be one this species
--   declares, and a cause requires @condition: dead@ (§2 rule 4).
parseTextureVariant ∷ Text → [LifePhaseTag] → [AnnualStageTag]
                    → (Int, Aeson.Value)
                    → Aeson.Parser FloraYamlTextureVariant
parseTextureVariant species declaredPhases declaredStages (i, val) =
  case val of
    Aeson.Object v → do
        rejectUnknownKeys species path
            ["context", "phase", "stage", "condition", "cause", "texture"] v
        ctx ← optionalVocabularyToken species path "context"
                  (parseByText contextText) contextVocabulary v
        ph ← optionalSelectorPhase "a texture variant" species path
                 declaredPhases v
        st ← optionalVocabularyToken species path "stage" parseCycleTag
                 annualStageVocabulary v
        forM_ st $ requireDeclaredBy "a texture variant" species path "stage"
            "annualCycle[]" annualStageText declaredStages
        cd ← optionalVocabularyToken species path "condition"
                 (parseByText conditionText) conditionVocabulary v
        ca ← optionalVocabularyToken species path "cause"
                 (parseByText deathCauseText) deathCauseVocabulary v
        forM_ ca $ \c → when (cd ≢ Just ConditionDead) $
            vocabularyFailure species path "cause" $
                "names '" <> deathCauseText c <> "' without condition: \
                \dead — only a dead occurrence carries a cause, so \
                \nothing could ever select this declaration"
        tex ← case KM.lookup "texture" v of
            Nothing → vocabularyFailure species path "texture"
                "is required: a path relative to texDir"
            Just (Aeson.String t)
                | T.null (T.strip t) → vocabularyFailure species path
                    "texture" "must be a non-empty path relative to \
                    \texDir, got ''"
                | otherwise → pure t
            Just other → vocabularyFailure species path "texture" $
                "must be a path relative to texDir, got "
                <> authoredToken other
        pure (FloraYamlTextureVariant (FloraVariantSelector ctx ph st cd ca)
                                      tex)
    _ → fail ∘ T.unpack $
        "flora species '" <> species <> "': " <> path <> " must be a \
        \block authoring a texture and optional selector axes, got "
        <> authoredToken val
  where
    path = "textureVariants[" <> tshow i <> "]"

-- | Refuse two declarations of one selector (contract §2 rule 2),
--   including a variant restating a selector the LEGACY entries already
--   declare: a @phases[]@ tag @dead@ normalizes to @{condition: dead}@
--   and a @cycleOverrides[]@ entry on phase @dead@ to
--   @{condition: dead, stage: …}@ (§1.1).
requireDistinctVariants ∷ Text → [LifePhaseTag] → [FloraYamlCycleOverride]
                        → [FloraYamlTextureVariant] → Aeson.Parser ()
requireDistinctVariants species phases overrides variants =
    go [] (zip [0 ∷ Int ..] (map fytvSelector variants))
  where
    deadSel = FloraVariantSelector Nothing Nothing Nothing
                  (Just ConditionDead) Nothing
    legacy = [ (deadSel, "the legacy phases[] tag 'dead'")
             | PhaseDead `elem` phases ]
          ⧺ [ ( deadSel { fvsStage = Just (fycoCycle o) }
              , "the legacy cycleOverrides[] entry on phase 'dead', cycle '"
                <> annualStageText (fycoCycle o) <> "'" )
            | o ← overrides, fycoPhase o ≡ PhaseDead ]
    go _ [] = pure ()
    go seen ((i, sel) : rest) = do
        let path = "textureVariants[" <> tshow i <> "]"
            clash why = vocabularyFailure species path "selector" $
                "declares " <> variantSelectorText sel <> ", which "
                <> why <> " already declares — two declarations of one \
                \selector are an authoring error"
        case lookup sel seen of
            Just j  → clash ("textureVariants[" <> tshow j <> "]")
            Nothing → case lookup sel legacy of
                Just desc → clash desc
                Nothing   → go ((sel, i) : seen) rest

-- | Read the optional @corpsePolicy:@ block (contract §7).
--
--   ABSENT loads the legacy default and says so through
--   'CorpsePolicyDefaulted'. A PRESENT key must be a block: an authored
--   @corpsePolicy: null@ is refused like @lifecycle: null@ rather than
--   silently read as the default (#1191).
requireCorpsePolicy ∷ Text → [LifePhaseTag] → Aeson.Object
                    → Aeson.Parser FloraCorpsePolicy
requireCorpsePolicy species declaredPhases v =
    case KM.lookup "corpsePolicy" v of
        Nothing → pure legacyCorpsePolicy
        Just (Aeson.Object p) → do
            rejectUnknownKeys species "corpsePolicy"
                ["visibility", "durationDays", "successor", "overrides"] p
            outcome ← parseCorpseOutcome species "corpsePolicy" p
            overrides ← case KM.lookup "overrides" p of
                Nothing               → pure []
                Just (Aeson.Array xs) → traverse
                    (parseCorpseOverride species declaredPhases)
                    (zip [0 ..] (V.toList xs))
                Just other → vocabularyFailure species
                    "corpsePolicy.overrides" "overrides" $
                    "must be a list of override blocks, got "
                    <> authoredToken other
            requireDistinctOverrides overrides
            pure FloraCorpsePolicy
                { fcpOutcome    = outcome
                , fcpOverrides  = HM.fromList [ (s, o) | (_, s, o) ← overrides ]
                , fcpProvenance = CorpsePolicyAuthored
                }
        Just other → vocabularyFailure species "corpsePolicy" "corpsePolicy" $
            "must be a block declaring visibility (transient or \
            \persistent), got " <> authoredToken other
  where
    requireDistinctOverrides = go []
      where
        go _ [] = pure ()
        go seen ((i, sel, _) : rest) = case lookup sel seen of
            Just j → vocabularyFailure species
                ("corpsePolicy.overrides[" <> tshow i <> "]") "selector" $
                "repeats the phase/cause selector of \
                \corpsePolicy.overrides[" <> tshow (j ∷ Int) <> "] — two \
                \overrides of one selector are an authoring error"
            Nothing → go ((sel, i) : seen) rest

-- | One @corpsePolicy.overrides[i]@ entry: a @phase@ and/or @cause@
--   selector (at least one) and its own COMPLETE outcome (§7.3).
parseCorpseOverride ∷ Text → [LifePhaseTag] → (Int, Aeson.Value)
                    → Aeson.Parser (Int, CorpseOverrideSelector, CorpseOutcome)
parseCorpseOverride species declaredPhases (i, val) = case val of
    Aeson.Object o → do
        rejectUnknownKeys species path
            ["phase", "cause", "visibility", "durationDays", "successor"] o
        ph ← optionalSelectorPhase "a corpse-policy override" species path
                 declaredPhases o
        ca ← optionalVocabularyToken species path "cause"
                 (parseByText deathCauseText) deathCauseVocabulary o
        when (isNothing ph ∧ isNothing ca) $
            vocabularyFailure species path "phase" "selects neither a \
                \phase nor a cause — an override naming neither restates \
                \the species-level policy"
        outcome ← parseCorpseOutcome species path o
        pure (i, CorpseOverrideSelector ph ca, outcome)
    _ → fail ∘ T.unpack $
        "flora species '" <> species <> "': " <> path <> " must be a block \
        \authoring a phase and/or cause and its own outcome, got "
        <> authoredToken val
  where
    path = "corpsePolicy.overrides[" <> tshow i <> "]"

-- | One complete outcome (§7.1): @visibility@, with @durationDays@ and
--   @successor@ required for @transient@ and refused for @persistent@.
parseCorpseOutcome ∷ Text → Text → Aeson.Object → Aeson.Parser CorpseOutcome
parseCorpseOutcome species path o = do
    vis ← requireVocabularyToken species path "visibility"
              (parseByText visibilityText)
              (map visibilityText [minBound .. maxBound]) o
    case vis of
        VisibilityPersistent → do
            forM_ ["durationDays", "successor"] $ \k →
                when (KM.member (Key.fromText k) o) $
                    vocabularyFailure species path k "is refused when \
                        \visibility is persistent — a persistent corpse \
                        \stays until it is cleared, so it has no window \
                        \and no successor"
            pure CorpsePersistent
        VisibilityTransient → CorpseTransient
            ⊚ requireDurationDays
            ⊛ requireVocabularyToken species path "successor"
                  (parseByText successorText) successorVocabulary o
  where
    -- A whole number of days of at least 1, checked on the authored
    -- 'Scientific' before any conversion: aeson's bounded 'Int' parser
    -- refuses a fractional value and one too large for 'Int', so
    -- nothing that would overflow or truncate reaches the loaded record.
    requireDurationDays = case KM.lookup "durationDays" o of
        Nothing → vocabularyFailure species path "durationDays"
            "is required when visibility is transient and has no default"
        Just val@(Aeson.Number _) → case Aeson.parseMaybe parseJSON val of
            Just (n ∷ Int) | n ≥ 1 → pure n
            _ → badDays val
        Just val → badDays val
    badDays val = vocabularyFailure species path "durationDays" $
        "must be a whole number of days of at least 1, got "
        <> authoredToken val

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
    , fydTextureVariants ∷ [FloraYamlTextureVariant]
      -- ^ Absent or null = none declared.
    , fydCorpsePolicy   ∷ FloraCorpsePolicy
      -- ^ Absent = 'legacyCorpsePolicy', marked defaulted.
    } deriving (Show, Eq, Generic)

instance FromJSON FloraYamlDef where
    parseJSON = withObject "FloraYamlDef" $ \v → do
        -- `name` is read FIRST, monadically, because the `harvestable:`
        -- block below is parsed by a named parser that carries it into
        -- every diagnostic — the applicative chain cannot pass an
        -- already-parsed field to a later one.
        name ← v .: "name"
        -- The five vocabulary positions (#2315) are read monadically
        -- for the same reason `name` is, and then for one more: the
        -- overrides are validated against THIS species' own declared
        -- phase and annual-cycle sets, which only exist once those two
        -- lists have been parsed. An applicative chain cannot hand one
        -- field to a later one.
        lifecycle ← requireLifecycle name v
        phases ← parseFloraList name "phases" (parseFloraYamlPhase name) v
        -- Read AFTER `phases` since #2212, for that same reason: a
        -- `phase_yield:` key is validated against the phases this
        -- species actually declares.
        --
        -- Looked up rather than read with `.:?` only so a present-but-
        -- null key keeps meaning exactly what it meant before (#1711 is
        -- about the block’s CONTENT, not its presence): aeson’s `.:?`
        -- reads `harvestable: null` as absent, and this reproduces that.
        harvest ← case KM.lookup "harvestable" v of
            Nothing         → pure Nothing
            Just Aeson.Null → pure Nothing
            Just hv         → Just <$> parseFloraYamlHarvest name
                                           (map fypTag phases) hv
        cycleStages ← parseFloraList name "annualCycle"
                          (parseFloraYamlCycleStage name) v
        overrides ← parseFloraList name "cycleOverrides"
                        (parseFloraYamlCycleOverride name
                            (map fypTag phases) (map fycsTag cycleStages)) v
        -- #2539: both selector blocks validate against the declared
        -- phases and stages, and the variants against the legacy
        -- entries they could restate, so they are read last.
        variants ← case KM.lookup "textureVariants" v of
            Nothing               → pure []
            Just Aeson.Null       → pure []
            Just (Aeson.Array xs) → traverse
                (parseTextureVariant name (map fypTag phases)
                    (map fycsTag cycleStages))
                (zip [0 ..] (V.toList xs))
            Just other → vocabularyFailure name "textureVariants"
                "textureVariants" $
                "must be a list of variant blocks, got " <> authoredToken other
        requireDistinctVariants name (map fypTag phases) overrides variants
        corpsePolicy ← requireCorpsePolicy name (map fypTag phases) v
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
            ⊛ pure variants
            ⊛ pure corpsePolicy

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

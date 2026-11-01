{-# LANGUAGE Strict #-}
-- | The pure half of the flora condition mutation seam (#2555, EFM-7).
--
--   The world thread's @WorldSetFloraCondition@ / @WorldClearFloraCondition@
--   commands ("World.Thread.Command.Flora.Condition") are the ONLY writer
--   of a persistent flora condition. Everything they decide that is not
--   IO lives here, so the rules are testable without a world:
--
--     * the cause vocabulary a request names ('parseDeathCause');
--     * the death record a set builds — the life phase and annual stage
--       FROZEN from the growth state on that world day (D-10), and the
--       retention outcome SNAPSHOTTED from the species' @corpsePolicy@
--       and the occurrence's context (D-5, D-13, D-19)
--       ('buildDeathRecord', 'selectCorpseOutcome', 'retentionFor');
--     * the store transitions, each an @Either@ that returns the store
--       unchanged on refusal ('setOccurrenceCondition',
--       'clearOccurrenceCondition', 'setPlotCondition',
--       'clearPlotCondition').
--
--   The store transitions are shaped to run INSIDE one
--   @atomicModifyIORef'@: a plot transition reads the plot it edits out
--   of the very map it was handed, so it can never restore a plot that a
--   concurrent harvest or replant removed or replaced from a stale
--   snapshot, and it leaves every other entry alone.
module World.Flora.ConditionSeam
    ( -- * Vocabulary
      deathCauseText
    , parseDeathCause
      -- * Refusals
    , SeamRefusal(..)
    , renderSeamRefusal
      -- * Building a death record
    , selectCorpseOutcome
    , retentionFor
    , buildDeathRecord
      -- * Store transitions
    , setOccurrenceCondition
    , clearOccurrenceCondition
    , setPlotCondition
    , clearPlotCondition
    ) where

import UPrelude
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import World.Flora.Condition
import World.Flora.CropPlot
    ( CropPlots, CropPlotOf(..), CropConditionRefusal(..)
    , clearCropPlotCondition, cropPlotElapsedDays, cropPlotInstance
    , markCropPlotDead, renderCropConditionProblem )
import World.Flora.Growth
    ( floraGrowth, growthVisualPhase, growthVisualStage )
import World.Flora.Identity (FloraInstanceId)
import World.Flora.Types

-- | The Lua-facing spelling of a cause. The authoring boundary's
--   @Engine.Asset.YamlFlora.deathCauseText@ is the same table (pinned by
--   the @flora condition seam@ hspec group); it lives there because the
--   YAML decoder owns the closed vocabulary, and here because
--   @World.*@ may not import the asset loader.
deathCauseText ∷ FloraDeathCause → Text
deathCauseText CauseNatural = "natural"
deathCauseText CauseDrought = "drought"
deathCauseText CauseFrost   = "frost"
deathCauseText CauseFire    = "fire"
deathCauseText CauseDisease = "disease"
deathCauseText CauseDamage  = "damage"
deathCauseText CauseUnknown = "unknown"

-- | The exact inverse of 'deathCauseText'. @unknown@ is a VALID cause; a
--   token outside the vocabulary is 'Nothing' (and a refused set).
parseDeathCause ∷ Text → Maybe FloraDeathCause
parseDeathCause t =
    lookup t [ (deathCauseText c, c) | c ← [minBound .. maxBound] ]

-- | Why a transition did nothing. Every one leaves the store untouched and
--   is recorded as an action outcome naming the reason.
data SeamRefusal
    = RefusedUnknownCause !Text
    | RefusedUnknownOccurrence
      -- ^ No resident or recorded occurrence has that identity on that
      --   tile (or no plot is planted there).
    | RefusedUnknownSpecies
      -- ^ The occurrence names a species the catalog does not hold.
    | RefusedAlreadyConditioned
    | RefusedInvalidRecord ![Text]
      -- ^ The shared validity check rejected the built record (rendered).
    deriving (Show, Eq)

renderSeamRefusal ∷ SeamRefusal → Text
renderSeamRefusal r = case r of
    RefusedUnknownCause t →
        "unknown cause '" <> t <> "' (expected one of "
        <> T.intercalate ", " (map deathCauseText [minBound .. maxBound]) <> ")"
    RefusedUnknownOccurrence → "no such flora occurrence"
    RefusedUnknownSpecies → "the occurrence's species is not registered"
    RefusedAlreadyConditioned → "the occurrence already has a condition"
    RefusedInvalidRecord ps →
        "invalid condition record: " <> T.intercalate "; " ps

-- | The corpse outcome the species policy selects for a death at
--   @phase@ from @cause@ (contract §7.3). An override declares a COMPLETE
--   outcome and inherits nothing; most specific first — phase and cause,
--   then phase alone, then cause alone, then the species-level policy.
--   Phase beats cause because retention is structural (D-12).
selectCorpseOutcome
    ∷ FloraCorpsePolicy → Maybe LifePhaseTag → FloraDeathCause → CorpseOutcome
selectCorpseOutcome policy mPhase cause =
    fromMaybe (fcpOutcome policy) $ listToMaybe $ mapMaybe pick selectors
  where
    pick sel = HM.lookup sel (fcpOverrides policy)
    selectors =
        [ CorpseOverrideSelector (Just p) (Just cause) | Just p ← [mPhase] ]
        ⧺ [ CorpseOverrideSelector (Just p) Nothing | Just p ← [mPhase] ]
        ⧺ [ CorpseOverrideSelector Nothing (Just cause) ]

-- | The retention a record snapshots: a persistent outcome has no expiry;
--   a transient one expires the ABSOLUTE day @began + durationDays@, and
--   its successor is the species' WILD outcome in a wild context but
--   always @await_replanting@ in a cultivated one (D-13, D-19).
retentionFor ∷ FloraContext → Int → CorpseOutcome → ConditionRetention
retentionFor _   _     CorpsePersistent = RetentionPersistent
retentionFor ctx began (CorpseTransient days successor) =
    RetentionTransient (began + days) $ case ctx of
        ContextWild       → successorForWild successor
        ContextCultivated → RetainAwaitReplanting

-- | The record a set builds for @inst@ dying of @cause@ on world day
--   @worldDay@.
--
--   @clockDay@ is the occurrence's OWN growth clock reading that day (the
--   world day for wild and row flora, the elapsed-days-since-planting for
--   a plot) and @tile@ its canonical global tile. The frozen phase and
--   stage are exactly what the render adapter presents for the same
--   state ('growthVisualPhase' / 'growthVisualStage' — the LAST LIVING
--   ones if the occurrence is already inside its natural dead window), so
--   the freeze can never disagree with what was on screen at death.
buildDeathRecord
    ∷ FloraSpecies → Int → (Int, Int) → Int → Int → FloraInstance
    → FloraDeathCause → FloraConditionRecord
buildDeathRecord species daysPerYear tile worldDay clockDay inst cause =
    FloraConditionRecord
        { fcrSpecies   = fiSpecies inst
        , fcrTile      = tile
        , fcrCondition = ConditionDead
        , fcrCause     = cause
        , fcrBeganDay  = worldDay
        , fcrContext   = fiContext inst
        , fcrPhase     = phase
        , fcrStage     = growthVisualStage species daysPerYear clockDay inst g
        , fcrRetention = retentionFor (fiContext inst) worldDay
              (selectCorpseOutcome (fsCorpsePolicy species) phase cause)
        }
  where
    g     = floraGrowth Nothing species clockDay inst
    phase = growthVisualPhase species inst g

-- | Set a wild or row-flora occurrence's condition. Refuses an unknown
--   species, then (via the store) an invalid record or an occurrence that
--   already has one.
setOccurrenceCondition
    ∷ FloraCatalog → Int → (Int, Int) → Int → FloraInstance → FloraDeathCause
    → FloraConditions → Either SeamRefusal FloraConditions
setOccurrenceCondition catalog daysPerYear tile worldDay inst cause store = do
    species ← maybe (Left RefusedUnknownSpecies) Right
                  (lookupSpecies (fiSpecies inst) catalog)
    let record = buildDeathRecord species daysPerYear tile worldDay worldDay
                     inst cause
    let replaceable = case lookupFloraCondition (fiInstanceId inst) store of
            Just old | fcrCondition old ≡ ConditionReseeded →
                clearFloraCondition (fiInstanceId inst) store
            _ → store
    either (Left . fromStoreRefusal) Right
        (markFloraDead (fiInstanceId inst) record replaceable)
  where
    fromStoreRefusal (AlreadyRecorded _) = RefusedAlreadyConditioned
    fromStoreRefusal (InvalidRecord ps) =
        RefusedInvalidRecord (map renderConditionProblem ps)

-- | Clear exactly one occurrence's condition: idempotent when it has none.
clearOccurrenceCondition
    ∷ FloraInstanceId → FloraConditions → FloraConditions
clearOccurrenceCondition = clearFloraCondition

-- | Set a groundcover plot's condition. The record is built from the plot
--   AS THE GIVEN MAP HOLDS IT, so the transition is correct inside an
--   atomic modify and a vanished plot is simply 'RefusedUnknownOccurrence'.
setPlotCondition
    ∷ FloraCatalog → Int → (Int, Int) → Int → FloraDeathCause
    → CropPlots → Either SeamRefusal CropPlots
setPlotCondition catalog daysPerYear tile worldDay cause plots = do
    cp ← maybe (Left RefusedUnknownOccurrence) Right (HM.lookup tile plots)
    species ← maybe (Left RefusedUnknownSpecies) Right
                  (lookupSpecies (cpSpecies cp) catalog)
    let record = buildDeathRecord species daysPerYear tile worldDay
                     (cropPlotElapsedDays worldDay cp)
                     (cropPlotInstance cp) cause
    case markCropPlotDead tile record cp of
        Right cp' → Right (HM.insert tile cp' plots)
        Left CropAlreadyConditioned → Left RefusedAlreadyConditioned
        Left (CropInvalidCondition ps) →
            Left (RefusedInvalidRecord (map renderCropConditionProblem ps))

-- | Clear a plot's condition (idempotent on an unconditioned plot); a tile
--   with no plot is refused. Touches only the addressed plot's condition.
clearPlotCondition
    ∷ (Int, Int) → CropPlots → Either SeamRefusal CropPlots
clearPlotCondition tile plots = case HM.lookup tile plots of
    Nothing → Left RefusedUnknownOccurrence
    Just cp → Right (HM.insert tile (clearCropPlotCondition cp) plots)

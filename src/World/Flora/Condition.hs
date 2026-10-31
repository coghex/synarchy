{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The page-scoped sparse flora CONDITION store (#2549, EFM-5).
--
--   Wild flora is regenerated deterministically with its chunk and row
--   crops are replayed from the edit log, so a death can only be durable
--   as a small overlay joined back onto the regenerated occurrence by
--   its 'FloraInstanceId' — the shape 'World.Flora.Harvest.FloraHarvests'
--   and 'World.Chop.Types.ChopDesignations' already have. This module is
--   that overlay's vocabulary and its pure operations. It changes no
--   rendering, growth, tick, hazard or crop-plot behaviour: a record's
--   only observable effect is that it exists, survives, and can be
--   looked up.
--
--   == What a record holds
--
--   Semantic tags only, never a texture handle, a path or a 'FloraId'
--   ordinal (design record D-6): the condition ('ConditionDead'
--   initially, in the append-only 'FloraCondition' vocabulary), the
--   cause, the absolute world day it began, the occurrence's render
--   context, the life phase and annual stage frozen at the transition
--   (D-10), and the retention outcome snapshotted at death (D-5, D-13,
--   D-19) — 'RetentionPersistent', or 'RetentionTransient' with its
--   expiry day and successor.
--
--   Two fields exist for VALIDATION rather than for the outcome:
--
--     * the species, because a 'FloraInstanceId' is an opaque digest or a
--       page-local counter and cannot recover a name, and a record for a
--       wild occurrence whose chunk is not resident must still be
--       checkable against the loading build's catalog
--       ('World.Save.Types.missingFloraReferences'); and
--     * the canonical tile the occurrence stands on, because a planted
--       id encodes neither page nor tile and may repeat across pages, so
--       the only page-scoped provenance there is to check is the
--       location ('World.Save.Integrity').
--
--   == Species spelling
--
--   Parameterized like 'World.Flora.CropPlot.CropPlotOf': the live
--   session holds 'World.Flora.Types.FloraId', a save holds the durable
--   'World.Flora.Reference.FloraRef', and the two conversions are
--   species swaps ('traverseConditionSpecies').
module World.Flora.Condition
    ( -- * Retention outcome
      RetentionSuccessor(..)
    , ConditionRetention(..)
    , successorForWild
      -- * The record
    , FloraConditionRecordOf(..)
    , FloraConditionRecord
    , SavedFloraConditionRecord
    , traverseConditionSpecies
      -- * The store
    , FloraConditions
    , SavedFloraConditions
    , emptyFloraConditions
    , ConditionRefusal(..)
    , markFloraDead
    , clearFloraCondition
    , lookupFloraCondition
      -- * Record validity
    , ConditionProblem(..)
    , conditionRecordProblems
    , recordShapeProblems
    , renderConditionProblem
      -- * Wire vocabulary
    , VocabTag
    , toVocabTag
    , fromVocabTag
    ) where

import UPrelude
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import World.Flora.Identity
    (FloraInstanceId, isFloraInstanceIdNone)
import World.Flora.Reference (FloraRef)
import World.Flora.Types
    ( AnnualStageTag, CorpseSuccessor(..), FloraCondition(..)
    , FloraContext(..), FloraDeathCause, FloraId, LifePhaseTag(..) )

-- | What follows a transient corpse. The two WILD outcomes are the
--   species-level @successor@ (@reseed@ | @absent@); 'RetainAwaitReplanting'
--   is the CULTIVATED outcome, a rule of the render context (D-13, D-19)
--   that has deliberately no constructor in
--   'World.Flora.Types.CorpseSuccessor' and is rejected as a species-level
--   token. A persisted record needs all three, because it snapshots the
--   outcome that applies to ITS context.
--
--   Positional like every enum: append-only
--   (@tools\/enum_append_only_audit.py@).
data RetentionSuccessor
    = RetainReseed
    | RetainAbsent
    | RetainAwaitReplanting
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize, NFData)

-- | The retention outcome snapshotted when death occurs, so a later
--   content-pack edit cannot silently reinterpret an existing save (D-5).
--   Selecting it from species policy is EFM-7\/EFM-10's job; this issue
--   stores whatever the caller supplies.
data ConditionRetention
    = RetentionPersistent
      -- ^ Visible until explicitly cleared, replaced or revived. Has no
      --   expiry, by construction.
    | RetentionTransient !Int !RetentionSuccessor
      -- ^ The ABSOLUTE world day the corpse stops being visible (equal
      --   to the begin day is permitted: a zero-length window), then the
      --   successor.
    deriving (Show, Eq, Generic, Serialize, NFData)

-- | The wild-context successor a species policy names, as a record
--   stores it.
successorForWild ∷ CorpseSuccessor → RetentionSuccessor
successorForWild SuccessorReseed = RetainReseed
successorForWild SuccessorAbsent = RetainAbsent

-- | One occurrence's condition record. The @world-activity@ wire shape is
--   the frozen DTO in "World.Save.Component.PageActivity", not this
--   record; only the transitional in-memory 'World.Save.Types.WorldPageSave'
--   bridge encodes it positionally, which is why it derives 'Serialize'.
data FloraConditionRecordOf s = FloraConditionRecord
    { fcrSpecies   ∷ !s
      -- ^ Validation provenance: see the module header.
    , fcrTile      ∷ !(Int, Int)
      -- ^ The occurrence's CANONICAL global tile
      --   ('World.Generate.Coordinates.canonicalTile'), for the same
      --   reason.
    , fcrCondition ∷ !FloraCondition
      -- ^ 'ConditionDead' initially. 'ConditionAlive' is not a stored
      --   condition: a living occurrence has no record.
    , fcrCause     ∷ !FloraDeathCause
    , fcrBeganDay  ∷ !Int
      -- ^ Absolute world day ('World.Time.Types.worldAbsoluteDay') the
      --   condition began.
    , fcrContext   ∷ !FloraContext
    , fcrPhase     ∷ !(Maybe LifePhaseTag)
      -- ^ Frozen at the transition (D-10); 'Nothing' for a species that
      --   declares no phases at all, whose growth has no visual phase
      --   (flora visual state contract §1). 'PhaseDead' is not a phase
      --   at death (§1.1) and is refused ('PhaseDeadAtDeath').
    , fcrStage     ∷ !(Maybe AnnualStageTag)
      -- ^ Frozen at the transition (D-10); 'Nothing' for an occurrence
      --   with no annual cycle, which is valid.
    , fcrRetention ∷ !ConditionRetention
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- | The LIVE record: species by runtime handle.
type FloraConditionRecord = FloraConditionRecordOf FloraId

-- | The PERSISTED record: species by durable reference.
type SavedFloraConditionRecord = FloraConditionRecordOf FloraRef

-- | Swap the species spelling, leaving every other field alone.
traverseConditionSpecies
    ∷ Applicative f
    ⇒ (a → f b) → FloraConditionRecordOf a → f (FloraConditionRecordOf b)
traverseConditionSpecies f r =
    (\s → r { fcrSpecies = s }) <$> f (fcrSpecies r)

-- | At most one record per occurrence, keyed by the #1854 identity.
type FloraConditions = HM.HashMap FloraInstanceId FloraConditionRecord

type SavedFloraConditions =
    HM.HashMap FloraInstanceId SavedFloraConditionRecord

emptyFloraConditions ∷ HM.HashMap FloraInstanceId (FloraConditionRecordOf s)
emptyFloraConditions = HM.empty

-- | Why a store operation did nothing. The store is returned unchanged
--   on every refusal.
data ConditionRefusal
    = AlreadyRecorded !FloraInstanceId
      -- ^ The occurrence already has a record; it is preserved exactly,
      --   never overwritten. Replacing a condition is an explicit
      --   'clearFloraCondition' first.
    | InvalidRecord ![ConditionProblem]
    deriving (Show, Eq)

-- | Mark an occurrence dead with a caller-supplied outcome. Refuses an
--   invalid record, then an occurrence that already has one.
markFloraDead
    ∷ FloraInstanceId → FloraConditionRecordOf s
    → HM.HashMap FloraInstanceId (FloraConditionRecordOf s)
    → Either ConditionRefusal
             (HM.HashMap FloraInstanceId (FloraConditionRecordOf s))
markFloraDead key record store
    | not (null problems)  = Left (InvalidRecord problems)
    | HM.member key store  = Left (AlreadyRecorded key)
    | otherwise            = Right (HM.insert key record store)
  where problems = conditionRecordProblems key record

-- | Remove exactly one occurrence's record (a no-op when there is none).
clearFloraCondition
    ∷ FloraInstanceId
    → HM.HashMap FloraInstanceId (FloraConditionRecordOf s)
    → HM.HashMap FloraInstanceId (FloraConditionRecordOf s)
clearFloraCondition = HM.delete

lookupFloraCondition
    ∷ FloraInstanceId
    → HM.HashMap FloraInstanceId (FloraConditionRecordOf s)
    → Maybe (FloraConditionRecordOf s)
lookupFloraCondition = HM.lookup

-- | A reason one record is not valid. Record-validity checks only: none
--   of them selects an outcome from species policy.
data ConditionProblem
    = ReservedKey
      -- ^ The key is 'World.Flora.Identity.floraInstanceIdNone', which
      --   names no plant.
    | NotAStoredCondition
      -- ^ The condition is 'ConditionAlive'.
    | PhaseDeadAtDeath
      -- ^ The frozen phase is the legacy 'PhaseDead' token, which the
      --   contract forbids as a phase at death.
    | ExpiryBeforeBegin !Int !Int
      -- ^ @(expiry day, begin day)@.
    | WildAwaitsReplanting
      -- ^ A wild occurrence's successor is @reseed@ or @absent@ (D-19).
    | CultivatedNamesWildSuccessor !RetentionSuccessor
      -- ^ A cultivated occurrence's transient outcome is awaiting
      --   replanting (D-13).
    | OutOfVocabulary !Text !Word8
      -- ^ A wire tag outside the named field's vocabulary — produced by
      --   the DTO decode in "World.Save.Component.PageActivity", never by
      --   a typed record.
    deriving (Show, Eq)

-- | Every reason the record is not valid, in a stable order.
conditionRecordProblems
    ∷ FloraInstanceId → FloraConditionRecordOf s → [ConditionProblem]
conditionRecordProblems key r =
    [ ReservedKey | isFloraInstanceIdNone key ] ⧺ recordShapeProblems r

-- | The key-INDEPENDENT validity of a record: everything
--   'conditionRecordProblems' judges except the occurrence key. A
--   groundcover plot (#2552) embeds the same record under a TILE key
--   rather than an instance id, so it cannot be asked 'ReservedKey' and
--   judges the record on exactly these terms, in the same order.
recordShapeProblems ∷ FloraConditionRecordOf s → [ConditionProblem]
recordShapeProblems r = concat
    [ [ NotAStoredCondition | fcrCondition r ≢ ConditionDead ]
    , [ PhaseDeadAtDeath | fcrPhase r ≡ Just PhaseDead ]
    , case fcrRetention r of
        RetentionPersistent → []
        RetentionTransient expiry successor → concat
            [ [ ExpiryBeforeBegin expiry (fcrBeganDay r)
              | expiry < fcrBeganDay r ]
            , case (fcrContext r, successor) of
                (ContextWild, RetainAwaitReplanting) → [WildAwaitsReplanting]
                (ContextCultivated, RetainAwaitReplanting) → []
                (ContextCultivated, s) → [CultivatedNamesWildSuccessor s]
                (ContextWild, _) → []
            ]
    ]

renderConditionProblem ∷ ConditionProblem → Text
renderConditionProblem p = case p of
    ReservedKey →
        "the key is the reserved non-identity value"
    NotAStoredCondition →
        "the condition is not a stored condition (only dead is)"
    PhaseDeadAtDeath →
        "the frozen life phase is the legacy dead token, which is not a \
        \phase at death"
    ExpiryBeforeBegin expiry began →
        "expiry day " <> tshow expiry <> " precedes begin day " <> tshow began
    WildAwaitsReplanting →
        "a wild occurrence cannot await replanting"
    CultivatedNamesWildSuccessor s →
        "a cultivated occurrence's transient successor must await "
        <> "replanting, not " <> tshow s
    OutOfVocabulary field tag →
        field <> " tag " <> tshow tag <> " is outside its vocabulary"

-- | A vocabulary value as the wire carries it: its constructor index.
--   The wire stores the index rather than the closed enum so that a
--   value outside the vocabulary DECODES and is then refused by
--   validation with the page and key named, instead of failing the whole
--   component decode with a positional-tag error that names neither.
--   The index is the 'Generic' 'Serialize' tag of the same enum, which
--   @tools\/enum_append_only_audit.py@ already freezes.
type VocabTag = Word8

toVocabTag ∷ Enum a ⇒ a → VocabTag
toVocabTag = fromIntegral ∘ fromEnum

fromVocabTag ∷ (Enum a, Bounded a) ⇒ VocabTag → Maybe a
fromVocabTag tag = L.find ((≡ tag) ∘ toVocabTag) [minBound .. maxBound]

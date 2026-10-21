{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The @"relation-causes"@ save component (#2548, FTS-6B of the
--   faction-tag arc #2496): every live DIRECTED relation cause
--   ("Unit.Faction.RelationStore"), in its own independently versioned
--   envelope component.
--
--   __Session-scoped, not page-scoped.__ A cause relates two TAGS, and a
--   tag is a session-wide identity — a unit on any page holds it — so
--   the component writes one list for the whole session and declares no
--   dependency on @"world-pages"@. It is captured from, and restored
--   onto, the global 'Engine.Core.State.relationCauseStoreRef' on the
--   same seam as the texture palette.
--
--   __Why a FOURTH optional component is justified.__
--   'World.Save.Component.saveComponentRegistry' requires every
--   component except #1087's @"container-knowledge"@, #1246's
--   @"transfer-orders"@ and #2512's @"portable-knowledge"@, and
--   @docs\/persistence_contract.md@ §5 admits an optional one only when
--   ABSENCE has an honest default rather than an invented one. It does
--   here: every baseline in @docs\/save_compat\/manifest.json@ predates
--   the store, so a session written before this slice had nowhere to
--   record a cause and no cause existed. An absent payload therefore
--   restores 'Unit.Faction.RelationStore.emptyRelationCauseStore' — "no
--   cause exists" — which is TRUE of every earlier save rather than
--   guessed. Requiring it instead would refuse every tracked baseline
--   outright.
--
--   \"Optional\" governs ABSENCE only. A PRESENT payload that is
--   malformed, carries a malformed tag, repeats a cause, or is encoded
--   at a version this reader does not accept remains a hard load error
--   exactly like any required component's: \"absent\" and \"broken\"
--   are different answers, and a cause is never silently discarded.
--
--   __The frozen-DTO boundary rule__ ("World.Save.Component.Types")
--   applies: 'Unit.Faction.FactionRelation' is live policy vocabulary
--   that FTS-7 is expected to reshape, so it never rides the wire as
--   itself. 'RelationValueDTO' mirrors it with an explicit total
--   conversion, so a constructor appended to the live enum is a
--   deliberate, visible wire event rather than a silent
--   reinterpretation of every saved cause.
--
--   __Order independence.__ The encoder writes the store's own
--   deterministic order, and the decoder rebuilds through
--   'relationCauseStoreFromList', so any permutation of a valid payload
--   restores the same store.
module World.Save.Component.RelationCauses
    ( relationCausesCodec
    , applyRelationCauses
    , validateRelationCauses
      -- * Frozen wire DTOs
    , RelationCausesDTO(..)
    , RelationCauseDTO(..)
    , RelationValueDTO(..)
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Unit.Faction (FactionRelation(..))
import Unit.Faction.Profile
    ( LiveRelationCause(..), factionTagText, relationCauseId
    , relationCauseIdText )
import Unit.Faction.RelationStore
    ( mkLiveRelationCause, relationCauseStoreFromList
    , relationCauseStoreOverlay )
import World.Save.Component.Types
import World.Save.Snapshot (SessionSnapshot(..))

-- | Frozen mirror of 'Unit.Faction.FactionRelation'. Positional by
--   constructor tag (cereal's 'Generic' encoding), so it is APPEND-ONLY
--   and guarded as such by @tools\/enum_append_only_audit.py@.
data RelationValueDTO
    = RvdAlly
    | RvdNeutral
    | RvdHostile
    deriving (Show, Eq, Generic, Serialize)

toRelationValueDTO ∷ FactionRelation → RelationValueDTO
toRelationValueDTO r = case r of
    RelAlly    → RvdAlly
    RelNeutral → RvdNeutral
    RelHostile → RvdHostile

fromRelationValueDTO ∷ RelationValueDTO → FactionRelation
fromRelationValueDTO d = case d of
    RvdAlly    → RelAlly
    RvdNeutral → RelNeutral
    RvdHostile → RelHostile

-- | One cause: its owner's identity, the ordered tag pair, and the
--   relation value. Tags are carried as text and re-validated on the way
--   in ('mkLiveRelationCause').
data RelationCauseDTO = RelationCauseDTO
    { rcdOwner    ∷ !Text
    , rcdSource   ∷ !Text
    , rcdTarget   ∷ !Text
    , rcdRelation ∷ !RelationValueDTO
    } deriving (Show, Eq, Generic, Serialize)

-- | The whole session's causes.
newtype RelationCausesDTO =
    RelationCausesDTO { rcsdCauses ∷ [RelationCauseDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

toCauseDTO ∷ LiveRelationCause → RelationCauseDTO
toCauseDTO c = RelationCauseDTO
    { rcdOwner    = relationCauseIdText (lrcCause c)
    , rcdSource   = factionTagText (lrcSource c)
    , rcdTarget   = factionTagText (lrcTarget c)
    , rcdRelation = toRelationValueDTO (lrcRelation c)
    }

-- | 'Left' for a malformed tag, naming the cause.
fromCauseDTO ∷ RelationCauseDTO → Either Text LiveRelationCause
fromCauseDTO d =
    mkLiveRelationCause (relationCauseId (rcdOwner d))
        (rcdSource d) (rcdTarget d) (fromRelationValueDTO (rcdRelation d))

-- | Component-local invariants:
--
--     * a source or target that is not a valid tag (non-empty, no
--       whitespace) — the store never contains one, so no encoder
--       produces one, and silently dropping the cause would LOSE data;
--     * the same (owner, source, target) repeated. The encoder writes
--       each key once, and the in-memory merge would otherwise invent a
--       value no one wrote by reducing the two.
--
--   Deliberately NOT checked: whether a tag is DECLARED in the faction
--   catalogue. The namespace is open (D-12), so an unknown but valid
--   tag is a legitimate runtime-minted one.
validateRelationCauses ∷ RelationCausesDTO → [ComponentError]
validateRelationCauses (RelationCausesDTO causes) = malformed ⧺ repeated
  where
    err = ComponentError relationCausesComponentId 1 ValidatePhase
    malformed = [ err e | d ← causes, Left e ← [fromCauseDTO d] ]
    keys = L.sort [ (rcdOwner d, rcdSource d, rcdTarget d) | d ← causes ]
    repeated =
        [ err ("cause '" <> o <> "' from " <> tshow s <> " to " <> tshow t
                 <> " appears " <> tshow n <> " times")
        | ((o, s, t), n) ← Map.toAscList
            (Map.fromListWith (+) [ (k, 1 ∷ Int) | k ← keys ])
        , n > 1 ]

-- | v1, and OPTIONAL — see the module header for why the absent case is
--   a legitimate default rather than a decode failure.
--
--   Declares NO dependencies: no part of it is keyed by a page, and
--   whether a tag is carried by a live unit is gameplay (a cause may
--   outlive every holder of its tag), not an assembly-time invariant.
relationCausesCodec ∷ ComponentCodec RelationCausesDTO
relationCausesCodec = componentCodec ComponentSpec
    { csComponent     = relationCausesComponentId
    , csVersion       = 1
    , csRequired      = False
    , csDeps          = []
    , csEncode        = \snap → RelationCausesDTO
        (map toCauseDTO (relationCauseStoreOverlay (snapRelationCauses snap)))
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = validateRelationCauses
    }

-- | Install the decoded causes. Runs only after 'validateRelationCauses'
--   accepted the payload, so every cause converts; the conversion's
--   'Left' arm is therefore unreachable and is skipped rather than
--   invented around.
applyRelationCauses
    ∷ Word32 → RelationCausesDTO → SessionSnapshot → SessionSnapshot
applyRelationCauses _ver (RelationCausesDTO causes) snap = snap
    { snapRelationCauses =
        relationCauseStoreFromList [ c | d ← causes, Right c ← [fromCauseDTO d] ] }

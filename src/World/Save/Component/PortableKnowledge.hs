{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The @"portable-knowledge"@ save component (#2512, epic #1231
--   PLC-7): what the player remembers about each PORTABLE container,
--   keyed by the item's own 'Item.Types.iiInstanceId'.
--
--   __Session-scoped, not page-scoped__ — the one structural difference
--   from #1087's @"container-knowledge"@ sibling
--   ("World.Save.Component.Knowledge"), and the reason this is a
--   separate component rather than another slice of that one. A crate
--   is carried between pages and owners; its record must survive that
--   move without being copied, so the component writes ONE map for the
--   whole session rather than a per-page slice, and it declares no
--   dependency on @"world-pages"@ because no part of it is keyed by a
--   page. @docs/portable_loot_containers.md@ D-13 and D-24 authorize
--   exactly that shape.
--
--   __Why a THIRD optional component is justified.__
--   'World.Save.Component.saveComponentRegistry' requires every
--   component except #1087's @"container-knowledge"@ and #1246's
--   @"transfer-orders"@, and
--   @docs/persistence_contract.md@ admits an optional one only when
--   ABSENCE has an honest default rather than an invented one. It does
--   here, for the same reason it did for both of those: every baseline
--   in @docs/save_compat/manifest.json@ predates portable containers
--   entirely, so a session written before this slice had nowhere to
--   record an observation and genuinely knew nothing about any crate.
--   An absent payload therefore restores 'Item.Knowledge.emptyPortableKnowledge'
--   — every portable container NEVER-INSPECTED, never known-empty and
--   never back-filled from whatever the live item happens to hold now.
--   Requiring it instead would refuse every tracked baseline outright.
--
--   \"Optional\" governs ABSENCE only. A PRESENT payload that is
--   malformed, or encoded at a version this reader does not accept,
--   remains a hard load error exactly like any required component's:
--   \"absent\" and \"broken\" are different answers.
--
--   The frozen-DTO boundary rule ("World.Save.Component.Types") applies
--   as usual: 'Item.Knowledge.PortableRecord' is a live gameplay record
--   that could plausibly gain fields and directly carries mutable
--   'Item.Types.ItemInstance' values, so it is mirrored here by
--   'PortableRecordDTO' whose items reuse the shared, recursively
--   frozen 'ItemInstanceDTO'.
module World.Save.Component.PortableKnowledge
    ( portableKnowledgeCodec
    , applyPortableKnowledge
    , PortableKnowledgeDTO(..)
    , PortableRecordDTO(..)
    , WeightObservationDTO(..)
    , ContentsObservationDTO(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Item.Knowledge
    ( PortableKnowledge(..), PortableRecord(..)
    , WeightObservation(..), ContentsObservation(..) )
import World.Save.Component.Page
    ( ItemInstanceDTO(..), toItemInstanceDTO, fromItemInstanceDTO )
import World.Save.Component.Types
import World.Save.Snapshot (SessionSnapshot(..))

-- | Frozen mirror of 'Item.Knowledge.WeightObservation'.
data WeightObservationDTO = WeightObservationDTO
    { wodWeight ∷ !Float
    , wodAt     ∷ !Double
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'Item.Knowledge.ContentsObservation'.
--
--   'codItems' are HISTORICAL OBSERVATIONS: full instance copies as
--   they were when the container was opened, deliberately excluded from
--   'World.Save.Snapshot.allItemInstanceIds' and from live
--   @item_instance@ reference resolution, so a remembered id stays
--   valid after the real item has moved or gone. Their DEF NAMES are
--   still ordinary content references and are validated by
--   'World.Save.Types.missingItemDefReferences'.
data ContentsObservationDTO = ContentsObservationDTO
    { codItems ∷ ![ItemInstanceDTO]
    , codAt    ∷ !Double
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'Item.Knowledge.PortableRecord'. Both observations
--   are independently optional on the wire exactly as they are live —
--   an absent one means "never learned", never a zero.
data PortableRecordDTO = PortableRecordDTO
    { prdWeight   ∷ !(Maybe WeightObservationDTO)
    , prdContents ∷ !(Maybe ContentsObservationDTO)
    } deriving (Show, Eq, Generic, Serialize)

-- | The whole session's map, keyed by item instance id.
newtype PortableKnowledgeDTO =
    PortableKnowledgeDTO { pkdRecords ∷ HM.HashMap Word64 PortableRecordDTO }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

toPortableRecordDTO ∷ PortableRecord → PortableRecordDTO
toPortableRecordDTO r = PortableRecordDTO
    { prdWeight   = toWeightDTO ⊚ prWeight r
    , prdContents = toContentsDTO ⊚ prContents r
    }
  where
    toWeightDTO w = WeightObservationDTO (woWeight w) (woAt w)
    toContentsDTO c =
        ContentsObservationDTO (map toItemInstanceDTO (coItems c)) (coAt c)

fromPortableRecordDTO ∷ PortableRecordDTO → PortableRecord
fromPortableRecordDTO d = PortableRecord
    { prWeight   = fromWeightDTO ⊚ prdWeight d
    , prContents = fromContentsDTO ⊚ prdContents d
    }
  where
    fromWeightDTO w = WeightObservation (wodWeight w) (wodAt w)
    fromContentsDTO c =
        ContentsObservation (map fromItemInstanceDTO (codItems c)) (codAt c)

-- | The fault in a remembered scalar, or 'Nothing' when it is a finite,
--   non-negative number.
--
--   Ordered so a non-finite value is never described as "negative" —
--   @NaN@ makes every ordered comparison false, and @-Infinity@ IS
--   @< 0@ while being wrong for a reason a reader needs told exactly.
--   Same three answers, in the same order, as
--   'World.Save.Component.Knowledge''s own @scalarFault@; kept separate
--   rather than shared because the two components' diagnostics quote
--   different identities and a shared helper would have to be
--   parameterised over nothing but its own name.
scalarFault ∷ RealFloat a ⇒ a → Maybe Text
scalarFault v
    | isNaN v      = Just "a not-a-number"
    | isInfinite v = Just "an infinite"
    | v < 0        = Just "a negative"
    | otherwise    = Nothing

-- | Component-local invariants — the same narrow pair
--   "World.Save.Component.Knowledge" checks, once per optional
--   observation:
--
--     * a remembered weight that is not a finite, non-negative number
--       (mass is non-negative and finite; 'Item.Types.itemTotalWeight'
--       sums finitely many non-negative finite terms), and
--     * an observation time that is not a finite, non-negative number
--       (game-time seconds start at 0 and only advance by finite
--       steps).
--
--   Both are structurally decodable and neither is producible by a real
--   observation, so both are the present-but-malformed payload
--   @docs/persistence_contract.md@ §5 requires the all-or-nothing load
--   to reject.
--
--   Deliberately NOT checked, and both exclusions are load-bearing:
--
--     * whether a remembered id still names a LIVE instance. A crate
--       that was destroyed, or that never existed in this session, is
--       gameplay rather than corruption — it is SCRUBBED with a
--       diagnostic at the load boundary ("World.Load.Stage") against
--       the replacement session's own item enumeration, exactly as
--       #1087's demolished-container memory is.
--     * whether the remembered weight still equals
--       'Item.Types.itemTotalWeight' of the remembered contents.
--       The weight is what was measured THEN, against the defs as they
--       were then; re-deriving it now would reject perfectly valid
--       historical observations. It is also legitimately unequal by
--       design, since it measures the whole crate and the contents list
--       is only what was inside it.
validatePortableKnowledge ∷ PortableKnowledgeDTO → [ComponentError]
validatePortableKnowledge (PortableKnowledgeDTO records) = concat
    [ [ err ("item #" <> tshow iid <> " has " <> fault
             <> " remembered weight (" <> tshow (wodWeight w) <> ")")
      | (iid, r) ← entries, Just w ← [prdWeight r]
      , Just fault ← [scalarFault (wodWeight w)] ]
    , [ err ("item #" <> tshow iid <> " has " <> fault
             <> " weigh time (" <> tshow (wodAt w) <> ")")
      | (iid, r) ← entries, Just w ← [prdWeight r]
      , Just fault ← [scalarFault (wodAt w)] ]
    , [ err ("item #" <> tshow iid <> " has " <> fault
             <> " reveal time (" <> tshow (codAt c) <> ")")
      | (iid, r) ← entries, Just c ← [prdContents r]
      , Just fault ← [scalarFault (codAt c)] ]
    ]
  where
    -- Sorted so an adversarial payload's findings come out in one
    -- deterministic order rather than a hash-map's arbitrary first
    -- entry (the cap upstream keeps only a prefix).
    entries = L.sortOn fst (HM.toList records)
    err     = ComponentError portableKnowledgeComponentId 1 ValidatePhase

-- | v1, and OPTIONAL — see the module header for why the absent case is
--   a legitimate default rather than a decode failure.
--
--   Declares NO dependencies. It is not keyed by page, so it needs no
--   page-set authority to check itself against; and the live-instance
--   set its records are scrubbed against is a LOAD-BOUNDARY concern
--   ("World.Load.Stage"), deliberately not an assembly-time one — a
--   dangling record is tolerated gameplay, so making @"world-activity"@
--   /@"buildings"@/@"units"@ dependencies here would imply a check this
--   component must not perform.
portableKnowledgeCodec ∷ ComponentCodec PortableKnowledgeDTO
portableKnowledgeCodec = componentCodec ComponentSpec
    { csComponent     = portableKnowledgeComponentId
    , csVersion       = 1
    , csRequired      = False
    , csDeps          = []
    , csEncode        = \snap → PortableKnowledgeDTO
        (HM.map toPortableRecordDTO
                (pkRecords (snapPortableKnowledge snap)))
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = validatePortableKnowledge
    }

applyPortableKnowledge
    ∷ Word32 → PortableKnowledgeDTO → SessionSnapshot → SessionSnapshot
applyPortableKnowledge _ver (PortableKnowledgeDTO records) snap = snap
    { snapPortableKnowledge =
        PortableKnowledge (HM.map fromPortableRecordDTO records) }

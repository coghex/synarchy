{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The @"container-knowledge"@ save component (#1087, epic #1013 phase
--   A3): the player's remembered view of what each container holds,
--   page-scoped, in its own independently versioned envelope component
--   rather than as a field on 'BuildingInstance' — a container's
--   last-known contents are PLAYER KNOWLEDGE, not building state, and
--   the two evolve for entirely different reasons.
--
--   __This is the first OPTIONAL gameplay component.__ Every other
--   component in 'World.Save.Component.saveComponentRegistry' is
--   required, and 'World.Save.Envelope.Codec.decodeEnvelope' refuses a
--   modern envelope missing one. A container-knowledge payload cannot
--   be required, because every supported baseline the compatibility
--   manifest tracks (@docs/save_compat/manifest.json@) predates this
--   feature and legitimately carries no such component. An ABSENT
--   payload therefore leaves every page at 'blankPageSnapshot''s empty
--   default — so every container in a pre-#1087 session reads as
--   NEVER-INSPECTED, never as known-empty, and never with its live
--   contents copied in. A PRESENT payload that is malformed, or is
--   encoded at a version this reader does not accept, remains a hard
--   load error exactly like any other component's would be: "absent" and
--   "broken" are different answers.
--
--   The frozen-DTO boundary rule ("World.Save.Component.Types") applies
--   as usual: 'ContainerRecord' is a live gameplay record that could
--   plausibly gain fields, and it directly carries mutable
--   'Item.Types.ItemInstance' values, so it is mirrored by
--   'ContainerRecordDTO' whose items reuse the shared, recursively
--   frozen 'ItemInstanceDTO'.
module World.Save.Component.Knowledge
    ( containerKnowledgeCodec
    , applyContainerKnowledge
    , ContainerKnowledgeDTO(..)
    , PageContainerKnowledgeDTO(..)
    , ContainerRecordDTO(..)
    , toContainerRecordDTO
    , fromContainerRecordDTO
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Building.Knowledge (ContainerKnowledge(..), ContainerRecord(..))
import Building.Types (BuildingId(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
    ( ItemInstanceDTO(..), toItemInstanceDTO, fromItemInstanceDTO )
import World.Save.Component.Types
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))

orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId ∘ HM.elems ∘ snapPages

tshow ∷ Show a ⇒ a → Text
tshow = T.pack ∘ show

-- | Frozen mirror of 'Building.Knowledge.ContainerRecord'.
--
--   'crdItems' are HISTORICAL OBSERVATIONS: full instance copies as
--   they were at reveal time, deliberately excluded from
--   'World.Save.Snapshot.allItemInstanceIds' and from live
--   @item_instance@ reference resolution, so a remembered id stays
--   valid after the real item has moved or gone (see that function's
--   note). Their DEF NAMES are still ordinary content references and
--   are validated by 'World.Save.Types.missingItemDefReferences'.
data ContainerRecordDTO = ContainerRecordDTO
    { crdItems        ∷ ![ItemInstanceDTO]
    , crdStoredWeight ∷ !Float
    , crdRevealedAt   ∷ !Double
    } deriving (Show, Eq, Generic, Serialize)

toContainerRecordDTO ∷ ContainerRecord → ContainerRecordDTO
toContainerRecordDTO r = ContainerRecordDTO
    { crdItems        = map toItemInstanceDTO (crItems r)
    , crdStoredWeight = crStoredWeight r
    , crdRevealedAt   = crRevealedAt r
    }

fromContainerRecordDTO ∷ ContainerRecordDTO → ContainerRecord
fromContainerRecordDTO d = ContainerRecord
    { crItems        = map fromItemInstanceDTO (crdItems d)
    , crStoredWeight = crdStoredWeight d
    , crRevealedAt   = crdRevealedAt d
    }

-- | One page's slice, in the same shape every other page-scoped
--   component uses ('applyPageSlices' enforces the page-set contract).
data PageContainerKnowledgeDTO = PageContainerKnowledgeDTO
    { pckPageId  ∷ !WorldPageId
    , pckRecords ∷ !(HM.HashMap BuildingId ContainerRecordDTO)
    } deriving (Show, Eq, Generic, Serialize)

newtype ContainerKnowledgeDTO =
    ContainerKnowledgeDTO { ckdPages ∷ [PageContainerKnowledgeDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The fault in a remembered scalar, or 'Nothing' when it is a
--   finite, non-negative number.
--
--   Ordered so a non-finite value is never described as "negative":
--   @NaN@ makes every ordered comparison false, and @-Infinity@ IS
--   @< 0@ while being wrong for a reason a reader needs told exactly
--   ("infinite", not "negative"). The result is the adjective phrase
--   the diagnostic reads with, so the finite-negative message is
--   character-for-character what it always was.
scalarFault ∷ RealFloat a ⇒ a → Maybe Text
scalarFault v
    | isNaN v      = Just "a not-a-number"
    | isInfinite v = Just "an infinite"
    | v < 0        = Just "a negative"
    | otherwise    = Nothing

-- | Component-local invariants. Deliberately narrow — the two things
--   that can only ever be corruption:
--
--     * a remembered weight that is not a finite, non-negative number
--       (mass is non-negative and finite; the measure that produces
--       it, 'Item.Types.itemTotalWeight', sums finitely many
--       non-negative finite terms), and
--     * a reveal time that is not a finite, non-negative number
--       (game-time seconds start at 0 and only advance by finite
--       steps).
--
--   Both checks were once a bare @< 0@, which let a structurally
--   decodable @NaN@ or @+Infinity@ through into the restored knowledge
--   map and out to Lua as a plain number (#1278) — every ordered
--   comparison against @NaN@ is false, and @+Infinity@ is not @< 0@.
--   Neither value is producible by a real observation, so both are the
--   present-but-malformed payload @docs\/persistence_contract.md@ §5
--   requires the all-or-nothing load to reject.
--
--   Deliberately NOT checked: whether a remembered record's
--   'BuildingId' still resolves to a live building. A demolished
--   container's lingering memory is gameplay, not corruption — the
--   same tolerated-dangling contract a craft bill's demolished station
--   already has ("World.Save.Snapshot") — and it is SCRUBBED with a
--   diagnostic at the load boundary ("World.Load.Stage") rather than
--   rejected. Nor is the stored weight re-derived from the items and
--   compared: the remembered weight is what was measured THEN, against
--   the item defs as they were then, and re-deriving it now against
--   possibly-changed defs would reject perfectly valid historical
--   observations.
validateContainerKnowledge ∷ ContainerKnowledgeDTO → [ComponentError]
validateContainerKnowledge (ContainerKnowledgeDTO slices) = concat
    [ [ err ("page '" <> tshow (pckPageId s) <> "': container #"
             <> tshow (unBuildingId bid) <> " has " <> fault
             <> " remembered weight (" <> tshow (crdStoredWeight r) <> ")")
      | s ← slices, (bid, r) ← HM.toList (pckRecords s)
      , Just fault ← [scalarFault (crdStoredWeight r)] ]
    , [ err ("page '" <> tshow (pckPageId s) <> "': container #"
             <> tshow (unBuildingId bid) <> " has " <> fault
             <> " reveal time (" <> tshow (crdRevealedAt r) <> ")")
      | s ← slices, (bid, r) ← HM.toList (pckRecords s)
      , Just fault ← [scalarFault (crdRevealedAt r)] ]
    ]
  where err = ComponentError containerKnowledgeComponentId 1 ValidatePhase

-- | v1, and OPTIONAL — see the module header for why the absent case is
--   a legitimate default rather than a decode failure. Depends on
--   @"world-pages"@ (the page-set authority every slice is checked
--   against) and @"buildings"@ (whose restored instance set the load
--   boundary scrubs dangling records against).
containerKnowledgeCodec ∷ ComponentCodec ContainerKnowledgeDTO
containerKnowledgeCodec = componentCodec ComponentSpec
    { csComponent     = containerKnowledgeComponentId
    , csVersion       = 1
    , csRequired      = False
    , csDeps          = [worldPagesComponentId, buildingsComponentId]
    , csEncode        = \snap → ContainerKnowledgeDTO
        [ PageContainerKnowledgeDTO (pgsPageId p)
            (HM.map toContainerRecordDTO
                    (ckRecords (pgsContainerKnowledge p)))
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = validateContainerKnowledge
    }

applyContainerKnowledge
    ∷ Word32 → ContainerKnowledgeDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyContainerKnowledge ver (ContainerKnowledgeDTO slices) =
    applyPageSlices containerKnowledgeComponentId ver pckPageId
        (\s p → p { pgsContainerKnowledge =
                      ContainerKnowledge
                          (HM.map fromContainerRecordDTO (pckRecords s)) })
        slices

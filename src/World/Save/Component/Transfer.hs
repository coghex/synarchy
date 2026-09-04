{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The @"transfer-orders"@ save component (#1246, epic #1013 phase 3 /
--   slice UIT-2A): the per-page queue of durable transfer orders
--   ("Unit.Transfer.Orders"), in its own independently versioned
--   envelope component.
--
--   __This is the SECOND optional component in the static Haskell
--   registry__ (after #1087's @"container-knowledge"@; the Lua side
--   already has its own optional @lua.tutorial_progress@). The
--   justification @docs\/persistence_contract.md@ §5 requires of every
--   optional component: every save that predates this feature carries no
--   such payload, and an ABSENT payload means exactly "no orders are
--   queued" — which is TRUE of every one of those sessions rather than
--   invented, because until this slice landed there was nowhere for an
--   order to be stored at all. A page therefore keeps
--   'World.Save.Component.PageCore.blankPageSnapshot''s
--   'Unit.Transfer.Orders.emptyTransferOrders' default, allocator and
--   all. A PRESENT payload that is malformed, truncated, or encoded at a
--   version this reader does not accept remains a hard load error
--   exactly as a required component's would be: "absent" and "broken"
--   are different answers.
--
--   __The frozen-DTO boundary rule__ ("World.Save.Component.Types")
--   decides every type below. 'Unit.Transfer''s
--   'Unit.Transfer.TransferState' / 'Unit.Transfer.TransferReason' /
--   'Unit.Transfer.TransferEndpoint' / 'Unit.Transfer.TransferItemRef'
--   are LIVE policy vocabulary — the whole point of that module is that
--   the vocabulary grows as the transfer contract does — so none of them
--   is ever encoded AS ITSELF on the wire. Each is mirrored here by a
--   frozen DTO with an explicit, total field-by-field conversion, so a
--   constructor appended to the live policy enum is a deliberate,
--   visible wire event rather than a silent reinterpretation of every
--   saved order.
--
--   Those live types DO each carry a 'Data.Serialize.Serialize'
--   instance, and the distinction is the whole point rather than a
--   loophole: the instances exist only so
--   'Unit.Transfer.Orders.TransferOrders' can ride
--   'World.Save.Types.WorldPageSave', the transitional IN-MEMORY load
--   bridge, which derives 'Data.Serialize.Serialize' wholesale — the
--   same arrangement 'Craft.Bills.CraftBill',
--   'Building.Knowledge.ContainerRecord' and 'Power.Types.PowerNode'
--   already have. Nothing on the SAVE BOUNDARY reaches them: this
--   component encodes 'TransferOrdersDTO' and decodes back through it,
--   so a field or constructor added to the live vocabulary cannot move
--   a byte of a shipped payload. See "Unit.Transfer"'s own header for
--   the same split stated from the other side.
--
--   __Every durable reference an order carries is typed__
--   ("World.Save.Reference"'s 'SamePageRef'): the acting unit, both
--   endpoint identities, and each requested item instance are all
--   expected on the SAME page as the order itself. Wrong-page is a hard
--   error and a target absent from the whole session is a tolerated,
--   non-blocking diagnostic — both decided by
--   "World.Save.Integrity" over the assembled session, not here; this
--   component-local validator sees only its own DTO (see
--   'validateTransferOrders').
module World.Save.Component.Transfer
    ( transferOrdersCodec
    , applyTransferOrders
    , validateTransferOrders
      -- * Frozen wire DTOs
    , TransferOrdersDTO(..)
    , PageTransferOrdersDTO(..)
    , TransferOrderQueueDTO(..)
    , TransferOrderDTO(..)
    , QueuedTransferDTO(..)
    , TransferEndpointDTO(..)
    , TransferStateDTO(..)
    , TransferFailureDTO(..)
    , TransferReasonDTO(..)
    ) where

import UPrelude
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Transfer
    ( TransferBatch(..), TransferEndpoint(..), TransferFailure(..)
    , TransferItemRef(..), TransferReason(..), TransferState(..)
    , QueuedTransfer(..) )
import Unit.Transfer.Orders
    ( TransferOrder(..), TransferOrderId(..), TransferOrders(..) )
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Types
import World.Save.Reference (SamePageRef(..))
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))

orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId ∘ HM.elems ∘ snapPages

-- * Frozen mirrors of the live policy vocabulary ---------------------

-- | Frozen mirror of 'Unit.Transfer.TransferReason'. Positional by
--   constructor tag (cereal's 'Generic' encoding), so it is APPEND-ONLY
--   and guarded as such by @tools\/enum_append_only_audit.py@ — the
--   constructor ORDER here is the wire format and deliberately matches
--   the live enum's declaration order so the two conversions below stay
--   readable side by side.
data TransferReasonDTO
    = TrdInstanceUnspecified
    | TrdSourceMissing
    | TrdSourceIneligible
    | TrdReceiverMissing
    | TrdReceiverIneligible
    | TrdInstanceMissing
    | TrdItemNotTransferable
    | TrdOutOfRange
    | TrdReceiverFull
    | TrdBecameStale
    deriving (Show, Eq, Generic, Serialize)

toTransferReasonDTO ∷ TransferReason → TransferReasonDTO
toTransferReasonDTO r = case r of
    ReasonInstanceUnspecified → TrdInstanceUnspecified
    ReasonSourceMissing       → TrdSourceMissing
    ReasonSourceIneligible    → TrdSourceIneligible
    ReasonReceiverMissing     → TrdReceiverMissing
    ReasonReceiverIneligible  → TrdReceiverIneligible
    ReasonInstanceMissing     → TrdInstanceMissing
    ReasonItemNotTransferable → TrdItemNotTransferable
    ReasonOutOfRange          → TrdOutOfRange
    ReasonReceiverFull        → TrdReceiverFull
    ReasonBecameStale         → TrdBecameStale

fromTransferReasonDTO ∷ TransferReasonDTO → TransferReason
fromTransferReasonDTO d = case d of
    TrdInstanceUnspecified → ReasonInstanceUnspecified
    TrdSourceMissing       → ReasonSourceMissing
    TrdSourceIneligible    → ReasonSourceIneligible
    TrdReceiverMissing     → ReasonReceiverMissing
    TrdReceiverIneligible  → ReasonReceiverIneligible
    TrdInstanceMissing     → ReasonInstanceMissing
    TrdItemNotTransferable → ReasonItemNotTransferable
    TrdOutOfRange          → ReasonOutOfRange
    TrdReceiverFull        → ReasonReceiverFull
    TrdBecameStale         → ReasonBecameStale

-- | Frozen mirror of 'Unit.Transfer.TransferFailure'. 'tfdCause' is
--   populated only for a stale refusal (see the live type), and its
--   OPTIONALITY is part of what a round trip must preserve — a failure
--   that came back with a fabricated cause would misreport which part
--   of the world moved underneath the order.
data TransferFailureDTO = TransferFailureDTO
    { tfdReason ∷ !TransferReasonDTO
    , tfdCause  ∷ !(Maybe TransferReasonDTO)
    } deriving (Show, Eq, Generic, Serialize)

toTransferFailureDTO ∷ TransferFailure → TransferFailureDTO
toTransferFailureDTO f = TransferFailureDTO
    { tfdReason = toTransferReasonDTO (tfReason f)
    , tfdCause  = toTransferReasonDTO <$> tfCause f
    }

fromTransferFailureDTO ∷ TransferFailureDTO → TransferFailure
fromTransferFailureDTO d = TransferFailure
    { tfReason = fromTransferReasonDTO (tfdReason d)
    , tfCause  = fromTransferReasonDTO <$> tfdCause d
    }

-- | Frozen mirror of 'Unit.Transfer.TransferState' — the six-state
--   per-item lifecycle. Append-only, same as 'TransferReasonDTO'.
data TransferStateDTO
    = TsdQueued
    | TsdInTransit
    | TsdReadyToCommit
    | TsdCompleted
    | TsdCancelled
    | TsdFailed !TransferFailureDTO
    deriving (Show, Eq, Generic, Serialize)

toTransferStateDTO ∷ TransferState → TransferStateDTO
toTransferStateDTO s = case s of
    TransferQueued        → TsdQueued
    TransferInTransit     → TsdInTransit
    TransferReadyToCommit → TsdReadyToCommit
    TransferCompleted     → TsdCompleted
    TransferCancelled     → TsdCancelled
    TransferFailed f      → TsdFailed (toTransferFailureDTO f)

fromTransferStateDTO ∷ TransferStateDTO → TransferState
fromTransferStateDTO d = case d of
    TsdQueued          → TransferQueued
    TsdInTransit       → TransferInTransit
    TsdReadyToCommit   → TransferReadyToCommit
    TsdCompleted       → TransferCompleted
    TsdCancelled       → TransferCancelled
    TsdFailed f        → TransferFailed (fromTransferFailureDTO f)

-- | Frozen mirror of 'Unit.Transfer.TransferEndpoint'. Both arms carry
--   a 'SamePageRef': an order's endpoints are expected on the order's
--   OWN page, which is what "World.Save.Integrity" reads to decide a
--   wrong-page violation. Append-only (two constructors today, and the
--   endpoint vocabulary is expected to grow — a ground stockpile is the
--   obvious next one).
data TransferEndpointDTO
    = TedUnit     !(SamePageRef UnitId)
    | TedBuilding !(SamePageRef BuildingId)
    deriving (Show, Eq, Generic, Serialize)

toTransferEndpointDTO ∷ TransferEndpoint → TransferEndpointDTO
toTransferEndpointDTO e = case e of
    EndpointUnit     uid → TedUnit     (SamePageRef uid)
    EndpointBuilding bid → TedBuilding (SamePageRef bid)

fromTransferEndpointDTO ∷ TransferEndpointDTO → TransferEndpoint
fromTransferEndpointDTO d = case d of
    TedUnit     r → EndpointUnit     (unSamePageRef r)
    TedBuilding r → EndpointBuilding (unSamePageRef r)

-- | Frozen mirror of 'Unit.Transfer.QueuedTransfer' — one requested
--   item and its own lifecycle state, flattened so the exact instance
--   identity is a first-class typed reference rather than a field of a
--   nested live record.
--
--   'qtdInstance' is held SIGNED, exactly as
--   'Unit.Transfer.TransferItemRef.tirInstanceId' is: the live contract
--   refuses 0 (the legacy "first def-name match" convention) and refuses
--   a negative value rather than letting it wrap into a large 'Word64',
--   and a wire type that silently widened it would destroy that
--   distinction. 'qtdDefName' rides along for the same reason the live
--   ref carries it — it is cross-checked against the resolved instance,
--   so a stale row is refused rather than silently retargeted.
data QueuedTransferDTO = QueuedTransferDTO
    { qtdInstance ∷ !(SamePageRef Int64)
    , qtdDefName  ∷ !Text
    , qtdState    ∷ !TransferStateDTO
    } deriving (Show, Eq, Generic, Serialize)

toQueuedTransferDTO ∷ QueuedTransfer → QueuedTransferDTO
toQueuedTransferDTO q = QueuedTransferDTO
    { qtdInstance = SamePageRef (tirInstanceId (qtItem q))
    , qtdDefName  = tirDefName (qtItem q)
    , qtdState    = toTransferStateDTO (qtState q)
    }

fromQueuedTransferDTO ∷ QueuedTransferDTO → QueuedTransfer
fromQueuedTransferDTO d = QueuedTransfer
    { qtItem  = TransferItemRef
        { tirInstanceId = unSamePageRef (qtdInstance d)
        , tirDefName    = qtdDefName d }
    , qtState = fromTransferStateDTO (qtdState d)
    }

-- | Frozen mirror of 'Unit.Transfer.Orders.TransferOrder' plus the
--   'Unit.Transfer.TransferBatch' it carries, flattened into one wire
--   record — the batch has no identity of its own, so a separate nested
--   DTO would only add a layer without adding a boundary.
--
--   'trdEntries' is a LIST, not a map: request order is meaningful (a
--   batch that only partly fits keeps the earliest items), so it is
--   persisted positionally and restored in exactly that order.
data TransferOrderDTO = TransferOrderDTO
    { trdId          ∷ !TransferOrderId
    , trdUnit        ∷ !(SamePageRef UnitId)
    , trdSource      ∷ !TransferEndpointDTO
    , trdDestination ∷ !TransferEndpointDTO
    , trdEntries     ∷ ![QueuedTransferDTO]
    } deriving (Show, Eq, Generic, Serialize)

toTransferOrderDTO ∷ TransferOrder → TransferOrderDTO
toTransferOrderDTO o = TransferOrderDTO
    { trdId          = troId o
    , trdUnit        = SamePageRef (troUnit o)
    , trdSource      = toTransferEndpointDTO (tbSource (troBatch o))
    , trdDestination = toTransferEndpointDTO (tbDestination (troBatch o))
    , trdEntries     = map toQueuedTransferDTO (tbEntries (troBatch o))
    }

fromTransferOrderDTO ∷ TransferOrderDTO → TransferOrder
fromTransferOrderDTO d = TransferOrder
    { troId    = trdId d
    , troUnit  = unSamePageRef (trdUnit d)
    , troBatch = TransferBatch
        { tbSource      = fromTransferEndpointDTO (trdSource d)
        , tbDestination = fromTransferEndpointDTO (trdDestination d)
        , tbEntries     = map fromQueuedTransferDTO (trdEntries d)
        }
    }

-- | Frozen mirror of the 'TransferOrders' queue (orders + its embedded
--   per-page id allocator).
data TransferOrderQueueDTO = TransferOrderQueueDTO
    { toqOrders ∷ !(HM.HashMap TransferOrderId TransferOrderDTO)
    , toqNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

toTransferOrdersQueueDTO ∷ TransferOrders → TransferOrderQueueDTO
toTransferOrdersQueueDTO q = TransferOrderQueueDTO
    { toqOrders = HM.map toTransferOrderDTO (trosOrders q)
    , toqNextId = trosNextId q }

fromTransferOrdersQueueDTO ∷ TransferOrderQueueDTO → TransferOrders
fromTransferOrdersQueueDTO d = TransferOrders
    { trosOrders = HM.map fromTransferOrderDTO (toqOrders d)
    , trosNextId = toqNextId d }

-- | One page's slice, in the same shape every other page-scoped
--   component uses ('applyPageSlices' enforces the page-set contract).
data PageTransferOrdersDTO = PageTransferOrdersDTO
    { ptoPageId ∷ !WorldPageId
    , ptoOrders ∷ !TransferOrderQueueDTO
    } deriving (Show, Eq, Generic, Serialize)

newtype TransferOrdersDTO =
    TransferOrdersDTO { todPages ∷ [PageTransferOrdersDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | Component-local invariants — everything about the store's own
--   identity discipline that a decoded-from-disk payload has no
--   structural guarantee of (mirrors
--   'World.Save.Component.EntitySystems.validateCraftBills', which enforces
--   the identical rules for the per-page bill allocator):
--
--     * the allocator is at least 1, so 'Unit.Transfer.Orders' can never
--       mint id 0 after a load — 0 is the reserved "no order" value;
--     * no order carries id 0, for the same reason;
--     * every order's id sits BELOW the page's allocator, so the next
--       programmatic creation cannot reuse a restored order's id. Note
--       this is what makes a SATURATED allocator ('maxBound') safe to
--       accept rather than reject: it is the legitimate terminal state
--       'Unit.Transfer.Orders.transferOrderAllocatorExhausted' names —
--       every stored id is strictly below it, and
--       'Unit.Transfer.Orders.addTransferOrder' refuses to issue another
--       rather than wrapping — so refusing the payload would reject a
--       perfectly consistent save instead of catching corruption;
--     * the map KEY and the DTO's own embedded 'trdId' agree. They are
--       two independent copies of one identity, kept in sync by
--       construction in the live store but not on the wire, and a
--       payload where they disagree would leave lookup-by-key and
--       lookup-by-field naming different orders.
--
--   A literal duplicate key within one page's map is structurally
--   impossible once decoded (a 'HashMap' cannot hold two entries under
--   one key), so there is nothing further to check there.
--
--   Deliberately OUT OF SCOPE, exactly as for craft bills: whether an
--   order's acting unit / endpoints / item instances still EXIST. This
--   validator sees one component's own DTO, never the assembled
--   cross-component picture; a dangling target is tolerated gameplay
--   (the carrier died, the destination was demolished) reported as a
--   non-blocking diagnostic by
--   'World.Save.Integrity.sessionIntegrityWarnings', and a WRONG-PAGE
--   target is the hard error 'World.Save.Integrity.sessionIntegrityErrors'
--   raises once the whole session is assembled.
validateTransferOrders ∷ TransferOrdersDTO → [ComponentError]
validateTransferOrders (TransferOrdersDTO slices) = concat
    [ [ err ("page '" <> tshow (ptoPageId s) <> "': transfer-order \
             \allocator is 0, which would mint the reserved \"no order\" \
             \id 1 steps later")
      | s ← slices, toqNextId (ptoOrders s) ≡ 0 ]
    , [ err ("page '" <> tshow (ptoPageId s) <> "': transfer order #0 \
             \is present, but 0 is the reserved \"no order\" id and is \
             \never allocated")
      | s ← slices, oid ← HM.keys (toqOrders (ptoOrders s))
      , unTransferOrderId oid ≡ 0 ]
    , [ err ("page '" <> tshow (ptoPageId s) <> "': transfer order #"
             <> tshow (unTransferOrderId oid) <> " is not below the \
                \page's order allocator ("
             <> tshow (toqNextId (ptoOrders s)) <> ")")
      | s ← slices, oid ← HM.keys (toqOrders (ptoOrders s))
      , unTransferOrderId oid ≥ toqNextId (ptoOrders s) ]
    , [ err ("page '" <> tshow (ptoPageId s) <> "': transfer-order map \
             \key #" <> tshow (unTransferOrderId k)
             <> " holds an order whose own id is #"
             <> tshow (unTransferOrderId (trdId v)))
      | s ← slices, (k, v) ← HM.toList (toqOrders (ptoOrders s))
      , k ≢ trdId v ]
    ]
  where err = ComponentError transferOrdersComponentId 1 ValidatePhase

-- | v1, and OPTIONAL — see the module header for the justification
--   @docs\/persistence_contract.md@ §5 requires. Depends on
--   @"world-pages"@ (the page-set authority every slice is checked
--   against) plus @"buildings"@ and @"units"@, whose restored instance
--   sets are what an order's endpoints and acting unit resolve against.
transferOrdersCodec ∷ ComponentCodec TransferOrdersDTO
transferOrdersCodec = componentCodec ComponentSpec
    { csComponent     = transferOrdersComponentId
    , csVersion       = 1
    , csRequired      = False
    , csDeps          = [ worldPagesComponentId, buildingsComponentId
                        , unitsComponentId ]
    , csEncode        = \snap → TransferOrdersDTO
        [ PageTransferOrdersDTO (pgsPageId p)
            (toTransferOrdersQueueDTO (pgsTransferOrders p))
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = validateTransferOrders
    }

applyTransferOrders
    ∷ Word32 → TransferOrdersDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyTransferOrders ver (TransferOrdersDTO slices) =
    applyPageSlices transferOrdersComponentId ver ptoPageId
        (\s p → p { pgsTransferOrders =
                      fromTransferOrdersQueueDTO (ptoOrders s) })
        slices

{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The player-managed transfer contract (#1000 phase A1, generalized
--   by #1085 phase A2 of epic #1013): one policy for "may these exact
--   item instances move from this endpoint to that endpoint, and what
--   happens when they do".
--
--   Pure and EngineEnv-free on purpose. The headless suite exercises
--   the whole policy directly (the same shape 'Craft.Execute' uses for
--   craft consumption), while the Lua verbs in
--   "Engine.Scripting.Lua.API.Units.Transfer" project the live
--   managers into a 'TransferScene' and apply the resulting
--   'TransferCommit's. Every later surface — C1's paired inventory
--   panel, C2's walk-then-commit — is expected to reach this module
--   through those verbs rather than re-deriving eligibility, proximity
--   or capacity of its own.
--
--   A2's generalization, in one paragraph: an endpoint is a unit
--   inventory OR a built building's loose storage, on BOTH sides, so
--   all four pairs are expressible and the direction is DERIVED from
--   the pair rather than carried as an independent operation field. A
--   request names an ORDERED LIST of item identities rather than one
--   instance and a quantity; the list is one order and (later) one
--   trip, but every item keeps its own six-state lifecycle entry, so a
--   batch that only partly fits reports per-item outcomes instead of
--   an all-or-nothing verdict. There is deliberately NO batch-wide
--   transaction: each item still moves through the same single-item
--   atomic path A1 shipped.
--
--   __On the 'Data.Serialize.Serialize' instances below__ (#1246): the
--   seven types a durable transfer ORDER carries — 'TransferEndpoint',
--   'TransferItemRef', 'TransferReason', 'TransferFailure',
--   'TransferState', 'QueuedTransfer' and 'TransferBatch' — are
--   serializable ONLY so 'Unit.Transfer.Orders.TransferOrders' can ride
--   'World.Save.Types.WorldPageSave', the transitional IN-MEMORY load
--   bridge (which derives 'Data.Serialize.Serialize' wholesale). That is
--   the same arrangement every other per-page gameplay layer on that
--   record already has — 'Craft.Bills.CraftBill',
--   'Building.Knowledge.ContainerRecord', 'Power.Types.PowerNode'.
--   Do not conflate that seven with the six one paragraph above: this is
--   a count of TYPES that derive 'Data.Serialize.Serialize', while the
--   six-state lifecycle is 'TransferState''s own CONSTRUCTOR count.
--
--   These instances are NOT the save WIRE format and must never be used
--   as one. The wire schema is
--   "World.Save.Component.Transfer"'s frozen DTO mirror
--   ('World.Save.Component.Transfer.TransferOrderDTO' and friends), with
--   an explicit field-by-field conversion, precisely so this policy
--   vocabulary can keep growing — which it is expected to — without
--   silently reinterpreting bytes already on disk. Adding a constructor
--   here is a deliberate, visible wire event: the frozen DTO must gain
--   the matching case, and @tools\/enum_append_only_audit.py@ guards the
--   constructor ORDER of both halves.
--
--   This module is ADDITIVE. The pre-existing verbs
--   ('unitTransferItemToUnitFn', 'unitDepositToCargoFn') keep their
--   documented semantics — no capacity check for unit-to-unit, no
--   adjacency check at all — because the AI fetch/repair/medic paths
--   depend on exactly that laxity. The stricter preconditions here
--   apply to the new transfer path only.
module Unit.Transfer
    ( -- * Identity
      TransferEndpoint(..)
    , TransferEndpointKind(..)
    , endpointKindOf
    , transferEndpointKindId
    , allTransferEndpointKinds
    , TransferItemRef(..)
    , TransferRequest(..)
      -- * Structured outcomes
    , TransferReason(..)
    , TransferFailure(..)
    , transferReasonId
    , transferReasonFromId
    , allTransferReasons
    , requestFailure
    , staleFailure
    , TransferRequestError(..)
    , transferRequestErrorId
    , allTransferRequestErrors
    , TransferCompletion(..)
    , transferCompletionId
      -- * Per-item queue state
    , TransferState(..)
    , QueuedTransfer(..)
    , TransferBatch(..)
    , transferStateId
    , allTransferStateIds
    , isTerminalState
      -- * The scene the policy is evaluated against
    , UnitEndpointView(..)
    , BuildingEndpointView(..)
    , TransferEndpointView(..)
    , TransferScene(..)
    , endpointEligible
    , endpointLooseItems
    , endpointCapacity
    , endpointLoad
    , endpointPage
    , endpointRect
      -- * Per-item policy
    , TransferPlan(..)
    , TransferCommit(..)
    , ReachPolicy(..)
    , validateBatch
    , planItem
    , planItemWith
    , commitItem
    , commitItemWith
      -- * Batch lifecycle
    , checkBatch
    , checkBatchWith
    , markBatchInTransit
    , markBatchReadyToCommit
    , cancelBatch
    , failPendingBatch
    , commitBatch
    , batchTerminal
    , batchHasQueued
    , batchQueuedCount
    , batchCompletedCount
    , checkCompletion
    , commitCompletion
    ) where

import UPrelude
import Data.Int (Int64)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Building.Types (BuildingId(..), footprintDistBetween)
import Item.Types (ItemInstance(..), itemMatches)
import Unit.Types.Manager (UnitId(..))
import World.Page.Types (WorldPageId(..))

-- | One end of a transfer. The SAME type on both sides: the direction
--   a request describes is derived from the pair, never supplied
--   independently, which is why A1's @TransferOperation@ (and its
--   @operation_mismatch@ refusal) no longer exist — with nothing to
--   disagree with, a mismatch has no reachable meaning.
data TransferEndpoint
    = EndpointUnit     !UnitId
      -- ^ The unit's loose @uiInventory@. Equipment slots and
      --   accessories are never moved (see 'uevEquipped').
    | EndpointBuilding !BuildingId
      -- ^ A built building's @biStorage@. NEVER
      --   @biMaterialsDelivered@: that is locked construction stock
      --   recovered on deconstruct, which 'unitTransferItemToBuildingFn'
      --   serves as a separate operation.
    deriving (Show, Eq, Generic, Serialize)

-- | The kind half of an endpoint, without its id — the vocabulary the
--   Lua handshake advertises so a caller can name an endpoint without
--   hardcoding a string the engine might rename.
data TransferEndpointKind
    = EndpointKindUnit
    | EndpointKindBuilding
    deriving (Show, Eq, Ord, Enum, Bounded)

endpointKindOf ∷ TransferEndpoint → TransferEndpointKind
endpointKindOf (EndpointUnit _)     = EndpointKindUnit
endpointKindOf (EndpointBuilding _) = EndpointKindBuilding

transferEndpointKindId ∷ TransferEndpointKind → Text
transferEndpointKindId EndpointKindUnit     = "unit"
transferEndpointKindId EndpointKindBuilding = "building"

allTransferEndpointKinds ∷ [TransferEndpointKind]
allTransferEndpointKinds = [minBound .. maxBound]

-- | One requested item. A def name alone is deliberately insufficient
--   identity: 'tirInstanceId' names the exact physical item, so a
--   merged inventory row holding two same-def instances can never move
--   the wrong one, and 'tirDefName' is cross-checked against the
--   resolved instance so a stale UI row is refused rather than
--   silently retargeted.
data TransferItemRef = TransferItemRef
    { tirInstanceId ∷ !Int64
      -- ^ The source instance's @iiInstanceId@, held SIGNED. Must be
      --   > 0: 0 is the legacy "first def-name match" convention the AI
      --   verbs accept and this contract does not, and a negative value
      --   is a caller bug that must refuse rather than wrap into a
      --   large positive 'Word64'.
    , tirDefName    ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

-- | One transfer order: an endpoint pair plus an ORDERED batch of item
--   identities. Order is meaningful — outcomes are reported in it, and
--   a batch that only partly fits keeps the earliest items.
data TransferRequest = TransferRequest
    { trSource      ∷ !TransferEndpoint
    , trDestination ∷ !TransferEndpoint
    , trItems       ∷ ![TransferItemRef]
    } deriving (Show, Eq)

-- | Why ONE item was refused. One value per precondition, plus
--   'ReasonBecameStale' for an item that passed its create-time checks
--   and failed revalidation at commit.
data TransferReason
    = ReasonInstanceUnspecified
    | ReasonSourceMissing
    | ReasonSourceIneligible
    | ReasonReceiverMissing
    | ReasonReceiverIneligible
    | ReasonInstanceMissing
    | ReasonItemNotTransferable
    | ReasonOutOfRange
    | ReasonReceiverFull
    | ReasonBecameStale
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize)

transferReasonId ∷ TransferReason → Text
transferReasonId r = case r of
    ReasonInstanceUnspecified → "instance_unspecified"
    ReasonSourceMissing       → "source_missing"
    ReasonSourceIneligible    → "source_ineligible"
    ReasonReceiverMissing     → "receiver_missing"
    ReasonReceiverIneligible  → "receiver_ineligible"
    ReasonInstanceMissing     → "instance_missing"
    ReasonItemNotTransferable → "item_not_transferable"
    ReasonOutOfRange          → "out_of_range"
    ReasonReceiverFull        → "receiver_full"
    ReasonBecameStale         → "became_stale"

allTransferReasons ∷ [TransferReason]
allTransferReasons = [minBound .. maxBound]

-- | The inverse of 'transferReasonId', derived from the enumeration
--   itself rather than from a second hand-written table — so a reason
--   added later is parseable the moment it is spellable, and the two
--   directions cannot disagree. 'Nothing' for anything that is not one
--   of the ten ids.
--
--   Exists for the callers that RECEIVE a reason from outside the
--   engine — @unit.failTransferOrder@ (#1247) takes the retirement
--   reason and its optional cause as these ids, so the unit job names
--   a refusal in the same vocabulary every other surface reports one
--   in instead of the engine inventing a private mapping for it.
transferReasonFromId ∷ Text → Maybe TransferReason
transferReasonFromId t =
    lookup t [(transferReasonId r, r) | r ← allTransferReasons]

-- | Why the WHOLE request was rejected before any item was checked or
--   moved. Deliberately a separate type from 'TransferReason': these
--   produce no per-item outcomes and perform no mutation, so a caller
--   that folded them into the per-item vocabulary would have to invent
--   outcomes that never existed.
data TransferRequestError
    = ErrEmptyBatch
    | ErrDuplicateInstance
    deriving (Show, Eq, Ord, Enum, Bounded)

transferRequestErrorId ∷ TransferRequestError → Text
transferRequestErrorId ErrEmptyBatch        = "empty_batch"
transferRequestErrorId ErrDuplicateInstance = "duplicate_instance"

allTransferRequestErrors ∷ [TransferRequestError]
allTransferRequestErrors = [minBound .. maxBound]

-- | How much of a batch got through. Later phases (C5/D1) map these
--   onto distinct player-facing warnings — "took what it could, some
--   remain" reads differently from "nothing moved at all" — so the
--   distinction is made here rather than left to each caller's own
--   arithmetic.
data TransferCompletion
    = CompletionNone
    | CompletionPartial
    | CompletionAll
    deriving (Show, Eq, Ord, Enum, Bounded)

transferCompletionId ∷ TransferCompletion → Text
transferCompletionId CompletionNone    = "none"
transferCompletionId CompletionPartial = "partial"
transferCompletionId CompletionAll     = "all"

-- | @completionOf succeeded requested@.
completionOf ∷ Int → Int → TransferCompletion
completionOf succeeded requested
    | requested ≤ 0          = CompletionNone
    | succeeded ≤ 0          = CompletionNone
    | succeeded ≥ requested  = CompletionAll
    | otherwise              = CompletionPartial

-- | A refusal. 'tfCause' is populated only for 'ReasonBecameStale',
--   naming the precondition that broke between request and commit —
--   D1 needs both "your queued transfer went stale" and which part of
--   the world moved underneath it.
data TransferFailure = TransferFailure
    { tfReason ∷ !TransferReason
    , tfCause  ∷ !(Maybe TransferReason)
    } deriving (Show, Eq, Generic, Serialize)

-- | A refusal raised while the request was being created.
requestFailure ∷ TransferReason → TransferFailure
requestFailure r = TransferFailure { tfReason = r, tfCause = Nothing }

-- | A refusal raised at commit for an item that had already passed.
staleFailure ∷ TransferReason → TransferFailure
staleFailure r =
    TransferFailure { tfReason = ReasonBecameStale, tfCause = Just r }

-- | Lifecycle of ONE requested item. There is deliberately no
--   aggregate batch state: a batch is terminal exactly when every item
--   entry is ('batchTerminal'), so a mixed success/refusal outcome
--   needs no third vocabulary to describe it.
data TransferState
    = TransferQueued
    | TransferInTransit
    | TransferReadyToCommit
    | TransferCompleted
    | TransferCancelled
    | TransferFailed !TransferFailure
    deriving (Show, Eq, Generic, Serialize)

transferStateId ∷ TransferState → Text
transferStateId s = case s of
    TransferQueued        → "queued"
    TransferInTransit     → "in_transit"
    TransferReadyToCommit → "ready_to_commit"
    TransferCompleted     → "completed"
    TransferCancelled     → "cancelled"
    TransferFailed _      → "failed"

-- | Every state id, for the Lua-side vocabulary handshake.
allTransferStateIds ∷ [Text]
allTransferStateIds = map transferStateId
    [ TransferQueued, TransferInTransit, TransferReadyToCommit
    , TransferCompleted, TransferCancelled
    , TransferFailed (requestFailure ReasonBecameStale) ]

isTerminalState ∷ TransferState → Bool
isTerminalState s = case s of
    TransferCompleted → True
    TransferCancelled → True
    TransferFailed _  → True
    _                 → False

isPending ∷ TransferState → Bool
isPending = not . isTerminalState

-- | One requested item and its own lifecycle state.
data QueuedTransfer = QueuedTransfer
    { qtItem  ∷ !TransferItemRef
    , qtState ∷ !TransferState
    } deriving (Show, Eq, Generic, Serialize)

-- | One transfer order: the endpoint pair it was created against, plus
--   one entry per requested item IN REQUEST ORDER.
data TransferBatch = TransferBatch
    { tbSource      ∷ !TransferEndpoint
    , tbDestination ∷ !TransferEndpoint
    , tbEntries     ∷ ![QueuedTransfer]
    } deriving (Show, Eq, Generic, Serialize)

-- | What the policy needs to know about a unit endpoint. Projected
--   from a live 'Unit.Types.UnitInstance'.
data UnitEndpointView = UnitEndpointView
    { uevPage          ∷ !WorldPageId
    , uevTile          ∷ !(Int, Int)
      -- ^ @(floor gx, floor gy)@ — the same tile craft.executeAt and
      --   the Store menu measure from.
    , uevCommandable   ∷ !Bool
      -- ^ 'Unit.Faction.isPlayerCommandable' of the unit's faction.
      --   Required in BOTH roles: A2 deleted the @transfer_receiver@
      --   data marker, and faction is what replaced it.
    , uevCapacity      ∷ !Float
      -- ^ Modifier-applied @carrying_capacity@. 0 = the unit has no
      --   such stat, which reads as no room at all.
    , uevInventory     ∷ ![ItemInstance]
    , uevEquipped      ∷ ![ItemInstance]
      -- ^ Equipment-slot items + accessories. Consulted ONLY to tell
      --   "worn, therefore not transferable" apart from "no such
      --   instance anywhere"; nothing is ever moved out of it, and a
      --   building endpoint has no equivalent at all.
    , uevCarriedWeight ∷ !Float
      -- ^ The unit.getCarryingWeight measure: inventory + equipment +
      --   accessories at full recursive instance weight.
    } deriving (Show, Eq)

-- | What the policy needs to know about a building endpoint.
data BuildingEndpointView = BuildingEndpointView
    { bevPage         ∷ !WorldPageId
    , bevAnchor       ∷ !(Int, Int)
    , bevTileSize     ∷ !(Int, Int)
    , bevBuilt        ∷ !Bool
      -- ^ @currentActivity now inst def ≡ Built@.
    , bevCapacity     ∷ !Float          -- ^ @bdStorageCapacity@ (kg).
    , bevStorage      ∷ ![ItemInstance] -- ^ @biStorage@.
    , bevStoredWeight ∷ !Float
      -- ^ Σ full instance weight already stored, by the same recursive
      --   measure depositToCargo uses.
    } deriving (Show, Eq)

-- | An endpoint as the policy sees it. ONE type for both roles: a
--   unit is eligible (in either role) exactly when it is
--   player-commandable and a building exactly when it is Built with
--   positive storage capacity, so a second role-specific view would be
--   two copies of one rule.
data TransferEndpointView
    = UnitEndpointAt     !UnitEndpointView
    | BuildingEndpointAt !BuildingEndpointView
    deriving (Show, Eq)

-- | Everything the policy reads, captured at one instant. 'tscWeigh'
--   is 'Item.Types.itemTotalWeight' partially applied to the live
--   ItemManager, so fill and nested contents count exactly as they do
--   for the existing deposit gate.
data TransferScene = TransferScene
    { tscSource      ∷ !(Maybe TransferEndpointView)
    , tscDestination ∷ !(Maybe TransferEndpointView)
    , tscWeigh       ∷ !(ItemInstance → Float)
    }

-- | The exact instance a validated item would move, and where it sits
--   in the source's loose list.
data TransferPlan = TransferPlan
    { tpItem  ∷ !ItemInstance
    , tpIndex ∷ !Int
      -- ^ Original index. A rollback splices the instance back HERE —
      --   loose-storage order is gameplay/UI-visible, so "unchanged"
      --   means order-preserving, not merely same-multiset.
    } deriving (Show, Eq)

-- | The post-move lists for ONE item. The caller writes both (or
--   neither) — there is no batch-wide prepare phase.
data TransferCommit = TransferCommit
    { tcItem             ∷ !ItemInstance
    , tcIndex            ∷ !Int
    , tcSourceItems      ∷ ![ItemInstance]
    , tcDestinationItems ∷ ![ItemInstance]
      -- ^ Building storage prepends and unit inventory appends,
      --   matching depositToCargo and transferItemToUnit respectively.
    } deriving (Show, Eq)

-- * Endpoint accessors — the ONE place each role-independent property
--   of an endpoint is read, so the two arms cannot drift.

endpointPage ∷ TransferEndpointView → WorldPageId
endpointPage (UnitEndpointAt u)     = uevPage u
endpointPage (BuildingEndpointAt b) = bevPage b

-- | Anchor + tile size. A unit occupies exactly one tile, which IS a
--   1x1 footprint, so unit↔unit, unit↔building and building↔building
--   proximity all reduce to one rect-to-rect measure.
endpointRect ∷ TransferEndpointView → ((Int, Int), (Int, Int))
endpointRect (UnitEndpointAt u)     = (uevTile u, (1, 1))
endpointRect (BuildingEndpointAt b) = (bevAnchor b, bevTileSize b)

-- | The loose list an item is drawn FROM as a source and lands IN as a
--   destination. Never a unit's worn gear, never a building's locked
--   construction stock.
endpointLooseItems ∷ TransferEndpointView → [ItemInstance]
endpointLooseItems (UnitEndpointAt u)     = uevInventory u
endpointLooseItems (BuildingEndpointAt b) = bevStorage b

-- | Items present but NOT transferable. Empty for a building: loose
--   storage is all a building has.
endpointHeldItems ∷ TransferEndpointView → [ItemInstance]
endpointHeldItems (UnitEndpointAt u)     = uevEquipped u
endpointHeldItems (BuildingEndpointAt _) = []

endpointCapacity ∷ TransferEndpointView → Float
endpointCapacity (UnitEndpointAt u)     = uevCapacity u
endpointCapacity (BuildingEndpointAt b) = bevCapacity b

-- | Weight already carried/stored, by the recursive measure.
endpointLoad ∷ TransferEndpointView → Float
endpointLoad (UnitEndpointAt u)     = uevCarriedWeight u
endpointLoad (BuildingEndpointAt b) = bevStoredWeight b

-- | May this endpoint take part in a transfer at all, in either role?
--   A unit must be player-commandable (A2 replaced the deleted
--   @transfer_receiver@ marker with faction); a building must be Built
--   with positive storage capacity.
endpointEligible ∷ TransferEndpointView → Bool
endpointEligible (UnitEndpointAt u)     = uevCommandable u
endpointEligible (BuildingEndpointAt b) = bevBuilt b ∧ bevCapacity b > 0

-- | Where a received item lands. Building storage prepends, unit
--   inventory appends — the orders depositToCargo and
--   transferItemToUnit already produce.
endpointReceive ∷ TransferEndpointView → ItemInstance → [ItemInstance]
endpointReceive (UnitEndpointAt u)     item = uevInventory u ⧺ [item]
endpointReceive (BuildingEndpointAt b) item = item : bevStorage b

withLooseItems ∷ [ItemInstance] → Float → TransferEndpointView
               → TransferEndpointView
withLooseItems items load (UnitEndpointAt u) =
    UnitEndpointAt u { uevInventory = items, uevCarriedWeight = load }
withLooseItems items load (BuildingEndpointAt b) =
    BuildingEndpointAt b { bevStorage = items, bevStoredWeight = load }

-- * Whole-request validation

-- | The two failures that reject an ENTIRE request before any item is
--   checked or moved. The duplicate scan runs over the parsed identity
--   list, ahead of every per-item validity check — so two entries
--   sharing one invalid id (two zeros, the same negative twice) are
--   one 'ErrDuplicateInstance', not two per-item refusals.
--
--   This list is exhaustive. Endpoint-level failures (missing,
--   ineligible, cross-page, out of range, self-transfer) are ORDINARY
--   per-item refusals repeated across every requested item, not a
--   third whole-request category.
validateBatch ∷ TransferRequest → Either TransferRequestError [TransferItemRef]
validateBatch req
    | null items                              = Left ErrEmptyBatch
    | hasDuplicate (map tirInstanceId items)  = Left ErrDuplicateInstance
    | otherwise                               = Right items
  where
    items = trItems req

hasDuplicate ∷ [Int64] → Bool
hasDuplicate = go []
  where
    go _    []       = False
    go seen (x : xs) = x `elem` seen ∨ go (x : seen) xs

-- * Per-item policy

-- | Whether an endpoint pair must ALREADY be adjacent for a request to
--   be valid (#1247).
--
--   'ReachRequired' is the original and still the default everywhere an
--   item is about to MOVE: an immediate transfer, and the arrival
--   commit of a queued order. Every existing caller keeps it, so
--   @unit.checkTransfer@/@unit.commitTransfer@ answer exactly as they
--   always have.
--
--   @ReachDeferred carrierPage@ exists for exactly one caller — creating
--   a durable transfer ORDER, whose entire premise is that the endpoints
--   are NOT adjacent yet and a unit is about to walk between them.
--   Without it an order at a distance is unexpressible: 'planItem'
--   refuses on range BEFORE it ever weighs the item, so every legitimate
--   remote order would come back @out_of_range@ and the create-time
--   capacity gate (the whole point of refusing a doomed trip before the
--   walk) would never run at all.
--
--   It relaxes ADJACENCY and nothing else. It carries the page the
--   CARRIER walks from, and requires BOTH endpoints to be on it — which
--   is strictly stronger than the same-page-as-each-other rule
--   'ReachRequired' enforces, and deliberately so:
--
--   * A unit cannot walk to another world page, so an order whose
--     endpoints sit on one could never complete and must refuse at
--     creation rather than stall forever afterwards.
--   * The acting unit is recorded ALONGSIDE the endpoint pair
--     ('Unit.Transfer.Orders.TransferOrder'), so "the endpoints agree
--     with each other" does not imply "the carrier is there too" — a
--     third page is expressible. That combination is not merely
--     unwalkable, it is CORRUPTING: an order lives in its page's store,
--     and "World.Save.Integrity" scopes the acting unit and both
--     endpoints to that page as BLOCKING @wrong-page@ errors. Accepting
--     one would poison every later save of the session, so the carrier's
--     page is a precondition of creation rather than something checked
--     afterwards.
data ReachPolicy
    = ReachRequired
    | ReachDeferred !WorldPageId
    deriving (Show, Eq, Ord)

-- | Validate every precondition for ONE item, requiring adjacency.
planItem ∷ TransferScene → TransferEndpoint → TransferEndpoint
         → TransferItemRef → Either TransferFailure TransferPlan
planItem = planItemWith ReachRequired

-- | Validate every precondition for ONE item under an explicit
--   'ReachPolicy'. Check order is fixed so the reason a given broken
--   world produces is deterministic.
planItemWith ∷ ReachPolicy → TransferScene → TransferEndpoint
             → TransferEndpoint → TransferItemRef
             → Either TransferFailure TransferPlan
planItemWith policy scene from to ref = do
    when (tirInstanceId ref ≤ 0) $ Left (requestFailure ReasonInstanceUnspecified)
    src ← note ReasonSourceMissing (tscSource scene)
    dst ← note ReasonReceiverMissing (tscDestination scene)
    -- Identity, not state: an endpoint is never its own container.
    when (from ≡ to) $ Left (requestFailure ReasonReceiverIneligible)
    unless (endpointEligible src) $ Left (requestFailure ReasonSourceIneligible)
    unless (endpointEligible dst) $ Left (requestFailure ReasonReceiverIneligible)
    plan ← resolveInstance ref src
    unless (reachable policy src dst) $
        Left (requestFailure ReasonOutOfRange)
    unless (fits scene dst (tpItem plan)) $
        Left (requestFailure ReasonReceiverFull)
    pure plan
  where
    note r = maybe (Left (requestFailure r)) Right

-- | Can an item get from @src@ to @dst@ under this policy?
--
--   'ReachRequired' is the original rule: same page as each other, and
--   already adjacent. @ReachDeferred p@ drops adjacency and pins BOTH
--   endpoints to @p@ — the carrier's own page — which also implies they
--   agree with each other. See 'ReachPolicy' for why the carrier's page
--   is the one that matters.
reachable ∷ ReachPolicy → TransferEndpointView → TransferEndpointView → Bool
reachable ReachRequired src dst =
    endpointPage src ≡ endpointPage dst ∧ withinReach src dst
reachable (ReachDeferred carrierPage) src dst =
    endpointPage src ≡ carrierPage ∧ endpointPage dst ≡ carrierPage

-- | Validate ONE item and compute both post-move lists, requiring
--   adjacency. Used directly for an immediate transfer; 'commitBatch'
--   folds it over a queued order's ready entries.
commitItem ∷ TransferScene → TransferEndpoint → TransferEndpoint
           → TransferItemRef → Either TransferFailure TransferCommit
commitItem = commitItemWith ReachRequired

-- | 'commitItem' under an explicit 'ReachPolicy'.
commitItemWith ∷ ReachPolicy → TransferScene → TransferEndpoint
               → TransferEndpoint → TransferItemRef
               → Either TransferFailure TransferCommit
commitItemWith policy scene from to ref = do
    plan ← planItemWith policy scene from to ref
    src  ← maybe (Left (requestFailure ReasonSourceMissing)) Right
                 (tscSource scene)
    dst  ← maybe (Left (requestFailure ReasonReceiverMissing)) Right
                 (tscDestination scene)
    let item = tpItem plan
        ix   = tpIndex plan
    pure TransferCommit
        { tcItem             = item
        , tcIndex            = ix
        , tcSourceItems      = removeAt ix (endpointLooseItems src)
        , tcDestinationItems = endpointReceive dst item
        }

-- | Advance a scene as if this commit had been applied. The ordered
--   initial check threads it between items, which is what makes
--   capacity remeasured after each provisionally accepted item — twelve
--   requested items with room for eight queue the FIRST eight.
applyTransferCommit ∷ TransferCommit → TransferScene → TransferScene
applyTransferCommit c scene =
    let w = tscWeigh scene (tcItem c)
    in scene
        { tscSource = (\v → withLooseItems (tcSourceItems c)
                                           (endpointLoad v - w) v)
                        ⊚ tscSource scene
        , tscDestination = (\v → withLooseItems (tcDestinationItems c)
                                                (endpointLoad v + w) v)
                        ⊚ tscDestination scene
        }

-- | Resolve the named instance in the source's LOOSE list. An id that
--   resolves to worn gear instead is refused as not transferable
--   rather than as missing, so the player gets the accurate reason.
--
--   The @(instanceId, defName)@ identity contract is validated FIRST,
--   in both lists (#1273). 'itemMatches' deliberately ignores the name
--   for a positive id (#67's exact-instance rule), so a stale pair
--   whose id resolves to a DIFFERENT def is a bad reference wherever
--   that instance sits — 'ReasonInstanceMissing', never a "worn"
--   answer asserting the requested item exists. Only a correctly named
--   held instance earns the location-specific reason.
resolveInstance ∷ TransferItemRef → TransferEndpointView
                → Either TransferFailure TransferPlan
resolveInstance ref src =
    case findIx matches (endpointLooseItems src) of
        Just (item, ix)
            | named item → Right TransferPlan { tpItem = item, tpIndex = ix }
            | otherwise  → Left (requestFailure ReasonInstanceMissing)
        Nothing
            | any (\it → matches it ∧ named it) (endpointHeldItems src) →
                Left (requestFailure ReasonItemNotTransferable)
            | otherwise → Left (requestFailure ReasonInstanceMissing)
  where
    -- Validated > 0 by planItem before this runs, so the widening is
    -- lossless and can never wrap.
    matches  = itemMatches (fromIntegral (tirInstanceId ref)) (tirDefName ref)
    named it = iiDefName it ≡ tirDefName ref

-- | Chebyshev ≤ 1 between the two endpoints' occupied rectangles: 0 =
--   overlapping, 1 = adjacent incl. diagonals. Deliberately NOT the
--   AI's Euclidean @mule_fetch_arrival@ 1.5 — that tunable stays
--   untouched, and a unit standing at Chebyshev 1 is inside it, so C2's
--   arrival rule can guarantee this one.
withinReach ∷ TransferEndpointView → TransferEndpointView → Bool
withinReach a b =
    let (aAnchor, aSize) = endpointRect a
        (bAnchor, bSize) = endpointRect b
    in footprintDistBetween aAnchor aSize bAnchor bSize ≤ 1

-- | Capacity. Both kinds weigh the ACTUAL instance (fill + nested
--   contents), never the def's base weight. Over capacity at a unit
--   destination is a REFUSAL, matching #920's commandPickup, even
--   though the engine elsewhere lets units carry above their cap.
fits ∷ TransferScene → TransferEndpointView → ItemInstance → Bool
fits scene dst item =
    endpointCapacity dst > 0
      ∧ endpointLoad dst + tscWeigh scene item ≤ endpointCapacity dst

-- * Batch lifecycle

-- | The create-time check for a whole order: items are processed IN
--   ORDER against a progressively updated snapshot, so an earlier
--   failure never stops a later unique item and capacity is remeasured
--   after every provisional acceptance.
--
--   Mutates nothing — the returned batch is advisory by construction,
--   exactly as A1's single-item check was, because 'commitBatch'
--   revalidates.
checkBatch ∷ TransferScene → TransferRequest
           → Either TransferRequestError TransferBatch
checkBatch = checkBatchWith ReachRequired

-- | 'checkBatch' under an explicit 'ReachPolicy'. Order creation
--   (#1247) passes @ReachDeferred carrierPage@ so the per-item CAPACITY
--   verdict is reached for endpoints that are not adjacent yet;
--   everything else about the check — request order, the progressively
--   updated scene, which entries queue — is identical.
checkBatchWith ∷ ReachPolicy → TransferScene → TransferRequest
               → Either TransferRequestError TransferBatch
checkBatchWith policy scene req = do
    items ← validateBatch req
    pure TransferBatch
        { tbSource      = trSource req
        , tbDestination = trDestination req
        , tbEntries     = go scene items
        }
  where
    go _  []         = []
    go sc (i : is) =
        case commitItemWith policy sc (trSource req) (trDestination req) i of
            Left f  → QueuedTransfer i (TransferFailed f) : go sc is
            Right c → QueuedTransfer i TransferQueued
                        : go (applyTransferCommit c sc) is

overEntries ∷ (QueuedTransfer → QueuedTransfer) → TransferBatch → TransferBatch
overEntries f b = b { tbEntries = map f (tbEntries b) }

-- | Movement started: every queued entry advances together. Entries
--   that already failed the initial check stay failed.
markBatchInTransit ∷ TransferBatch → TransferBatch
markBatchInTransit = overEntries (step TransferQueued TransferInTransit)

-- | The order arrived: every in-transit entry becomes committable.
markBatchReadyToCommit ∷ TransferBatch → TransferBatch
markBatchReadyToCommit =
    overEntries (step TransferInTransit TransferReadyToCommit)

step ∷ TransferState → TransferState → QueuedTransfer → QueuedTransfer
step from to q
    | qtState q ≡ from = q { qtState = to }
    | otherwise        = q

-- | Player or AI abandoned the order: every PENDING entry becomes
--   cancelled; already terminal entries are left exactly as they are.
--
--   Pending-ONLY is load-bearing, not tidiness (#1253, whose
--   @unit.cancelTransferOrder@ is the live caller): a cancel landing
--   after a partial commit must record six delivered and six abandoned,
--   never overwrite six real deliveries with an intent nobody had.
cancelBatch ∷ TransferBatch → TransferBatch
cancelBatch = overEntries $ \q →
    if isPending (qtState q) then q { qtState = TransferCancelled } else q

-- | The failure twin of 'cancelBatch' (#1247): every PENDING entry
--   becomes 'TransferFailed' with the SAME given failure, already
--   terminal entries are left exactly as they are.
--
--   The distinction from 'cancelBatch' is who decided. Cancellation is
--   somebody CHOOSING to abandon an order that could still have run
--   (UIT-5A's explicit cancel); this is the order self-terminating
--   because it provably cannot finish — the counterpart stopped
--   existing, or the carrier stalled short of it. Recording that as
--   'TransferCancelled' would claim an intent nobody had, and would
--   throw away the reason, which is the one thing a player asking "why
--   didn't my supplies arrive?" needs.
--
--   No vocabulary is invented for it: the caller supplies an ordinary
--   'TransferFailure', so a stall is @out_of_range@ and a vanished
--   counterpart is @became_stale@ with @source_missing@ /
--   @receiver_missing@ as its cause.
failPendingBatch ∷ TransferFailure → TransferBatch → TransferBatch
failPendingBatch failure = overEntries $ \q →
    if isPending (qtState q) then q { qtState = TransferFailed failure }
                             else q

-- | Commit at arrival: ready entries are attempted SEQUENTIALLY in
--   original request order against a scene advanced after every
--   success, so each item's preconditions are re-read against the
--   state its predecessors left behind. One item's failure neither
--   rolls back a successful sibling nor stops a later one.
--
--   ONLY 'TransferReadyToCommit' commits. A still-queued or in-transit
--   entry is left untouched with no move: 'markBatchReadyToCommit' is
--   what records that the order arrived, so committing before it would
--   let C2 hand over an item from across the map. Every terminal entry
--   is likewise inert.
commitBatch ∷ TransferScene → TransferBatch
            → (TransferBatch, [TransferCommit])
commitBatch scene batch = go scene (tbEntries batch) [] []
  where
    go _  []       es cs = (batch { tbEntries = reverse es }, reverse cs)
    go sc (q : qs) es cs
        | qtState q ≢ TransferReadyToCommit = go sc qs (q : es) cs
        | otherwise =
            case commitItem sc (tbSource batch) (tbDestination batch)
                             (qtItem q) of
                -- It passed at create time and the order arrived, so
                -- any refusal here is the world having moved
                -- underneath it.
                Left f →
                    go sc qs
                       (q { qtState = TransferFailed
                                        (staleFailure (tfReason f)) } : es)
                       cs
                Right c →
                    go (applyTransferCommit c sc) qs
                       (q { qtState = TransferCompleted } : es) (c : cs)

-- | A batch is terminal exactly when every item entry is. There is
--   deliberately no separate aggregate state to keep in sync.
batchTerminal ∷ TransferBatch → Bool
batchTerminal = all (isTerminalState . qtState) . tbEntries

-- | Is there anything to make the trip for? An order whose every item
--   failed the initial check starts no movement.
batchHasQueued ∷ TransferBatch → Bool
batchHasQueued = (> 0) . batchQueuedCount

batchQueuedCount ∷ TransferBatch → Int
batchQueuedCount = length . filter ((≡ TransferQueued) . qtState) . tbEntries

batchCompletedCount ∷ TransferBatch → Int
batchCompletedCount =
    length . filter ((≡ TransferCompleted) . qtState) . tbEntries

-- | For a create-time check: queued items against requested items.
checkCompletion ∷ TransferBatch → TransferCompletion
checkCompletion b = completionOf (batchQueuedCount b) (length (tbEntries b))

-- | For a commit: completed items against requested items.
commitCompletion ∷ TransferBatch → TransferCompletion
commitCompletion b =
    completionOf (batchCompletedCount b) (length (tbEntries b))

-- | First match with its index.
findIx ∷ (a → Bool) → [a] → Maybe (a, Int)
findIx p = go 0
  where
    go _ []     = Nothing
    go i (x:xs)
        | p x       = Just (x, i)
        | otherwise = go (i + 1) xs

removeAt ∷ Int → [a] → [a]
removeAt i xs = let (pre, post) = splitAt i xs in pre ++ drop 1 post

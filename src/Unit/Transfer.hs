{-# LANGUAGE Strict #-}
-- | The player-managed unit→container transfer contract (#1000, epic
--   #1013 phase A1): one policy for "may this exact item instance move
--   from this unit into this container, and what happens when it
--   does".
--
--   Pure and EngineEnv-free on purpose. The headless suite exercises
--   the whole policy directly (the same shape 'Craft.Execute' uses for
--   craft consumption), while the Lua verbs in
--   "Engine.Scripting.Lua.API.Units.Transfer" project the live
--   managers into a 'TransferScene' and apply the resulting
--   'TransferCommit'. Every later surface — B1's context-menu entry,
--   C1's paired inventory panel, C2's walk-then-commit — is expected
--   to reach this module through those verbs rather than re-deriving
--   eligibility, proximity, or capacity of its own.
--
--   This module is ADDITIVE. The pre-existing verbs
--   ('unitTransferItemToUnitFn', 'unitDepositToCargoFn') keep their
--   documented semantics — no capacity check for unit-to-unit, no
--   adjacency check at all — because the AI fetch/repair/medic paths
--   depend on exactly that laxity. The stricter preconditions here
--   apply to the new transfer path only.
module Unit.Transfer
    ( -- * Identity
      TransferOperation(..)
    , TransferReceiver(..)
    , TransferRequest(..)
    , transferOperationId
    , allTransferOperations
      -- * Structured outcomes
    , TransferReason(..)
    , TransferFailure(..)
    , transferReasonId
    , allTransferReasons
    , requestFailure
    , staleFailure
      -- * Queued state
    , TransferState(..)
    , QueuedTransfer(..)
    , transferStateId
    , allTransferStateIds
      -- * The scene the policy is evaluated against
    , TransferSourceView(..)
    , BuildingReceiverView(..)
    , UnitReceiverView(..)
    , TransferReceiverView(..)
    , TransferScene(..)
      -- * Policy
    , TransferPlan(..)
    , TransferCommit(..)
    , planTransfer
    , commitTransfer
      -- * Queue transitions
    , queueTransfer
    , markInTransit
    , markReadyToCommit
    , cancelTransfer
    , commitQueued
    ) where

import UPrelude
import Building.Types (BuildingId(..), footprintDistAt)
import Item.Types (ItemInstance(..), itemMatches)
import Unit.Types.Manager (UnitId(..))
import World.Page.Types (WorldPageId(..))

-- | Which container an item is being moved into. Enumerated (rather
--   than inferred from the receiver's type) so a request records the
--   operation the player asked for and a mismatch is a refusal, not a
--   silent reinterpretation.
data TransferOperation
    = ToBuildingStorage
      -- ^ unit inventory → a built building's @biStorage@. NEVER
      --   @biMaterialsDelivered@: that is locked construction stock
      --   recovered on deconstruct, which 'unitTransferItemToBuildingFn'
      --   serves as a separate operation.
    | ToUnitInventory
      -- ^ unit inventory → another unit's @uiInventory@, for units the
      --   shipped data marks as transfer receivers (the technomule
      --   first).
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Stable machine-readable id. D1 owns presentation; these never
--   become player-facing prose.
transferOperationId ∷ TransferOperation → Text
transferOperationId ToBuildingStorage = "unit_to_building_storage"
transferOperationId ToUnitInventory   = "unit_to_unit_inventory"

allTransferOperations ∷ [TransferOperation]
allTransferOperations = [minBound .. maxBound]

-- | The container an item is headed for.
data TransferReceiver
    = ReceiverBuilding !BuildingId
    | ReceiverUnit     !UnitId
    deriving (Show, Eq)

-- | A single transfer's identity. A def name alone is deliberately
--   insufficient: 'trInstanceId' names the exact physical item, so a
--   merged inventory row holding two same-def instances can never move
--   the wrong one.
data TransferRequest = TransferRequest
    { trSource     ∷ !UnitId
    , trReceiver   ∷ !TransferReceiver
    , trInstanceId ∷ !Word64
      -- ^ The source instance's 'iiInstanceId'. Must be > 0 — 0 is the
      --   legacy "first def-name match" convention the AI verbs accept
      --   and this contract does not.
    , trDefName    ∷ !Text
      -- ^ Expected definition of that instance. Cross-checked against
      --   the resolved instance so a request built against a stale UI
      --   row is refused rather than silently retargeted.
    , trQuantity   ∷ !Int
      -- ^ Expected quantity. An 'ItemInstance' is one physical item, so
      --   the only supported value is 1; anything else is refused
      --   rather than rounded.
    , trOperation  ∷ !TransferOperation
    } deriving (Show, Eq)

-- | Why a transfer was refused. One value per precondition, plus
--   'ReasonBecameStale' for a request that passed its create-time
--   checks and failed revalidation at commit.
data TransferReason
    = ReasonQuantityUnsupported
    | ReasonInstanceUnspecified
    | ReasonSourceMissing
    | ReasonReceiverMissing
    | ReasonOperationMismatch
    | ReasonReceiverIneligible
    | ReasonInstanceMissing
    | ReasonItemNotTransferable
    | ReasonOutOfRange
    | ReasonReceiverFull
    | ReasonBecameStale
    deriving (Show, Eq, Ord, Enum, Bounded)

transferReasonId ∷ TransferReason → Text
transferReasonId r = case r of
    ReasonQuantityUnsupported → "quantity_unsupported"
    ReasonInstanceUnspecified → "instance_unspecified"
    ReasonSourceMissing       → "source_missing"
    ReasonReceiverMissing     → "receiver_missing"
    ReasonOperationMismatch   → "operation_mismatch"
    ReasonReceiverIneligible  → "receiver_ineligible"
    ReasonInstanceMissing     → "instance_missing"
    ReasonItemNotTransferable → "item_not_transferable"
    ReasonOutOfRange          → "out_of_range"
    ReasonReceiverFull        → "receiver_full"
    ReasonBecameStale         → "became_stale"

allTransferReasons ∷ [TransferReason]
allTransferReasons = [minBound .. maxBound]

-- | A refusal. 'tfCause' is populated only for 'ReasonBecameStale',
--   naming the precondition that broke between request and commit —
--   D1 needs both "your queued transfer went stale" and which part of
--   the world moved underneath it.
data TransferFailure = TransferFailure
    { tfReason ∷ !TransferReason
    , tfCause  ∷ !(Maybe TransferReason)
    } deriving (Show, Eq)

-- | A refusal raised while the request was being created.
requestFailure ∷ TransferReason → TransferFailure
requestFailure r = TransferFailure { tfReason = r, tfCause = Nothing }

-- | A refusal raised at commit for a request that had already passed.
staleFailure ∷ TransferReason → TransferFailure
staleFailure r =
    TransferFailure { tfReason = ReasonBecameStale, tfCause = Just r }

-- | Lifecycle of a queued transfer. A1 ships no producer — B1/C1/C2
--   create requests and drive the walk — so nothing here is persisted;
--   the states exist so those surfaces share one vocabulary instead of
--   inventing three.
data TransferState
    = TransferQueued
    | TransferInTransit
    | TransferReadyToCommit
    | TransferCompleted
    | TransferCancelled
    | TransferFailed !TransferFailure
    deriving (Show, Eq)

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

data QueuedTransfer = QueuedTransfer
    { qtRequest ∷ !TransferRequest
    , qtState   ∷ !TransferState
    } deriving (Show, Eq)

-- | What the policy needs to know about the source unit. Projected
--   from a live 'Unit.Types.UnitInstance'.
data TransferSourceView = TransferSourceView
    { tsvPage      ∷ !WorldPageId
    , tsvTile      ∷ !(Int, Int)
      -- ^ @(floor gx, floor gy)@ — the same tile craft.executeAt and
      --   the Store menu measure from.
    , tsvInventory ∷ ![ItemInstance]
    , tsvEquipped  ∷ ![ItemInstance]
      -- ^ Equipment-slot items + accessories. Consulted ONLY to tell
      --   "worn, therefore not transferable" apart from "no such
      --   instance anywhere"; nothing is ever moved out of it.
    } deriving (Show, Eq)

data BuildingReceiverView = BuildingReceiverView
    { brvPage         ∷ !WorldPageId
    , brvAnchor       ∷ !(Int, Int)
    , brvTileSize     ∷ !(Int, Int)
    , brvBuilt        ∷ !Bool
      -- ^ @currentActivity now inst def ≡ Built@.
    , brvCapacity     ∷ !Float          -- ^ @bdStorageCapacity@ (kg).
    , brvStorage      ∷ ![ItemInstance] -- ^ @biStorage@.
    , brvStoredWeight ∷ !Float
      -- ^ Σ full instance weight already stored, by the same recursive
      --   measure depositToCargo uses.
    } deriving (Show, Eq)

data UnitReceiverView = UnitReceiverView
    { urvPage         ∷ !WorldPageId
    , urvTile         ∷ !(Int, Int)
    , urvIsReceiver   ∷ !Bool
      -- ^ The def's @udTransferReceiver@ marker. Data-driven, never a
      --   def-name comparison.
    , urvCapacity     ∷ !Float
      -- ^ Modifier-applied @carrying_capacity@. 0 = the unit has no
      --   such stat, which reads as no room at all.
    , urvInventory    ∷ ![ItemInstance]
    , urvCarriedWeight ∷ !Float
      -- ^ The unit.getCarryingWeight measure: inventory + equipment +
      --   accessories at full recursive instance weight.
    } deriving (Show, Eq)

data TransferReceiverView
    = BuildingReceiverAt !BuildingReceiverView
    | UnitReceiverAt     !UnitReceiverView
    deriving (Show, Eq)

-- | Everything the policy reads, captured at one instant. 'tscWeigh'
--   is 'Item.Types.itemTotalWeight' partially applied to the live
--   ItemManager, so fill and nested contents count exactly as they do
--   for the existing deposit gate.
data TransferScene = TransferScene
    { tscSource   ∷ !(Maybe TransferSourceView)
    , tscReceiver ∷ !(Maybe TransferReceiverView)
    , tscWeigh    ∷ !(ItemInstance → Float)
    }

-- | The exact instance a validated request would move, and where it
--   sits in the source inventory.
data TransferPlan = TransferPlan
    { tpItem  ∷ !ItemInstance
    , tpIndex ∷ !Int
      -- ^ Original index. A rollback splices the instance back HERE —
      --   inventory order is gameplay/UI-visible, so "unchanged" means
      --   order-preserving, not merely same-multiset.
    } deriving (Show, Eq)

-- | The post-move lists. The caller writes both (or neither).
data TransferCommit = TransferCommit
    { tcItem            ∷ !ItemInstance
    , tcIndex           ∷ !Int
    , tcSourceInventory ∷ ![ItemInstance]
    , tcReceiverItems   ∷ ![ItemInstance]
      -- ^ Building storage prepends and unit inventory appends,
      --   matching depositToCargo and transferItemToUnit respectively.
    } deriving (Show, Eq)

-- | Validate every precondition. Check order is fixed so the reason a
--   given broken world produces is deterministic.
planTransfer ∷ TransferScene → TransferRequest → Either TransferFailure TransferPlan
planTransfer scene req = do
    when (trQuantity req ≠ 1) $ Left (requestFailure ReasonQuantityUnsupported)
    when (trInstanceId req ≡ 0) $ Left (requestFailure ReasonInstanceUnspecified)
    src ← note ReasonSourceMissing (tscSource scene)
    rcv ← note ReasonReceiverMissing (tscReceiver scene)
    unless (operationMatches (trOperation req) (trReceiver req) rcv) $
        Left (requestFailure ReasonOperationMismatch)
    when (selfTransfer req) $ Left (requestFailure ReasonReceiverIneligible)
    unless (receiverEligible rcv) $ Left (requestFailure ReasonReceiverIneligible)
    plan ← resolveInstance req src
    unless (samePage src rcv ∧ withinReach src rcv) $
        Left (requestFailure ReasonOutOfRange)
    unless (fits scene rcv (tpItem plan)) $
        Left (requestFailure ReasonReceiverFull)
    pure plan
  where
    note r = maybe (Left (requestFailure r)) Right

-- | Validate and compute both post-move lists. Used directly for an
--   immediate transfer; 'commitQueued' wraps it for a queued one.
commitTransfer ∷ TransferScene → TransferRequest → Either TransferFailure TransferCommit
commitTransfer scene req = do
    plan ← planTransfer scene req
    rcv  ← maybe (Left (requestFailure ReasonReceiverMissing)) Right
                 (tscReceiver scene)
    src  ← maybe (Left (requestFailure ReasonSourceMissing)) Right
                 (tscSource scene)
    let item     = tpItem plan
        ix       = tpIndex plan
        srcInv   = removeAt ix (tsvInventory src)
        received = case rcv of
            BuildingReceiverAt b → item : brvStorage b
            UnitReceiverAt     u → urvInventory u ++ [item]
    pure TransferCommit
        { tcItem            = item
        , tcIndex           = ix
        , tcSourceInventory = srcInv
        , tcReceiverItems   = received
        }

-- | Create a queued request: 'TransferQueued' when every precondition
--   holds at request time, 'TransferFailed' with the offending reason
--   otherwise.
queueTransfer ∷ TransferScene → TransferRequest → QueuedTransfer
queueTransfer scene req = QueuedTransfer
    { qtRequest = req
    , qtState   = case planTransfer scene req of
        Left f  → TransferFailed f
        Right _ → TransferQueued
    }

-- | Movement started. Only a queued request may enter transit.
markInTransit ∷ QueuedTransfer → QueuedTransfer
markInTransit q
    | qtState q ≡ TransferQueued = q { qtState = TransferInTransit }
    | otherwise                  = q

-- | The unit arrived. Only an in-transit request becomes committable.
markReadyToCommit ∷ QueuedTransfer → QueuedTransfer
markReadyToCommit q
    | qtState q ≡ TransferInTransit = q { qtState = TransferReadyToCommit }
    | otherwise                     = q

-- | Player or AI abandoned it. Terminal states stay put.
cancelTransfer ∷ QueuedTransfer → QueuedTransfer
cancelTransfer q
    | isPending (qtState q) = q { qtState = TransferCancelled }
    | otherwise             = q

-- | Revalidate immediately before commit and, when it still holds,
--   produce the move. A request that was valid when queued and is not
--   valid now fails as 'ReasonBecameStale' carrying the precondition
--   that broke — the create-time check is never trusted at commit
--   time. A request in a state that cannot commit is returned
--   untouched with no move.
commitQueued ∷ TransferScene → QueuedTransfer
             → (QueuedTransfer, Maybe TransferCommit)
commitQueued scene q
    | not (isPending (qtState q)) = (q, Nothing)
    | otherwise = case commitTransfer scene (qtRequest q) of
        Left f  → (q { qtState = TransferFailed (restage f) }, Nothing)
        Right c → (q { qtState = TransferCompleted }, Just c)
  where
    -- A request that never passed keeps its own reason; one that DID
    -- pass and broke since is reported as stale.
    restage f
        | qtState q ≡ TransferQueued
        ∨ qtState q ≡ TransferInTransit
        ∨ qtState q ≡ TransferReadyToCommit = staleFailure (tfReason f)
        | otherwise                         = f

isPending ∷ TransferState → Bool
isPending s = s ≡ TransferQueued
            ∨ s ≡ TransferInTransit
            ∨ s ≡ TransferReadyToCommit

-- | The requested operation must match the receiver the request names
--   AND the receiver actually resolved.
operationMatches ∷ TransferOperation → TransferReceiver → TransferReceiverView → Bool
operationMatches ToBuildingStorage (ReceiverBuilding _) (BuildingReceiverAt _) = True
operationMatches ToUnitInventory   (ReceiverUnit _)     (UnitReceiverAt _)     = True
operationMatches _ _ _ = False

-- | A unit is never its own container.
selfTransfer ∷ TransferRequest → Bool
selfTransfer req = case trReceiver req of
    ReceiverUnit u → u ≡ trSource req
    _              → False

receiverEligible ∷ TransferReceiverView → Bool
receiverEligible (BuildingReceiverAt b) = brvBuilt b ∧ brvCapacity b > 0
receiverEligible (UnitReceiverAt u)     = urvIsReceiver u

-- | Resolve the named instance in the source's LOOSE inventory. An id
--   that resolves to worn gear instead is refused as not transferable
--   rather than as missing, so the player gets the accurate reason.
resolveInstance ∷ TransferRequest → TransferSourceView
                → Either TransferFailure TransferPlan
resolveInstance req src =
    case findIx (itemMatches (trInstanceId req) (trDefName req))
                (tsvInventory src) of
        Just (item, ix)
            | iiDefName item ≡ trDefName req →
                Right TransferPlan { tpItem = item, tpIndex = ix }
            | otherwise → Left (requestFailure ReasonInstanceMissing)
        Nothing
            | any (itemMatches (trInstanceId req) (trDefName req))
                  (tsvEquipped src) →
                Left (requestFailure ReasonItemNotTransferable)
            | otherwise → Left (requestFailure ReasonInstanceMissing)

samePage ∷ TransferSourceView → TransferReceiverView → Bool
samePage src (BuildingReceiverAt b) = tsvPage src ≡ brvPage b
samePage src (UnitReceiverAt u)     = tsvPage src ≡ urvPage u

-- | Tile Chebyshev ≤ 1 either way: nearest footprint tile for a
--   building (the measure craft.executeAt uses), tile-to-tile for a
--   unit receiver. Deliberately NOT the AI's Euclidean
--   @mule_fetch_arrival@ 1.5 — that tunable stays untouched, and a
--   unit standing at Chebyshev 1 is inside it, so C2's arrival rule
--   can guarantee this one.
withinReach ∷ TransferSourceView → TransferReceiverView → Bool
withinReach src (BuildingReceiverAt b) =
    footprintDistAt (brvAnchor b) (brvTileSize b) (tsvTile src) ≤ 1
withinReach src (UnitReceiverAt u) =
    let (sx, sy) = tsvTile src
        (ux, uy) = urvTile u
    in max (abs (sx - ux)) (abs (sy - uy)) ≤ 1

-- | Capacity. Both kinds weigh the ACTUAL instance (fill + nested
--   contents), never the def's base weight. Over capacity at a unit
--   receiver is a REFUSAL, matching #920's commandPickup, even though
--   the engine elsewhere lets units carry above their cap.
fits ∷ TransferScene → TransferReceiverView → ItemInstance → Bool
fits scene (BuildingReceiverAt b) item =
    brvCapacity b > 0
      ∧ brvStoredWeight b + tscWeigh scene item ≤ brvCapacity b
fits scene (UnitReceiverAt u) item =
    urvCapacity u > 0
      ∧ urvCarriedWeight u + tscWeigh scene item ≤ urvCapacity u

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

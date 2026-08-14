{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies,
             GeneralizedNewtypeDeriving #-}
-- | Durable transfer ORDERS (#1246, epic #1013 phase 3 / slice UIT-2A):
--   the per-page live owner that makes "order at a distance" outlive the
--   click. 'Unit.Transfer' built the policy vocabulary — 'TransferBatch'
--   (an endpoint pair plus one 'QueuedTransfer' per requested item, in
--   request order) — but nothing stored one, so a batch existed only for
--   as long as the verb that built it.
--
--   This module is the store, and it is deliberately the SAME shape
--   'Craft.Bills' already established for a per-page job layer: a
--   'HashMap' keyed by an id of its own, the allocator living INSIDE the
--   record so it persists with the orders, and every transition pure so
--   the headless suite exercises them directly while the live engine
--   wraps each one in a single @atomicModifyIORef'@ on the world's
--   @wsTransferOrdersRef@.
--
--   __What is deliberately NOT here.__ Nothing in this slice creates an
--   order from a player gesture (UIT-2C) or executes one as a unit job
--   (UIT-2B); 'addTransferOrder' is the minimal programmatic creation
--   surface the persistence tests need. The transient Mode A session
--   (@scripts/transfer_session.lua@) stays transient — it registers a
--   @saveModules.registerResetHook@ so a session never survives a load
--   (epic decision D-3) — and no session state enters this store.
--
--   __Endpoint symmetry is load-bearing__ (epic decision D-10):
--   'TransferBatch' is endpoint-symmetric, so a building→building order
--   is expressible here with no special case. This store carves nothing
--   out; the acting unit is recorded ALONGSIDE the endpoint pair rather
--   than being inferred from either side, which is exactly what lets a
--   unit carry an order between two buildings it is not itself an end
--   of.
module Unit.Transfer.Orders
    ( TransferOrderId(..)
    , TransferOrder(..)
    , TransferOrders(..)
    , emptyTransferOrders
    , addTransferOrder
    , transferOrderAllocatorExhausted
    , removeTransferOrder
    , lookupTransferOrder
    , ordersForUnit
    , transferOrderList
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import Unit.Types.Manager (UnitId(..))
import Unit.Transfer (TransferBatch(..))

-- | Order ids start at 1 (see 'emptyTransferOrders') so 0 never names an
--   order — the same rule 'Craft.Bills.BillId' follows, for the same
--   reason: a Lua caller cannot confuse a real id with a falsy default.
--   Allocated PER PAGE, like 'Craft.Bills.BillId' and
--   'Power.Types.PowerNodeId' and unlike the session-global unit /
--   building / item allocators, so the same number legitimately names
--   two different orders on two different pages.
newtype TransferOrderId = TransferOrderId { unTransferOrderId ∷ Word32 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, Serialize)

-- | One durable transfer order: who is carrying it out, and what it is.
--
--   'troUnit' is the ACTING unit — the one that will walk the trip —
--   and is deliberately independent of 'tbSource'/'tbDestination'. For a
--   unit→building order it happens to equal the source; for a
--   building→building order (D-10) it equals neither, which is precisely
--   why it cannot be derived from the batch.
data TransferOrder = TransferOrder
    { troId    ∷ !TransferOrderId
    , troUnit  ∷ !UnitId
    , troBatch ∷ !TransferBatch
      -- ^ The endpoint pair plus one 'Unit.Transfer.QueuedTransfer' per
      --   requested item IN REQUEST ORDER. Order is meaningful (see
      --   'TransferBatch'), so this list is stored and restored as a
      --   list, never as a set or a map that would lose it.
    } deriving (Show, Eq, Generic, Serialize)

-- | One page's order set. The id counter lives inside so it persists
--   WITH the orders — a loaded save cannot mint an id that collides with
--   a restored one, which is the same invariant
--   'Craft.Bills.CraftBills' keeps and
--   'World.Save.Component.Transfer.validateTransferOrders' enforces on
--   decode.
data TransferOrders = TransferOrders
    { trosOrders ∷ !(HM.HashMap TransferOrderId TransferOrder)
    , trosNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

emptyTransferOrders ∷ TransferOrders
emptyTransferOrders = TransferOrders HM.empty 1

-- | Has this page's allocator run out of ids to issue? 'trosNextId'
--   SATURATES at 'maxBound' rather than wrapping, so that value is the
--   terminal "no id left" state, not an issuable id.
--
--   Reaching it needs ~4.29 billion orders on ONE page, so this is a
--   correctness boundary rather than a practical one — but the failure
--   it prevents is silent durable-identity REUSE, which is the one class
--   of bug a save format cannot recover from: incrementing past
--   'maxBound' wraps to 0, the next allocation normalises that back to 1,
--   and 'HM.insert' then overwrites whatever order already holds id 1.
transferOrderAllocatorExhausted ∷ TransferOrders → Bool
transferOrderAllocatorExhausted orders = trosNextId orders ≡ maxBound

-- | Queue a new order for @uid@. The allocator is read from the store
--   itself, so after a load this mints the next UNUSED id rather than
--   restarting at 1 and overwriting a restored order.
--
--   'Nothing' when the allocator is exhausted
--   ('transferOrderAllocatorExhausted'). Refusing is the ONLY safe
--   answer: every alternative — wrapping, reusing, or silently returning
--   the store unchanged with some id the caller would then treat as
--   real — hands out an id that already names a different order, and a
--   durable identity that names two things is exactly what
--   'World.Save.Integrity' can no longer tell apart afterwards. A caller
--   that cannot proceed without an order must surface the refusal, not
--   paper over it.
--
--   @max 1@ makes "0 is never issued" structural rather than merely
--   conventional: 'emptyTransferOrders' starts at 1 and every decode
--   path rejects an allocator below it, so this can only ever be the
--   identity — but a single missed guard elsewhere would otherwise mint
--   the one id the rest of the system treats as "no order".
addTransferOrder ∷ UnitId → TransferBatch → TransferOrders
                 → Maybe (TransferOrders, TransferOrderId)
addTransferOrder uid batch orders
    | transferOrderAllocatorExhausted orders = Nothing
    | otherwise =
        let next  = max 1 (trosNextId orders)
            oid   = TransferOrderId next
            order = TransferOrder { troId = oid, troUnit = uid
                                  , troBatch = batch }
        in Just ( orders { trosOrders = HM.insert oid order (trosOrders orders)
                         , trosNextId = next + 1 }
                , oid )

-- | Cancel an order outright. False when the id names nothing.
removeTransferOrder ∷ TransferOrderId → TransferOrders
                    → (TransferOrders, Bool)
removeTransferOrder oid orders
    | HM.member oid (trosOrders orders) =
        (orders { trosOrders = HM.delete oid (trosOrders orders) }, True)
    | otherwise = (orders, False)

lookupTransferOrder ∷ TransferOrderId → TransferOrders → Maybe TransferOrder
lookupTransferOrder oid = HM.lookup oid ∘ trosOrders

-- | Every order @uid@ is carrying, oldest first. Sorted by id (which is
--   monotonic) rather than by hash-map order, so two calls on the same
--   store can never disagree.
ordersForUnit ∷ UnitId → TransferOrders → [TransferOrder]
ordersForUnit uid = filter ((≡ uid) ∘ troUnit) ∘ transferOrderList

-- | Every order on this page, oldest first — the deterministic
--   enumeration every consumer should use instead of 'HM.elems'.
transferOrderList ∷ TransferOrders → [TransferOrder]
transferOrderList = sortOn troId ∘ HM.elems ∘ trosOrders

{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The durable MATERIAL RECEIPT a paid construction designation owns
--   (#1844), and the payment state it is the authority for.
--
--   Before this, payment was one 'Bool' on the designation
--   (@cdMaterialsPaid@) and a refund re-read the CURRENT structure-pack
--   YAML to decide what to give back. That cannot reproduce what was
--   actually spent: change a pack's @build:@ costs, or remove the pack,
--   and a job paid under the old costs refunds the new ones — or
--   nothing at all. The saved Lua job could not help either; it
--   deliberately strips its copied build table on save.
--
--   So the exact multiset removed for one attempt is recorded ON the
--   designation at payment time and never recomputed. Consequences
--   worth knowing:
--
--     * RECEIPT PRESENCE IS THE PAID STATE. There is no second boolean
--       and no Lua-local field that may disagree with it (requirement
--       15); the Lua job's @paid@ flag is a READ of this and nothing
--       more.
--     * A refund reads the receipt and only the receipt. Current pack
--       metadata is never consulted, so a paid job is grandfathered
--       across a cost change and an UNPAID one simply uses whatever the
--       cost is when it next becomes actionable (requirement 17).
--     * The entry list is CANONICAL — summed per material id and sorted
--       by it — so two receipts describing the same multiset are equal
--       and encode to the same bytes. A save must be deterministic, and
--       a receipt built from a hash-ordered walk of a Lua table would
--       not be.
--     * An EMPTY receipt is a real, distinct state: a kind whose
--       @materials@ block is empty was paid, and cost nothing. It is not
--       the same as no receipt at all.
module World.Construct.Receipt
    ( MaterialReceipt(..)
    , mkMaterialReceipt
    , receiptEntries
    , receiptItems
    , ConstructPayment(..)
    , paymentReceipt
    , isPaid
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.Map.Strict as M

-- | The exact multiset of materials removed for one attempt, in
--   deterministic order.
--
--   Construct it with 'mkMaterialReceipt', never the bare constructor
--   from an arbitrary list: the canonical order is what makes the
--   encoding stable and 'Eq' mean "the same multiset".
newtype MaterialReceipt = MaterialReceipt
    { mrEntries ∷ [(Text, Int)]
      -- ^ (item def name, count), ascending by name, each name once,
      --   every count positive.
    } deriving stock (Generic)
      deriving newtype (Show, Eq, Serialize, NFData)

-- | Canonicalise a raw (name, count) list into a receipt: counts summed
--   per name, non-positive totals dropped, ascending by name.
--
--   Dropping a non-positive total is not a silent repair of a real
--   cost — a build cost of zero of something removes nothing, so
--   recording it would make the refund spawn nothing while claiming an
--   entry. What the receipt promises is what was actually TAKEN.
mkMaterialReceipt ∷ [(Text, Int)] → MaterialReceipt
mkMaterialReceipt raw = MaterialReceipt
    [ (name, n)
    | (name, n) ← M.toAscList (M.fromListWith (+) raw)
    , n > 0 ]

receiptEntries ∷ MaterialReceipt → [(Text, Int)]
receiptEntries = mrEntries

-- | The receipt flattened to one element per unit of material, in the
--   receipt's own order — what a refund actually spawns.
receiptItems ∷ MaterialReceipt → [Text]
receiptItems r = [ name | (name, n) ← mrEntries r, _ ← [1 .. n] ]

-- | A designation's payment state. Field order is load-bearing
--   (positional 'Generic' 'Serialize' — append, don't reorder).
data ConstructPayment
    = CpUnpaid
      -- ^ Nothing has been removed for this attempt. What every fresh
      --   designation starts as, and what a partially failed payment
      --   restores to.
    | CpPaid !MaterialReceipt
      -- ^ Paid, and this is exactly what was removed.
    | CpLegacyPaid
      -- ^ A pre-#1844 payload said "paid" with a bare boolean and no
      --   record of WHAT. Produced only by migration, resolved only by
      --   load staging — which reconstructs the receipt from the
      --   currently registered build metadata, or rejects the load when
      --   that metadata is gone, because inventing a refund and losing
      --   the materials are both wrong (requirement 21).
      --
      --   Deliberately unreachable outside those two places: a v4
      --   payload never encodes it, because staging has resolved it
      --   before anything can be saved. It must NOT be deleted — this
      --   is a positionally serialized, append-only enum.
    deriving (Show, Eq, Generic, Serialize, NFData)

-- | The receipt a refund would use, or 'Nothing'.
--
--   'CpLegacyPaid' answers 'Nothing' because it genuinely has no
--   receipt — the caller that must not treat that as "unpaid" is load
--   staging, which matches on the constructor.
paymentReceipt ∷ ConstructPayment → Maybe MaterialReceipt
paymentReceipt (CpPaid r) = Just r
paymentReceipt _          = Nothing

-- | Has material been removed for this attempt? True for both paid
--   shapes: a legacy paid job's materials really did leave someone's
--   inventory, whatever the record of them is now worth.
isPaid ∷ ConstructPayment → Bool
isPaid CpUnpaid = False
isPaid _        = True

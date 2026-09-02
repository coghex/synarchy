{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Construction-designation state (issue #95).
--
--   A construction designation marks one tile for a future build job —
--   either a structure piece (wall / floor / ceiling / post, described
--   abstractly so the build AI resolves art + materials at execution
--   time) or a building. Each carries a status (pending → claimed →
--   complete) and a build-progress accumulator that the build AI
--   (issue #96) fills.
--
--   This is the construction parallel to 'World.Mine.Types': a per-tile
--   designation layer keyed by global tile coords, persisted in saves,
--   and rendered as a ghost of the thing itself — a STRUCTURE piece's
--   own art at the z the placer will use (#1846), a BUILDING's own
--   sprite at its anchor (#1845). Unlike mining (which removes
--   material) construction ADDS it; the execution side is issue #96, so
--   this module is purely the data the designation tool stores.
module World.Construct.Types
    ( StructurePiece(..)
    , ConstructTarget(..)
    , ConstructStatus(..)
    , ConstructDesignation(..)
    , ConstructDesignations
    , newConstructDesignation
    , constructDesignationPaid
    , constructDesignationReceipt
    , constructCorners
    , constructStatusToText
    , textToConstructStatus
    , constructTargetCategory
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM

import World.Construct.Attempt (ConstructAttemptId)
import World.Construct.Receipt
    (ConstructPayment(..), MaterialReceipt, isPaid, paymentReceipt)

-- | Abstract structure-piece descriptor: pack + kind (+ wall edge).
--   Deliberately art-free — the build AI (#96) resolves this to the
--   concrete texture / facemap / slot-tag via scripts/structures.lua at
--   build time, so a designation stays save-stable across art changes
--   (mirrors "material selection at designation time is not needed
--   here", #95).
data StructurePiece = StructurePiece
    { spPack ∷ !Text
      -- ^ Structure pack name (e.g. "dungeon_1").
    , spKind ∷ !Text
      -- ^ Piece kind: "wall" | "floor" | "ceiling" | "post".
    , spEdge ∷ !(Maybe Text)
      -- ^ Wall edge "ne"|"nw"|"se"|"sw" for walls; Nothing for
      --   floor / ceiling / post (which have no orientation).
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- | What a designated tile is slated to become.
data ConstructTarget
    = CtStructure !StructurePiece
    | CtBuilding  !Text            -- ^ building def name (e.g. "cargo_hold_S")
    deriving (Show, Eq, Generic, Serialize, NFData)

-- | Job lifecycle. APPEND-ONLY (positional Generic Serialize — new
--   variants go at the end). Pending: unclaimed work. Claimed: a worker
--   has taken it (#96 sets this). Complete: built (the designation is
--   cleared on completion, so this is mostly a transient signal).
--   Placing: the claimant is inside its final placement hand-off (#1844
--   requirement 18) — see 'World.Thread.Command.Cursor.Construct.beginConstructPlacement'
--   for why that window needs a state of its own, and
--   "World.Construct.Revalidate" for what skips it.
data ConstructStatus = CsPending | CsClaimed | CsComplete | CsPlacing
    deriving (Show, Eq, Generic, Serialize, NFData)

-- | One designated tile. Field order is load-bearing (positional
--   Generic Serialize — append, don't reorder).
data ConstructDesignation = ConstructDesignation
    { cdZ        ∷ !Int
      -- ^ Surface z captured at designation time.
      --
      --   NEITHER ghost renders straight from it any more. A BUILDING
      --   ghost draws at the z its stake will land on, read live from
      --   the anchor's own terrain by the same
      --   'Building.Placement.buildingAnchorZ' @building.spawn@ stamps
      --   'biGridZ' from (#1845) — otherwise a terrain edit under a live
      --   designation would leave the plan at a level the building will
      --   not land on, and staking would visibly move it. This stored
      --   value is that ghost's FALLBACK, for a non-resident chunk: the
      --   one state nobody can answer for.
      --
      --   A STRUCTURE ghost does not either (#1846). This is a SURFACE level and
      --   the piece sits above it (floor, wall and wire at + 1, a ceiling
      --   at + 2, a post at its supporting floor's z), so the ghost draws
      --   at the final grid z 'World.Construct.Plan' resolves against
      --   live terrain. What 'cdZ' contributes there is the REQUIRED
      --   surface level: a site whose terrain has since drifted is
      --   invalid, never silently retargeted (#1844 requirement 4).
    , cdTarget   ∷ !ConstructTarget
    , cdStatus   ∷ !ConstructStatus
    , cdProgress ∷ !Float
      -- ^ Build progress 0.0 → 1.0, filled by the build AI (#96). 0 at
      --   designation time.
    , cdAttempt  ∷ !ConstructAttemptId
      -- ^ #1844: this designation's durable, never-reused ATTEMPT
      --   identity, allocated from the page's own monotonic allocator at
      --   admission. Every delayed lifecycle operation — claim, status,
      --   progress, payment, cancellation, completion, designation
      --   removal, slope cleanup — carries the identity it OBSERVED and
      --   applies only when this field matches it, so an operation from
      --   a removed attempt is a no-op against a successor at the same
      --   tile rather than a silent corruption of it.
    , cdPayment  ∷ !ConstructPayment
      -- ^ #1844: the durable payment authority, replacing #799's bare
      --   @cdMaterialsPaid :: Bool@. A receipt records the EXACT
      --   material multiset removed for THIS attempt, so a refund
      --   reproduces what was actually spent instead of re-reading pack
      --   metadata that may since have changed or vanished. Receipt
      --   presence IS the paid state — there is deliberately no second
      --   boolean anywhere that could disagree with it (see
      --   "World.Construct.Receipt").
    } deriving (Show, Eq, Generic, Serialize, NFData)

type ConstructDesignations = HM.HashMap (Int, Int) ConstructDesignation

-- | Has material been removed for this designation's attempt? The ONE
--   answer — every consumer reads it here rather than testing a field of
--   its own (#1844 requirement 15).
constructDesignationPaid ∷ ConstructDesignation → Bool
constructDesignationPaid = isPaid ∘ cdPayment

-- | The receipt a refund for this designation would spawn, or 'Nothing'.
constructDesignationReceipt ∷ ConstructDesignation → Maybe MaterialReceipt
constructDesignationReceipt = paymentReceipt ∘ cdPayment

-- | Fresh designation: pending, no progress, unpaid, carrying the
--   attempt identity its admission allocated.
newConstructDesignation
    ∷ Int → ConstructTarget → ConstructAttemptId → ConstructDesignation
newConstructDesignation z tgt aid =
    ConstructDesignation z tgt CsPending 0.0 aid CpUnpaid

-- | Corner-progress state derived from a designation's build progress
--   (#96) — the input 'World.Mine.Types.digSlopeMask' expects, so a
--   tile under construction renders through the SAME slope-variant
--   corner display a mid-dig tile does. Corners drain in fixed
--   NW→NE→SE→SW order, one quarter of the job each (a scalar can't
--   carry the digger-side-first order mining gets from its live
--   worker position — and it must stay derived, or the designation
--   would need a schema change). progress 0 → all corners full
--   (mask 0, nothing shown); progress 1 → all drained.
constructCorners ∷ Float → (Float, Float, Float, Float)
constructCorners progress =
    let corner i = 1.0 - max 0.0 (min 1.0 (progress * 4.0 - fromIntegral i))
    in (corner (0 ∷ Int), corner 1, corner 2, corner 3)

constructStatusToText ∷ ConstructStatus → Text
constructStatusToText CsPending  = "pending"
constructStatusToText CsClaimed  = "claimed"
constructStatusToText CsComplete = "complete"
constructStatusToText CsPlacing  = "placing"

textToConstructStatus ∷ Text → Maybe ConstructStatus
textToConstructStatus "pending"  = Just CsPending
textToConstructStatus "claimed"  = Just CsClaimed
textToConstructStatus "complete" = Just CsComplete
textToConstructStatus "placing"  = Just CsPlacing
textToConstructStatus _          = Nothing

-- | "structure" | "building" — the designation's target CLASS, as the
--   debug log line and @construction.getDesignationAt@ report it.
--
--   It no longer picks a ghost texture, and no longer picks anything
--   else about the drawing either: #1846 resolves a structure ghost
--   from the piece's own descriptor and #1845 resolves a building
--   ghost from its own definition, so neither category has a
--   placeholder left to select.
constructTargetCategory ∷ ConstructTarget → Text
constructTargetCategory (CtStructure _) = "structure"
constructTargetCategory (CtBuilding  _) = "building"

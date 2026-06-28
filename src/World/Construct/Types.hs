{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
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
--   and rendered as a blueprint ghost. Unlike mining (which removes
--   material) construction ADDS it; the execution side is issue #96, so
--   this module is purely the data the designation tool stores.
module World.Construct.Types
    ( StructurePiece(..)
    , ConstructTarget(..)
    , ConstructStatus(..)
    , ConstructDesignation(..)
    , ConstructDesignations
    , newConstructDesignation
    , constructStatusToText
    , textToConstructStatus
    , constructTargetCategory
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM

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
data ConstructStatus = CsPending | CsClaimed | CsComplete
    deriving (Show, Eq, Generic, Serialize, NFData)

-- | One designated tile. Field order is load-bearing (positional
--   Generic Serialize — append, don't reorder).
data ConstructDesignation = ConstructDesignation
    { cdZ        ∷ !Int
      -- ^ Surface z captured at designation time (the ghost renders from
      --   it, no per-frame column reads — same trick as MineDesignation).
    , cdTarget   ∷ !ConstructTarget
    , cdStatus   ∷ !ConstructStatus
    , cdProgress ∷ !Float
      -- ^ Build progress 0.0 → 1.0, filled by the build AI (#96). 0 at
      --   designation time.
    } deriving (Show, Eq, Generic, Serialize, NFData)

type ConstructDesignations = HM.HashMap (Int, Int) ConstructDesignation

-- | Fresh designation: pending, no progress.
newConstructDesignation ∷ Int → ConstructTarget → ConstructDesignation
newConstructDesignation z tgt = ConstructDesignation z tgt CsPending 0.0

constructStatusToText ∷ ConstructStatus → Text
constructStatusToText CsPending  = "pending"
constructStatusToText CsClaimed  = "claimed"
constructStatusToText CsComplete = "complete"

textToConstructStatus ∷ Text → Maybe ConstructStatus
textToConstructStatus "pending"  = Just CsPending
textToConstructStatus "claimed"  = Just CsClaimed
textToConstructStatus "complete" = Just CsComplete
textToConstructStatus _          = Nothing

-- | "structure" | "building" — used to pick which ghost texture a
--   designation renders with.
constructTargetCategory ∷ ConstructTarget → Text
constructTargetCategory (CtStructure _) = "structure"
constructTargetCategory (CtBuilding  _) = "building"

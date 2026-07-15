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
    , constructCorners
    , constructStatusToText
    , textToConstructStatus
    , constructTargetCategory
    , constructDesignationFootprint
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import Building.Types (BuildingDef(..), footprintTiles)

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
    , cdMaterialsPaid ∷ !Bool
      -- ^ #799: has the piece's full material cost already been taken
      --   from SOME claimant's inventory? This is the durable payment
      --   marker — unlike the build AI's in-memory @job.consumed@ (lost
      --   when its claimant dies), it rides the designation itself, so
      --   a replacement worker (or the same worker after a save/load)
      --   never re-sources and re-pays a cost that was already spent.
      --   False at designation time; append-only field (positional
      --   Generic Serialize).
    } deriving (Show, Eq, Generic, Serialize, NFData)

type ConstructDesignations = HM.HashMap (Int, Int) ConstructDesignation

-- | Fresh designation: pending, no progress, unpaid.
newConstructDesignation ∷ Int → ConstructTarget → ConstructDesignation
newConstructDesignation z tgt = ConstructDesignation z tgt CsPending 0.0 False

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

-- | Tile footprint one designation renders across (#95 blueprint ghost
--   requirement, completed by #807). A structure piece is already one
--   map entry PER TILE — the designation tool tiles the whole
--   rectangle at commit time (Construct.hs's handleWorldDesignateConstructCommand),
--   so it renders as just its own anchor here. A building target is
--   the opposite: ALWAYS one anchor-only map entry, one durable job,
--   regardless of the def's footprint size — this is what expands
--   that single entry into the full 'footprintTiles' rectangle using
--   the SAME anchor/tile_size convention 'Building.Placement.canPlaceAt'
--   and 'building.spawn' use, so the render pass can't drift from
--   placement. A def missing from the supplied map (a broken save or
--   mod) falls back to the anchor tile alone rather than guessing
--   geometry — the caller is responsible for surfacing that
--   observably (see 'World.Render.CursorQuads').
constructDesignationFootprint
    ∷ HM.HashMap Text BuildingDef → (Int, Int) → ConstructDesignation
    → [(Int, Int)]
constructDesignationFootprint defs (ax, ay) cd = case cdTarget cd of
    CtStructure _      → [(ax, ay)]
    CtBuilding defName → case HM.lookup defName defs of
        Just def → footprintTiles ax ay (bdTileW def) (bdTileH def)
        Nothing  → [(ax, ay)]

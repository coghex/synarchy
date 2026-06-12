{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Tile-level edits the player has made to the procedurally-generated
--   world. Stored as a per-chunk log so chunks can evict freely and be
--   regenerated without losing edits — on load, the chunk is built from
--   the seed and then the log is replayed onto it.
--
--   See "World.Edit.Apply" for the replay implementation. The edit
--   handlers in "World.Thread.Command.Edit" append to this log and
--   simultaneously mutate the in-memory chunk so the live game state
--   matches what a replay would produce.
module World.Edit.Types
    ( WorldEdit(..)
    , WorldEdits
    , emptyWorldEdits
    , appendEdit
    ) where

import UPrelude
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..))
import World.Fluid.Types (FluidType(..))
import World.Material.Id (MaterialId(..))

-- | One tile-scale edit, keyed by global (gx, gy). Replay order
--   matters: deleting a column then placing fluid produces a fluid
--   tile on top of the lowered column, not the original.
--
--   Add new constructors at the END to keep the cereal-derived
--   encoding's tag ordering stable across releases. Adding a new
--   variant is still a save-schema break (bumps currentSaveVersion),
--   but at least order-preserving lets us write a manual migration
--   later if we ever need one.
data WorldEdit
    = WeDeleteTile !Int !Int               -- ^ Lower the column at (gx,gy) by 1 z.
    | WeSetFluidTile !Int !Int !FluidType  -- ^ Place a fluid tile on top of the column at (gx,gy).
    | WeAddTile !Int !Int !MaterialId      -- ^ Raise the column at (gx,gy) by 1 z of the given
                                           --   material (spoil-pile promotion to terrain).
    deriving (Show, Eq, Generic, Serialize)

-- | All edits in a world, keyed by the chunk that contains them.
--   Within each chunk, edits are stored oldest-first so replay can
--   left-fold them onto a freshly-generated chunk.
type WorldEdits = HM.HashMap ChunkCoord [WorldEdit]

emptyWorldEdits ∷ WorldEdits
emptyWorldEdits = HM.empty

-- | Append an edit to the per-chunk log. Snocs to maintain oldest-first
--   order; the lists are bounded by tile-edit frequency (typically
--   tens to low hundreds per chunk), so the O(n) append is fine.
appendEdit ∷ ChunkCoord → WorldEdit → WorldEdits → WorldEdits
appendEdit coord edit = HM.alter (Just . maybe [edit] (++ [edit])) coord

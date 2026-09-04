{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Items lying in the world.
--
--   A ground item is a full 'ItemInstance' (so inventory → ground →
--   inventory round-trips preserve fill / quality / condition) at a
--   FLOAT world position. Deliberately no stored z: the render pass
--   derives the resting height from the CURRENT terrain surface (and
--   its slope) every frame, so digging the tile under an item drops
--   the item with it automatically — there is no way for a ground
--   item's height to go stale.
--
--   Stored on 'WorldState' (wsGroundItemsRef) and persisted in saves
--   (sdGroundItems, v32). Writers use atomicModifyIORef' — debug
--   spawn (Lua thread), future drop/pickup (unit thread), and future
--   dig yields (world thread) all mutate the same map safely.
module Item.Ground
    ( GroundItem(..)
    , GroundItems(..)
    , emptyGroundItems
    , spawnGroundItem
    , removeGroundItem
    , groundPositionIsFinite
    , sanitizeGroundItems
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import Item.Types (ItemInstance(..))

data GroundItem = GroundItem
    { giInst ∷ !ItemInstance
    , giX    ∷ !Float          -- ^ world tile-space x (float, sub-tile)
    , giY    ∷ !Float          -- ^ world tile-space y
    } deriving (Show, Eq, Generic, Serialize)

data GroundItems = GroundItems
    { gisNextId ∷ !Int
    , gisItems  ∷ !(HM.HashMap Int GroundItem)
    } deriving (Show, Eq, Generic, Serialize)

emptyGroundItems ∷ GroundItems
emptyGroundItems = GroundItems 0 HM.empty

-- | Insert; returns the assigned id.
spawnGroundItem ∷ ItemInstance → Float → Float → GroundItems
                → (GroundItems, Int)
spawnGroundItem inst x y gis =
    let gid = gisNextId gis
        gi  = GroundItem inst x y
    in ( gis { gisNextId = gid + 1
             , gisItems  = HM.insert gid gi (gisItems gis) }
       , gid )

-- | Remove by id; returns the removed item (for pickup flows).
removeGroundItem ∷ Int → GroundItems → (GroundItems, Maybe GroundItem)
removeGroundItem gid gis =
    case HM.lookup gid (gisItems gis) of
        Nothing → (gis, Nothing)
        Just gi → (gis { gisItems = HM.delete gid (gisItems gis) }, Just gi)

-- | Whether an entry's stored position is one the world can actually
--   place it at (#2336).
--
--   The live spawn boundaries refuse a non-finite coordinate outright
--   (@Engine.Scripting.Lua.API.Items.Ground.groundSpawnCoord@ — a code
--   span, not a link: it is module-private there), so this is about the
--   saves written BEFORE they did. Nothing downstream
--   raises on a stored NaN or infinity: GHC's 'floor' answers 0, so
--   "World.Render.GroundItemQuads" resolves tile (0, 0) with NaN
--   sub-tile offsets and emits a quad the GPU discards, leaving an item
--   that is invisible, unhittable and unpickable — and that round-trips
--   through the page DTO exactly, so it survives every later save.
groundPositionIsFinite ∷ GroundItem → Bool
groundPositionIsFinite gi = finite (giX gi) ∧ finite (giY gi)
  where finite v = not (isNaN v ∨ isInfinite v)

-- | Drop every entry whose stored position is not finite, answering the
--   surviving map beside the dropped entries in ascending id order.
--
--   'gisNextId' is deliberately UNTOUCHED: dropping an entry retires an
--   id, it never rewinds the allocator, so a loaded session cannot mint
--   an id a save already spent. (The same rule pruning a transfer order
--   follows.) Every surviving entry keeps its own page-local id, so
--   nothing that references one by id is invalidated by a sibling's
--   removal.
--
--   Deterministic in the dropped order because the caller warns from
--   it: a 'HM.HashMap' does not promise an iteration order, and a
--   diagnostic that reshuffles between runs is a diagnostic nobody can
--   pin.
sanitizeGroundItems ∷ GroundItems → (GroundItems, [(Int, GroundItem)])
sanitizeGroundItems gis =
    ( gis { gisItems = HM.filter groundPositionIsFinite (gisItems gis) }
    , sortOn fst [ e | e@(_, gi) ← HM.toList (gisItems gis)
                     , not (groundPositionIsFinite gi) ] )

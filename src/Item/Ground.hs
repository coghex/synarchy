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
--   (sdGroundItems, v32). Every writer uses atomicModifyIORef', so
--   they mutate the same map safely, but they fall into two classes and
--   only one of them is locked:
--
--   * ADDITIONS go straight to 'spawnGroundItem' and take NO lock, from
--     BOTH the Lua thread (@item.spawnGround@, a drop out of an
--     inventory, a forage harvest, location\/loot salvage) and the
--     world thread (dig yields, and a construct designation's refund).
--     They need no lock: a spawn never reuses an id, so it cannot
--     invalidate a gid a selection just validated.
--   * REMOVALS take the page's ground-item lock, through
--     "World.GroundItems", because they can: @item.removeGround@ and a
--     pickup through 'World.GroundItems.takeGroundItemOnPage' (Lua
--     thread), and #2490's lava-water reaction through
--     'World.GroundItems.takeGroundItemsOnPage' (world thread),
--     destroying whatever lay on the cell it turned to stone. That lock
--     is what keeps a removal from landing between a selection's check
--     and its commit; see that module.
--   * RELOCATION (#2486) takes the same lock, through
--     'World.GroundItems.moveGroundItemOnPage'. It belongs to neither
--     class above: it neither allocates an id nor retires one, so the
--     spawn argument does not cover it, and unlike a removal it has to
--     agree with the UNLOCKED writers as well — a temperature tick
--     rewrites @giInst@ of the very row it repositions — which is why
--     the commit re-reads the live row under the lock rather than
--     writing back a copy captured before it.
module Item.Ground
    ( GroundItem(..)
    , GroundItems(..)
    , emptyGroundItems
    , spawnGroundItem
    , removeGroundItem
    , moveGroundItem
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

-- | Relocate @gid@'s row to @(x, y)@ in place, reporting whether it
--   took (#2486).
--
--   The identity-preserving counterpart of a remove-then-respawn, which
--   is NOT an alternative: 'spawnGroundItem' always mints a fresh gid,
--   so the round trip retires the id every caller, selection and
--   persisted reference already names. This rewrites the two position
--   fields of the row that is there and nothing else — 'gisNextId' is
--   untouched (it allocates; it does not describe), 'giInst' is carried
--   over WHOLE (contents, fill, quality, condition, temperature), and
--   every other row is left alone.
--
--   @iid@ is the instance the caller believes it is moving, and the
--   match against 'Item.Types.iiInstanceId' is what makes this safe to
--   call on a live page: a gid is a page-local SLOT, so between the
--   caller's read and this commit the row it named can have been picked
--   up and the number reused by nothing — or, once an id is retired,
--   the caller's stale gid can simply miss. Refusing an id whose
--   instance no longer matches is the difference between moving the
--   item the caller meant and moving whatever is wearing that number
--   now, which is the same substitution hazard
--   @world.spawnLocationSignificantItem@ exists to close.
--
--   False changes nothing at all, which is what lets a caller treat a
--   refusal as "nothing happened": a picked-up item is not recreated
--   here, and a pickup is never undone.
moveGroundItem ∷ Int → Word64 → Float → Float → GroundItems
               → (GroundItems, Bool)
moveGroundItem gid iid x y gis =
    case HM.lookup gid (gisItems gis) of
        Just gi | iiInstanceId (giInst gi) ≡ iid →
            ( gis { gisItems = HM.insert gid
                        gi { giX = x, giY = y } (gisItems gis) }
            , True )
        _ → (gis, False)

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

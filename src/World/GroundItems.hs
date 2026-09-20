{-# LANGUAGE Strict #-}
-- | The ground-item operations that have to agree with the
--   ground-item SELECTION, and therefore cannot be a bare
--   'atomicModifyIORef'' on @wsGroundItemsRef@ (#2300).
--
--   'Item.Ground' owns the pure map; this owns the page-level
--   discipline around it. A page keeps its item map in
--   'World.State.Types.wsGroundItemsRef' and the id of the selected
--   item in 'World.State.Types.wsCursorRef' — two separate 'IORef's.
--   Selecting an item is a read of the first followed by a write to
--   the second, and nothing about that pair is atomic: a removal
--   landing between them leaves a committed selection for an item that
--   no longer exists, which is the stale-selection defect from the
--   other side of the same window.
--
--   So every half takes 'World.State.Types.wsGroundItemLock' for its
--   whole read-decide-write, exactly as 'World.Chunk.Queue' takes
--   'World.State.Types.wsInitQueueLock' for the init queue and its load
--   phase (#2001). A spawn still does NOT have to participate: it
--   cannot invalidate a gid a selection just validated, and
--   'Item.Ground.spawnGroundItem' never reuses an id, so an item that
--   was present at the moment the lock was held stays
--   present-or-removed and never becomes a different item. Removal is
--   one mutation that does; 'moveGroundItemOnPage' (#2486) is the
--   other, and it is in for a second reason as well — it edits a FIELD
--   of a live row, beside unlocked writers that edit other fields of
--   the same row, so its decision and its edit must both see the map
--   as it is at commit time.
--
--   No function here reads or writes anything but the page it is
--   given, which is what keeps the page-local ground-item contract
--   (#1208) true here by construction.
module World.GroundItems
    ( selectGroundItemOnPage
    , takeGroundItemOnPage
    , takeGroundItemsOnPage
    , moveGroundItemOnPage
    , groundRestShift
    ) where

import UPrelude
import Control.Concurrent.MVar (withMVar)
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, atomicModifyIORef')
import Item.Ground
    (GroundItem, GroundItems(..), moveGroundItem, removeGroundItem)
import World.Chunk.Types (ColumnTiles(..), LoadedChunk(..), columnIndex)
import World.Cursor.Types (CursorState(..))
import World.Chunk.Residency (canonicalChunkCoord)
import World.Generate.Coordinates (canonicalTileFrameWith)
import World.Generate.Types (WorldGenParams)
import World.State.Types (WorldState(..))
import World.Tile.Types (WorldTileData, lookupChunk)

-- | Select ground item @gid@ on @ws@, reporting whether it took.
--
--   True only when @gid@ names an item this page actually holds AND
--   the selection was committed to it; the id is checked and installed
--   under the page's ground-item lock, so a removal cannot slip between
--   the two. False leaves the previous selection exactly as it was —
--   including a previous selection of a DIFFERENT live item, which is
--   what makes a refusal safe to treat as "nothing happened".
--
--   A later removal of a successfully selected item is a different
--   thing and is deliberately not handled here: it leaves the selection
--   standing, and @scripts/item_info_panel.lua@'s same-id refresh is
--   what notices and clears it.
selectGroundItemOnPage ∷ WorldState → Int → IO Bool
selectGroundItemOnPage ws gid =
    withMVar (wsGroundItemLock ws) $ \_ → do
        gis ← readIORef (wsGroundItemsRef ws)
        if HM.member gid (gisItems gis)
            then do
                atomicModifyIORef' (wsCursorRef ws) $ \cs →
                    (cs { selectedGroundItem = Just gid }, ())
                pure True
            else pure False

-- | Remove ground item @gid@ from @ws@, returning it for pickup flows.
--
--   The single-item removal counterpart of 'selectGroundItemOnPage' —
--   the pickup path. Since #2490 it is not the only way an item leaves
--   a live page: 'takeGroundItemsOnPage' below removes a captured SET
--   for the lava-water reaction. Both take the same lock, which is what
--   makes "no removal interleaves a selection" true rather than a
--   property of which thread happens to run the removal today.
takeGroundItemOnPage ∷ WorldState → Int → IO (Maybe GroundItem)
takeGroundItemOnPage ws gid =
    withMVar (wsGroundItemLock ws) $ \_ →
        atomicModifyIORef' (wsGroundItemsRef ws) (removeGroundItem gid)

-- | Remove the ground items @gids@ names, answering the entries that
--   were actually there in ascending id order (#2490).
--
--   The bulk counterpart of 'takeGroundItemOnPage', for a caller that
--   decided WHICH items to destroy at an earlier instant than the one
--   it destroys them at. It takes ids rather than a predicate for
--   exactly that reason: the lava-water reaction chooses its victims
--   before its stone lands and removes them after, and re-deciding at
--   removal time would destroy an item that was dropped onto the cell
--   in between — one that never occupied the cell the reaction caught.
--   An id no longer present is simply skipped.
--
--   Takes the SAME lock 'takeGroundItemOnPage' and
--   'selectGroundItemOnPage' take, for the same reason: the whole
--   read-decide-write happens inside one hold, so a selection cannot
--   validate an id this removal is about to retire.
--
--   Unlike the single-item take it also CLEARS a selection of an item
--   it removed, and that difference is deliberate. A pickup leaves the
--   selection standing because @scripts\/item_info_panel.lua@'s
--   same-id refresh notices and clears it; a solidification can destroy
--   an item on a page the player is not looking at, with no panel open
--   to notice, so the clear has to happen where the removal does
--   (requirement 3 of #2490). A selection naming an item this call did
--   NOT remove is left exactly as it was.
takeGroundItemsOnPage ∷ WorldState → [Int] → IO [(Int, GroundItem)]
takeGroundItemsOnPage _ [] = pure []
takeGroundItemsOnPage ws gids =
    withMVar (wsGroundItemLock ws) $ \_ → do
        removed ← atomicModifyIORef' (wsGroundItemsRef ws) $ \gis →
            let hit = sortOn fst [ (gid, gi)
                                 | gid ← gids
                                 , Just gi ← [HM.lookup gid (gisItems gis)] ]
            in ( gis { gisItems = foldl' (flip HM.delete) (gisItems gis)
                                         (map fst hit) }
               , hit )
        -- 'gisNextId' is untouched for the same reason
        -- 'Item.Ground.sanitizeGroundItems' leaves it alone: removal
        -- retires ids, it never rewinds the allocator.
        when (not (null removed)) $
            atomicModifyIORef' (wsCursorRef ws) $ \cs →
                ( case selectedGroundItem cs of
                    Just gid | gid `elem` map fst removed →
                        cs { selectedGroundItem = Nothing }
                    _ → cs
                , () )
        pure removed

-- | Relocate ground item @gid@ on @ws@ to @(x, y)@, reporting whether
--   it took (#2486).
--
--   The third writer that has to participate in the page's ground-item
--   lock, and the only one that is neither an addition nor a removal.
--   It takes the lock for the whole read-decide-write for the reason
--   the two above do — a pickup landing between the caller's read and
--   this commit would otherwise relocate a row that is already gone,
--   or, worse, a row a later spawn happened to be handed the same
--   number for.
--
--   The re-read inside the hold is load-bearing beyond that. Unlike a
--   selection, this writes a FIELD of the row rather than a separate
--   ref, and the row's OTHER fields have writers that take no lock at
--   all: @item.setGroundTemp@ and the per-page temperature tick both
--   rewrite @giInst@ with a bare 'atomicModifyIORef''. Writing back a
--   'Item.Ground.GroundItem' captured before the lock was acquired
--   would silently revert whichever of those landed in between, so the
--   decision and the edit are both made against the map as it is at
--   commit time, inside 'Item.Ground.moveGroundItem'.
--
--   @iid@ is checked there, not here: an id whose instance no longer
--   matches is refused, and a refusal writes nothing — no row, no
--   'gisNextId', no selection. Like the rest of this module it reads
--   and writes only the page it is given, so a gid live on another page
--   is simply absent here (#1208).
--
--   Destination validity is NOT this function's business and is decided
--   by the caller, against 'groundRestShift'. Terrain is not guarded by
--   this lock and never could be: holding it across a chunk read would
--   order this against removals and nothing else.
moveGroundItemOnPage ∷ WorldState → Int → Word64 → Float → Float → IO Bool
moveGroundItemOnPage ws gid iid x y =
    withMVar (wsGroundItemLock ws) $ \_ →
        atomicModifyIORef' (wsGroundItemsRef ws) (moveGroundItem gid iid x y)

-- | The whole-tile shift carrying raw tile @(rawTX, rawTY)@ into the
--   frame chunks are STORED under, answered only when the column it
--   lands in is loaded AND has material at its own terrain surface
--   (#2486). 'Nothing' means "no ground item can rest there".
--
--   This is deliberately the elevation
--   'World.Render.GroundItemQuads.itemGeometry' rests a ground item at,
--   read from the same two vectors of the same column: a ground item
--   stores no z, so the height it is drawn at is whatever
--   @lcTerrainSurfaceMap@ says the moment it is drawn. Anything else —
--   the camera's z-slice, the elevation a pointer hit — describes where
--   a CLICK was, not where the item would come to rest, and validating
--   against one of those would accept a destination the renderer then
--   resolves to empty air.
--
--   The material index is bounds-checked rather than assumed, because
--   'World.Chunk.Types.ColumnTiles' stores only a trimmed contiguous
--   z-range: a surface z outside @[ctStartZ, ctStartZ + length ctMats)@
--   is an unbuilt or fully-trimmed column, which counts as no material
--   and refuses. 'itemGeometry' guards the parallel @ctSlopes@ index
--   for the same reason, and 'World.Render.HitTest.pickWorldTile'
--   guards the same range.
--
--   Answers the SHIFT rather than the canonical tile so the caller can
--   apply it to the float coordinate it is storing: the shift moves
--   whole chunks, so it carries the sub-tile fraction across unchanged
--   (#1135).
--
--   The frame comes from the page's own params through
--   'World.Chunk.Residency.canonicalChunkCoord', the ONE canonicalisation
--   chunk storage keys are built with (#2001), rather than from a bare
--   world size. An ARENA records a sentinel @wgpWorldSize@ of 100000
--   instead of an extent, so wrapping by that number would map a far
--   arena coord onto a chunk the loader stored under its own identity
--   coord — fabricating a loaded destination out of a wrap the page does
--   not have. 'Nothing' params is identity for the same reason it is
--   elsewhere: "World.Thread.ChunkLoading" bails out on it and has
--   inserted no chunk, so the lookup below misses anyway.
groundRestShift ∷ Maybe WorldGenParams  -- ^ the page's generation params
                → WorldTileData         -- ^ the page's loaded chunks
                → Int → Int             -- ^ raw destination tile
                → Maybe (Int, Int)
groundRestShift mParams td rawTX rawTY = do
    let canon = maybe id canonicalChunkCoord mParams
        (coord, (lx, ly), shift) = canonicalTileFrameWith canon rawTX rawTY
    lc ← lookupChunk coord td
    let idx = columnIndex lx ly
        tz  = lcTerrainSurfaceMap lc VU.! idx
        col = lcTiles lc V.! idx
        mi  = tz - ctStartZ col
    guard (mi ≥ 0 ∧ mi < VU.length (ctMats col))
    guard (ctMats col VU.! mi ≢ 0)
    pure shift

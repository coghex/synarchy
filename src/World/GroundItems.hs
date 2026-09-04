{-# LANGUAGE Strict #-}
-- | The two ground-item operations that have to agree with the
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
--   So both halves take 'World.State.Types.wsGroundItemLock' for their
--   whole read-decide-write, exactly as 'World.Chunk.Queue' takes
--   'World.State.Types.wsInitQueueLock' for the init queue and its load
--   phase (#2001). Removal is the only mutation that has to
--   participate: a spawn cannot invalidate a gid a selection just
--   validated, and 'Item.Ground.spawnGroundItem' never reuses an id, so
--   an item that was present at the moment the lock was held stays
--   present-or-removed and never becomes a different item.
--
--   Neither function reads or writes anything but the page it is given,
--   which is what keeps the page-local ground-item contract (#1208)
--   true here by construction.
module World.GroundItems
    ( selectGroundItemOnPage
    , takeGroundItemOnPage
    ) where

import UPrelude
import Control.Concurrent.MVar (withMVar)
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Item.Ground (GroundItem, GroundItems(..), removeGroundItem)
import World.Cursor.Types (CursorState(..))
import World.State.Types (WorldState(..))

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
--   The removal counterpart of 'selectGroundItemOnPage' and the only
--   way a ground item leaves a live page: taking the same lock is what
--   makes "no removal interleaves a selection" true rather than a
--   property of which thread happens to run the removal today.
takeGroundItemOnPage ∷ WorldState → Int → IO (Maybe GroundItem)
takeGroundItemOnPage ws gid =
    withMVar (wsGroundItemLock ws) $ \_ →
        atomicModifyIORef' (wsGroundItemsRef ws) (removeGroundItem gid)

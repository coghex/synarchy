-- | The live, IO half of the page-owned tile and plant tags (#2705):
--   edits against a page's live state, and admission reconciliation.
--   The state, its rules and the synchronization boundary are
--   "Gameplay.Tags.Page"'s; read its module haddock first.
--
--   'mutatePageTileTags' and 'mutateOccurrenceTags' and
--   'reconcileAdmittedPageTags' run ON THE WORLD THREAD, the only writer
--   of the tile store their checks read — a caller anywhere else sends
--   'World.Command.Types.WorldEditPageTags' and waits for its reply
--   ('requestPageTagEdit'). 'mutatePlotTags' is one atomic step of the
--   plot store and may run on any thread.
module Gameplay.Tags.PageLive
    ( -- * Edits
      mutatePageTileTags
    , mutateOccurrenceTags
    , mutatePlotTags
    , mutatePageTargetTags
      -- * Admission
    , reconcileAdmittedPageTags
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Log (LoggerState, LogCategory(..), logInfo)
import Gameplay.Tags.Edit (TagEdit, TagEditResult(..))
import Gameplay.Tags.Page
import Gameplay.Tags.Types
import World.Chunk.Types (LoadedChunk(..))
import World.Flora.Identity (floraInstanceIdToLua)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..))
import World.Tile.Types (WorldTileData, lookupChunk)

-- * Edits

-- | A cell edit against this page's live tile store. World thread only.
mutatePageTileTags ∷ WorldState → TileTarget → TagEdit → IO TagEditResult
mutatePageTileTags ws t edit = do
    td ← readIORef (wsTilesRef ws)
    atomicModifyIORef' (wsPageTagsRef ws) (editTileTags td t edit)

-- | An occurrence edit against this page's live tile store. World thread
--   only.
mutateOccurrenceTags ∷ WorldState → PlantTarget → TagEdit → IO TagEditResult
mutateOccurrenceTags ws t edit = do
    td ← readIORef (wsTilesRef ws)
    atomicModifyIORef' (wsPageTagsRef ws) (editOccurrenceTags td t edit)

-- | A plot edit: one atomic step of the plot store, which is also where
--   plots are planted and removed. Any thread.
mutatePlotTags ∷ WorldState → PlantTarget → TagEdit → IO TagEditResult
mutatePlotTags ws t edit = do
    td ← readIORef (wsTilesRef ws)
    atomicModifyIORef' (wsCropPlotsRef ws) (editPlotTags td t edit)

-- | Any tile or plant target of THIS page; anything else (another
--   page's, or another category) creates nothing. World thread only.
mutatePageTargetTags ∷ WorldPageId → WorldState → TagTarget → TagEdit → IO TagEditResult
mutatePageTargetTags page ws target edit = case target of
    TargetTile t | tileTargetPage t ≡ page → mutatePageTileTags ws t edit
    TargetPlant p | plantTargetPage p ≡ page →
        if isPlotTarget p then mutatePlotTags ws p edit
                          else mutateOccurrenceTags ws p edit
    _ → pure TagEditNoTarget

-- * Admission

-- | After an admission committed: reconcile the assignments the
--   admitted chunks OWN against what those chunks really hold, and log
--   each removal by coordinate or occurrence id (the chop reconcile's
--   discipline). @admitted@ are the chunks as admitted; only the ones
--   still in @committed@ are examined — a chunk evicted in the same
--   transaction was not stripped of anything and keeps every
--   assignment. World thread only.
reconcileAdmittedPageTags ∷ LoggerState → WorldState → WorldPageId
                          → [LoadedChunk] → WorldTileData → IO ()
reconcileAdmittedPageTags logger ws page admitted committed = do
    let resident = [ lc | a ← admitted, Just lc ← [lookupChunk (lcCoord a) committed] ]
    (cells, occs) ← atomicModifyIORef' (wsPageTagsRef ws) $ \pt →
        let (pt', cs, os) = reconcileAdmittedChunks resident pt in (pt', (cs, os))
    unless (null cells) $ logInfo logger CatWorld $
        "Gameplay tags: admission of page " <> unWorldPageId page
        <> " dropped tags on " <> tshow (length cells)
        <> " cell(s) that are no longer stored non-air terrain: "
        <> T.intercalate ", " [ "(" <> tshow (tileTargetX t) <> "," <> tshow (tileTargetY t)
                                 <> "," <> tshow (tileTargetZ t) <> ")" | t ← cells ]
    unless (null occs) $ logInfo logger CatWorld $
        "Gameplay tags: admission of page " <> unWorldPageId page
        <> " dropped tags on " <> tshow (length occs)
        <> " flora occurrence(s) the admitted chunk no longer holds: "
        <> T.intercalate ", " (map (tshow . floraInstanceIdToLua) occs)

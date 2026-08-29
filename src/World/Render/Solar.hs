{-# LANGUAGE Strict #-}
-- | Which world page lights which geometry (#1869).
--
--   A frame can draw several visible pages at once, and each of them
--   runs its own clock ("World.Thread.Time" advances every entry of
--   @wmVisible@ on that page's own @wsTimeScaleRef@) and was generated
--   at its own size. This module assigns those pages the small slots a
--   vertex carries in @solarPage@ and builds the table those slots
--   index — the two halves that let one merged vertex buffer and one
--   uniform upload still light every page by its own sun.
--
--   Both functions are pure so the attribution can be gated without an
--   engine (@--match \"per-page solar attribution\"@).
module World.Render.Solar
    ( canonicalSolarOrder
    , solarSlotAssignment
    , buildSolarPageTable
    , pageSolarInputs
    , circumferenceTilesFor
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.List (sort, nub)
import Engine.Graphics.Solar
    (SolarBase(..), SolarPageEntry(..), SolarPageTable(..)
    , maxSolarPages, solarPageNone, solarSlotVertexValue)
import World.Chunk.Types (chunkSize)
import World.Page.Types (WorldPageId(..))

-- | The one order slots are handed out in, and the one the table is
--   built in — so an index into either always means the same page.
--
--   The pages' own ids, SORTED and deduplicated: deliberately NOT
--   @wmVisible@'s order. Two things follow, and both are requirements:
--
--     * Reordering the visible list (a @world.hide@ + @world.show@, or
--       a load restoring a different order) produces the identical
--       assignment, so no page's lighting moves and no page's cached
--       geometry is invalidated by a reorder alone.
--     * Adding or removing a page CAN move another page's slot. That is
--       why 'World.Render.Camera.Types.WorldQuadCache' stamps the slot
--       it was built with and rebuilds when it no longer matches:
--       attribution baked into cached vertices must never outlive the
--       assignment it was baked under.
--
canonicalSolarOrder ∷ [WorldPageId] → [WorldPageId]
canonicalSolarOrder = sort . nub

-- | Assign each visible page the @solarPage@ value its geometry carries,
--   numbering 'canonicalSolarOrder' from 1.
--
--   Pages past 'maxSolarPages' are assigned 'solarPageNone' — the
--   pre-#1869 global attribution — rather than dropped or aliased onto
--   someone else's slot. No shipped flow reaches that; a debug console
--   or a save could.
solarSlotAssignment ∷ [WorldPageId] → HM.HashMap WorldPageId Word32
solarSlotAssignment visible = HM.fromList
    [ (pageId, slotFor i)
    | (i, pageId) ← zip [0 ..] (canonicalSolarOrder visible) ]
  where
    slotFor i
        | i < maxSolarPages = solarSlotVertexValue i
        | otherwise         = solarPageNone

-- | The table the assignment above indexes into: entry @i@ is what a
--   vertex carrying @solarSlotVertexValue i@ is lit by.
--
--   Takes the same visible list and a per-page lookup of
--   @(own clock angle, own world size in chunks)@. Either half can be
--   absent — a page not in @wmWorlds@ at all, or one whose generation
--   parameters have not landed yet — and each falls back to what the
--   pre-#1869 global path used, so a page mid-generation is lit exactly
--   as it was before.
buildSolarPageTable
    ∷ SolarBase
    → (WorldPageId → Maybe (Float, Maybe Int))
    → [WorldPageId]
    → SolarPageTable
buildSolarPageTable base inputs visible = SolarPageTable $ V.fromList
    [ pageSolarInputs base (inputs pageId)
    | pageId ← take maxSolarPages (canonicalSolarOrder visible) ]

-- | One page's entry.
--
--   The circumference is ALWAYS that page's own. The base angle is that
--   page's own clock too, unless @world.setSunAngle@ is overriding, in
--   which case every page takes the override as its base and keeps its
--   own circumference — the documented meaning of that page-less call
--   ("Engine.Scripting.Lua.API.World.Clock").
pageSolarInputs ∷ SolarBase → Maybe (Float, Maybe Int) → SolarPageEntry
pageSolarInputs base minputs = SolarPageEntry
    { speSunAngle = if sbOverridden base then sbAngle base else ownAngle
    , speCircumferenceTiles = circumferenceTilesFor (snd =≪ minputs)
    }
  where
    ownAngle = maybe (sbAngle base) fst minputs

-- | A world's u-axis (gx-gy) circumference in tiles.
--
--   'Nothing' — generation parameters not loaded yet — falls back to
--   128 chunks, the same default 'World.Render.Quads' and
--   'Engine.Loop.Frame.activeWorldCircumferenceTiles' already use.
circumferenceTilesFor ∷ Maybe Int → Float
circumferenceTilesFor mWorldSize =
    fromIntegral (fromMaybe 128 mWorldSize * chunkSize)

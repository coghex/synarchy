{-# LANGUAGE LambdaCase #-}
-- | Scene-assembly telemetry (#1921) — the measurements
--   'World.Render.updateWorldTiles' publishes once per completed pass,
--   and the query contract @debug.getSceneStats()@ answers from.
--
--   The engine assembles a frame's world quads from ten categories, and
--   they are not ten uniform traversals: terrain may reuse a cached
--   run, units and buildings filter global manager maps, structures
--   walk loaded chunks and their pieces, the ghost is a single optional
--   value, and the zoom pass combines baked entries with live icons and
--   a cursor. This module carries only the shape those measurements are
--   reported in; every counter's per-category MEANING is stated on
--   'SceneCategory' below and enforced at the site that counts it.
--
--   This is transient session telemetry. Nothing here is serialized,
--   the types derive no 'Serialize' instance, and no save-format field
--   depends on them.
--
--   == Publication
--
--   One 'IORef' holds either 'Nothing' (no completed pass since the
--   last world teardown — the query reports @available = false@) or one
--   whole 'SceneStats' value. 'publishSceneStats' replaces it in a
--   single atomic write, so a reader can never observe rows from two
--   different passes together, and the sequence advances exactly once
--   per completed pass. 'clearSceneStats' returns it to 'Nothing' at
--   the two world-teardown sites that already clear the published
--   quads, so telemetry and 'Engine.Scene.Types.LayeredQuads' can never
--   disagree about whether a world lifecycle ended; the next completed
--   pass then republishes at sequence 1.
module Engine.Scene.Stats
  ( SceneCategory(..)
  , sceneCategoryOrder
  , sceneCategoryId
  , SceneCategoryStat(..)
  , zeroCategoryStat
  , SceneStats(..)
  , publishSceneStats
  , clearSceneStats
  , measureCategory
  , forcedQuadCount
  , forcedLayeredQuadCount
  ) where

import UPrelude
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Data.IORef (IORef, atomicModifyIORef', writeIORef)
import qualified Data.Map as Map
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTimeNSec)

-- | The ten scene-assembly categories, in the fixed order every
--   snapshot reports them in. The order is the enumeration order and is
--   part of the published contract — append only at the end, and only
--   together with the query's documented shape.
--
--   Each constructor's @scanned@ meaning, which is category-specific by
--   design:
--
--   * 'ScTiles' — terrain cells visited during REAL cache rebuilds. A
--     cache reuse scans zero terrain cells while still reporting the
--     reused quads as emitted.
--   * 'ScCursor' — marker-tile candidates evaluated by the builders
--     whose quads the ACTIVE tool mode contributes: the always-on
--     marker builders (mine, chop, till, plant, and the expanded
--     construction footprints) plus that mode's own hover, selection
--     and preview builders. One candidate may emit a background and a
--     foreground quad.
--   * 'ScGroundItems' — ground-item records evaluated on visible pages.
--   * 'ScSpoil' — pile-level evaluations: each pile contributes once
--     for each of the two level passes that examines it.
--   * 'ScBlood' — stored blood-decal records examined while deriving
--     render records for visible pages, before texture, viewport or Z
--     rejection.
--   * 'ScUnits' — entries examined in the GLOBAL unit-manager map,
--     before visible-page, texture or Z filtering.
--   * 'ScBuildings' — entries examined in the GLOBAL building-manager
--     state: every live instance in its instance map PLUS every stored
--     destruction effect (#2091's transient, render-only presentation
--     of a demolished building), before visible-page, texture or Z
--     filtering. A frame holding only effects is therefore still
--     measured, not reported as an empty pass.
--   * 'ScStructures' — structure-piece records examined after their
--     loaded chunk passes chunk-visibility culling.
--   * 'ScGhost' — the optional building-ghost candidate: zero or one,
--     before definition, texture-system or other rejection.
--   * 'ScZoomMap' — baked zoom entries, location instances, and present
--     hover/selection cursor candidates evaluated while the zoom pass
--     is active.
--
--   There is deliberately no generic @emitted <= scanned@ invariant: a
--   broken ground item emits its sprite plus an overlay, a spoil pile
--   expands into tile-level quads, a structure piece may emit several
--   wall strips, and a cursor candidate may emit two quads.
data SceneCategory
  = ScTiles
  | ScCursor
  | ScGroundItems
  | ScSpoil
  | ScBlood
  | ScUnits
  | ScBuildings
  | ScStructures
  | ScGhost
  | ScZoomMap
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every category, in the one published order.
sceneCategoryOrder ∷ [SceneCategory]
sceneCategoryOrder = [minBound .. maxBound]

-- | The stable identifier a snapshot row carries. These strings are the
--   query's public contract; they are never derived from the
--   constructor name.
sceneCategoryId ∷ SceneCategory → Text
sceneCategoryId = \case
    ScTiles       → "tiles"
    ScCursor      → "cursor"
    ScGroundItems → "ground_items"
    ScSpoil       → "spoil"
    ScBlood       → "blood"
    ScUnits       → "units"
    ScBuildings   → "buildings"
    ScStructures  → "structures"
    ScGhost       → "ghost"
    ScZoomMap     → "zoom_map"

-- | One category's measurements for one completed pass. @scsScanned@
--   and @scsEmitted@ are non-negative counts; @scsDurationNs@ is an
--   elapsed monotonic-clock duration in whole nanoseconds, never a
--   wall-clock difference.
data SceneCategoryStat = SceneCategoryStat
  { scsCategory   ∷ !SceneCategory
  , scsScanned    ∷ !Int
  , scsEmitted    ∷ !Int
  , scsDurationNs ∷ !Word64
  } deriving (Eq, Show)

-- | The all-zero row for a category, used to normalise a snapshot that
--   somehow omitted one rather than publishing a short list.
zeroCategoryStat ∷ SceneCategory → SceneCategoryStat
zeroCategoryStat cat = SceneCategoryStat
  { scsCategory   = cat
  , scsScanned    = 0
  , scsEmitted    = 0
  , scsDurationNs = 0
  }

-- | One completed pass's whole snapshot. 'ssCategories' always holds
--   exactly one row per 'SceneCategory', in 'sceneCategoryOrder' —
--   'publishSceneStats' is what guarantees it.
data SceneStats = SceneStats
  { ssSequence   ∷ !Word64
  , ssCategories ∷ ![SceneCategoryStat]
  } deriving (Eq, Show)

-- | Publish one completed pass's rows. The sequence is derived from
--   whatever is currently published — 1 after a teardown (or before the
--   first pass), one more than the previous snapshot otherwise — and
--   the whole value lands in a single atomic write, so no reader can
--   see two passes mixed.
--
--   The rows are reordered into 'sceneCategoryOrder' and any missing
--   category is filled with 'zeroCategoryStat', so the published shape
--   is the complete ten-row contract regardless of what the caller
--   assembled. Each row carries its own 'scsCategory', which is what
--   makes that normalisation total rather than positional.
publishSceneStats ∷ IORef (Maybe SceneStats) → [SceneCategoryStat] → IO ()
publishSceneStats ref rows = do
    let rowFor cat = case filter ((≡ cat) . scsCategory) rows of
            (row : _) → row
            []        → zeroCategoryStat cat
        normalised = map rowFor sceneCategoryOrder
    atomicModifyIORef' ref $ \prev →
        let nextSeq = maybe 1 (\prior → ssSequence prior + 1) prev
        in (Just (SceneStats nextSeq normalised), ())

-- | Return the telemetry to its unavailable state. Called from the two
--   world-teardown sites that clear the published quads, so a query
--   after a teardown can never answer with a prior lifecycle's numbers.
clearSceneStats ∷ IORef (Maybe SceneStats) → IO ()
clearSceneStats ref = writeIORef ref Nothing

-- | Measure one category end to end.
--
--   The interval deliberately encloses the whole action, INCLUDING its
--   activation guard and early-return path — a category skipped by its
--   guard still reports the (small) time that decision took. Both the
--   scanned count and the caller's emitted count are forced with
--   'evaluate' BEFORE the end timestamp, so lazily deferred assembly
--   work can never be charged to a later category.
--
--   @emittedOf@ therefore has to FORCE the payload as it counts, not
--   merely measure it. A vector's 'V.length' is @O(1)@ and forces no
--   element at all, so counting alone would leave the quads themselves
--   as thunks for the frame loop — or the NEXT category — to pay for,
--   which is precisely the misattribution this measurement exists to
--   avoid. 'forcedQuadCount' and 'forcedLayeredQuadCount' below are the
--   two counters that do it; do not substitute a bare length.
measureCategory
    ∷ SceneCategory
    → (α → Int)          -- ^ emitted quads, FORCING the payload as it counts
    → IO (Int, α)        -- ^ scanned sources, and the payload
    → IO (SceneCategoryStat, α)
measureCategory cat emittedOf act = do
    start ← getMonotonicTimeNSec
    (scanned, payload) ← act
    scanned' ← evaluate scanned
    emitted  ← evaluate (emittedOf payload)
    end ← getMonotonicTimeNSec
    return
        ( SceneCategoryStat
            { scsCategory   = cat
            , scsScanned    = scanned'
            , scsEmitted    = emitted
            , scsDurationNs = end - start
            }
        , payload )

-- | Emitted count of one per-tick dynamic run, forcing every quad as it
--   counts.
--
--   Forcing a 'Engine.Scene.Types.SortableQuad' to WHNF forces it
--   completely — every field of it and of its four vertices is strict,
--   which is why its own 'NFData' instance is @rwhnf@ — so this is a
--   cheap element walk rather than a deep traversal, and it is exactly
--   what makes the category's own assembly work land inside the
--   category's own timing interval.
forcedQuadCount ∷ NFData α ⇒ V.Vector α → Int
forcedQuadCount = V.foldl' (\n q → rnf q `seq` (n + 1)) 0

-- | 'forcedQuadCount' over a whole per-layer static run.
--
--   This is what forces the per-layer vectors that
--   @Map.unionsWith mergeSortedQuads@ leaves as value thunks (Data.Map
--   is spine-strict, not value-strict), so the merge is charged to the
--   terrain category that caused it instead of to the frame loop's
--   first read.
forcedLayeredQuadCount ∷ NFData α ⇒ Map.Map κ (V.Vector α) → Int
forcedLayeredQuadCount = Map.foldl' (\n v → n + forcedQuadCount v) 0

{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The immutable, validated in-memory capture of an entire game
--   session at one coordinated "Engine.Save.Barrier" boundary (#758,
--   save-overhaul A3). This is deliberately NOT 'World.Save.Types'
--   ('SaveData'/'WorldPageSave') — those are the positional cereal
--   shape B1/B2's legacy in-memory load bridge still uses internally,
--   append-only and version-bumped on every layout change to THAT
--   bridge shape, not to the wire format. 'SessionSnapshot' has no
--   'Serialize' instance and no append-only constraint: it is built
--   once, validated once, and (on save) handed straight to
--   "World.Save.Envelope"'s component encoders (issue #760,
--   save-overhaul B2) — never itself written to disk, and no longer
--   routed through 'World.Save.Snapshot.Adapter' on the way out.
--   'World.Save.Snapshot.Adapter' still exists as the reverse,
--   LOAD-side bridge: decoding an envelope reconstructs a
--   'SessionSnapshot' first, then the adapter converts that into the
--   legacy shape the world-thread load path consumes.
--
--   Everything here is pure. The caller (currently
--   'World.Thread.Command.Save.WriteWorld') still owns every
--   'readIORef' — this module never touches 'IO' or 'EngineEnv' — so
--   'captureSessionSnapshot' is directly constructible from hspec with
--   synthetic manager/page values, no engine boot required.
--
--   What's deliberately ABSENT from this type (vs. 'SaveData'/
--   'WorldPageSave'), per the #756 contract's classification: per-page
--   time scale (load policy, not gameplay state — resume always uses
--   the default speed), tool mode (reset on load, #103), and any
--   runtime handle, selection, or UI-transient field. Their absence is
--   a type-level guarantee, not a runtime filter — there is no field to
--   accidentally leak them back in.
module World.Save.Snapshot
    ( SessionGlobals(..)
    , SessionSnapshot(..)
    , PageSnapshot(..)
    , LiveCameraSnapshot(..)
    , SnapshotError(..)
    , buildSessionSnapshot
    , validateSessionSnapshot
    , captureSessionSnapshot
    , structureEditPaletteErrors
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Structure.Palette (TexPalette(..))
import World.Generate.Types (WorldGenParams)
import World.Page.Types (WorldPageId, WorldIdentity)
import World.Render.Zoom.Types (ZoomMapMode)
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Mine.Types (MineDesignations)
import World.Construct.Types (ConstructDesignations)
import Craft.Bills (CraftBills)
import Power.Types (PowerNodes)
import World.Chop.Types (ChopDesignations)
import World.Till.Types (TillDesignations)
import World.Plant.Types (PlantDesignations)
import World.Spoil.Types (SpoilPiles)
import World.Flora.Harvest (FloraHarvests)
import World.Flora.CropPlot (CropPlots)
import Item.Ground (GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..))
import Engine.Graphics.Camera (CameraFacing)
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState)
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )

-- | Genuinely global, once-per-session values, gathered by the caller
--   before any page is captured. Kept as its own record (rather than
--   flattened into 'buildSessionSnapshot''s argument list) so the
--   "these are shared across every page" grouping is explicit at the
--   call site and in hspec fixtures.
data SessionGlobals = SessionGlobals
    { sgGameTime       ∷ !Double
    , sgTexPalette     ∷ !TexPalette
    , sgNextItemId     ∷ !Word64
    , sgNextBuildingId ∷ !Word32
      -- ^ 'Building.Types.bmNextId' — one global counter, read once.
      --   The temporary adapter duplicates it into every legacy
      --   per-page 'bsnNextId' slot (matching v88's existing
      --   behaviour); this snapshot represents it correctly, once.
    , sgNextUnitId     ∷ !Word32
      -- ^ 'Unit.Types.umNextId', same reasoning.
    , sgActivePage     ∷ !WorldPageId
    , sgVisiblePages   ∷ ![WorldPageId]
    , sgLiveCamera     ∷ !LiveCameraSnapshot
      -- ^ The single global render camera. Captured exactly once;
      --   never copied into every page merely because the old
      --   'WorldPageSave' schema had nowhere else to put a global
      --   zoom/facing pair (contract requirement 5).
    } deriving (Show, Eq)

-- | The live render camera's position/zoom/facing. 'lcsOwnerPage' is
--   'Just' the page it's currently attributed to (the visible page, if
--   any) — 'Nothing' when no page is visible, in which case the
--   position/zoom/facing values still exist (the global camera is
--   always live) but aren't attributable to a specific page.
data LiveCameraSnapshot = LiveCameraSnapshot
    { lcsOwnerPage ∷ !(Maybe WorldPageId)
    , lcsX      ∷ !Float
    , lcsY      ∷ !Float
    , lcsZoom   ∷ !Float
    , lcsFacing ∷ !CameraFacing
    } deriving (Show, Eq)

-- | One page's captured state. 'pgsCameraX'/'pgsCameraY' are that
--   page's OWN remembered position (genuine per-page state, restored
--   even for a page that was never visible) — distinct from
--   'sgLiveCamera', which is global and belongs to whichever page was
--   actually on screen. Deliberately has no time-scale or tool-mode
--   field (contract: load policy / reset-to-default, not persisted
--   gameplay state).
data PageSnapshot = PageSnapshot
    { pgsPageId       ∷ !WorldPageId
    , pgsGenParams    ∷ !WorldGenParams
    , pgsCameraX      ∷ !Float
    , pgsCameraY      ∷ !Float
    , pgsTimeHour     ∷ !Int
    , pgsTimeMinute   ∷ !Int
    , pgsDateYear     ∷ !Int
    , pgsDateMonth    ∷ !Int
    , pgsDateDay      ∷ !Int
    , pgsMapMode      ∷ !ZoomMapMode
    , pgsEdits        ∷ !WorldEdits
    , pgsMineDesignations      ∷ !MineDesignations
    , pgsConstructDesignations ∷ !ConstructDesignations
    , pgsGroundItems  ∷ !GroundItems
    , pgsSpoilPiles   ∷ !SpoilPiles
    , pgsBuildings    ∷ !BuildingSnapshot
    , pgsUnits        ∷ !UnitSnapshot
    , pgsUnitSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    , pgsFloraHarvests ∷ !FloraHarvests
    , pgsChopDesignations ∷ !ChopDesignations
    , pgsCraftBills   ∷ !CraftBills
    , pgsPowerNodes   ∷ !PowerNodes
    , pgsTillDesignations ∷ !TillDesignations
    , pgsCropPlots    ∷ !CropPlots
    , pgsPlantDesignations ∷ !PlantDesignations
    , pgsIdentity     ∷ !(Maybe WorldIdentity)
    } deriving (Show, Eq)

-- | The whole-session capture: every persistable page plus the
--   genuinely global values. The canonical structural-comparison
--   surface for tests is this type's own derived 'Eq' — it holds only
--   gameplay state (no request metadata, no storage path, no
--   timestamp; see 'World.Save.Serialize.SaveRequest' for those), and
--   'HashMap'-keyed collections make comparison order-independent.
data SessionSnapshot = SessionSnapshot
    { snapGameTime       ∷ !Double
    , snapTexPalette     ∷ !TexPalette
    , snapNextItemId     ∷ !Word64
    , snapNextBuildingId ∷ !Word32
    , snapNextUnitId     ∷ !Word32
    , snapActivePage     ∷ !WorldPageId
    , snapVisiblePages   ∷ ![WorldPageId]
    , snapLiveCamera     ∷ !LiveCameraSnapshot
    , snapPages          ∷ !(HM.HashMap WorldPageId PageSnapshot)
    } deriving (Show, Eq)

-- | One failed invariant, naming the specific component/identity that
--   failed it (requirement 9: a failure must identify what broke, not
--   just that something did).
data SnapshotError
    = NoPersistablePages
    | DuplicatePageIds ![WorldPageId]
    | ActivePageMissing !WorldPageId
    | VisiblePageMissing !WorldPageId
    | OrphanedUnitSimState !WorldPageId !UnitId
    | ItemInstanceIdNotBelowAllocator !Word64
    | DuplicateItemInstanceId !Word64
    | BuildingAllocatorTooLow !BuildingId
    | UnitAllocatorTooLow !UnitId
    | StructureEditPaletteIdMissing !WorldPageId !Int
    deriving (Show, Eq)

-- | Build the raw candidate snapshot (unconditionally — validation is
--   a separate pass so tests can construct a deliberately-invalid
--   value directly and exercise 'validateSessionSnapshot' alone).
--   Duplicate page ids in @pages@ collapse silently here (the last one
--   wins in the resulting 'HashMap'); 'captureSessionSnapshot' checks
--   for that BEFORE the fold can lose the evidence.
buildSessionSnapshot ∷ SessionGlobals → [PageSnapshot] → SessionSnapshot
buildSessionSnapshot globals pages = SessionSnapshot
    { snapGameTime       = sgGameTime globals
    , snapTexPalette     = sgTexPalette globals
    , snapNextItemId     = sgNextItemId globals
    , snapNextBuildingId = sgNextBuildingId globals
    , snapNextUnitId     = sgNextUnitId globals
    , snapActivePage     = sgActivePage globals
    , snapVisiblePages   = sgVisiblePages globals
    , snapLiveCamera     = sgLiveCamera globals
    , snapPages          = HM.fromList [ (pgsPageId p, p) | p ← pages ]
    }

-- | Every referential-integrity invariant a valid snapshot must
--   satisfy. Deliberately narrower than every relationship the #756
--   inventory could theoretically name: a craft bill's station
--   reference and a power node's host-building reference are NOT
--   checked here, because a demolished station leaving its bills
--   "lingering, visible + cancellable" is documented, tolerated
--   gameplay behaviour (CLAUDE.md's craft-bill notes), not corruption
--   — hard-failing on it would reject otherwise-valid saves. Likewise
--   tile-coordinate bounds are not re-validated (the inventory already
--   records this as a pre-existing, accepted gap, not a #758
--   requirement). What IS checked below are invariants that should
--   ALWAYS hold by construction; a violation means real corruption.
--
--   Lua-owned state is not represented in this type at all (issue #761,
--   save-overhaul B3 — previously an opaque @snapLuaModules@ blob map):
--   each registered Lua module now snapshots/decodes/validates/applies
--   itself as its own dynamically-added envelope component
--   (@"lua.<module>"@), gathered and validated by
--   "Engine.Scripting.Lua.API.Save" entirely outside 'SessionSnapshot',
--   so there is no Lua-shaped gap for this validator to (not) check.
validateSessionSnapshot ∷ SessionSnapshot → [SnapshotError]
validateSessionSnapshot snap = concat
    [ [ NoPersistablePages | HM.null (snapPages snap) ]
    , [ ActivePageMissing (snapActivePage snap)
      | not (HM.member (snapActivePage snap) (snapPages snap)) ]
    , [ VisiblePageMissing pid
      | pid ← snapVisiblePages snap
      , not (HM.member pid (snapPages snap)) ]
    , [ VisiblePageMissing pid
      | Just pid ← [ lcsOwnerPage (snapLiveCamera snap) ]
      , not (HM.member pid (snapPages snap)) ]
    , orphanedUnitSimStateErrors snap
    , itemAllocatorErrors snap
    , duplicateItemIdErrors snap
    , buildingAllocatorErrors snap
    , unitAllocatorErrors snap
    ]

-- | Every texture/facemap palette id a persisted 'WeSetStructure' edit
--   references, across every page (#760 round 9). Unlike a craft bill's
--   station reference or a power node's host-building reference (both
--   deliberately NOT checked above — a demolished station/building
--   leaving a dangling reference is documented, tolerated gameplay
--   behaviour), a structure edit's palette id is NOT optional/tolerable:
--   'Structure.Palette.lookupPath' is how the renderer turns the id back
--   into a texture path at load, so an id absent from the assembled
--   'TexPalette' can never be resolved to anything — the edit would
--   render as nothing. This genuinely is corruption, so it's a hard
--   reject rather than a tolerated gap.
--
--   Deliberately NOT folded into 'validateSessionSnapshot' (unlike every
--   other cross-cutting check above): that function is shared with
--   'captureSessionSnapshot', which the "full-encode forcing" contract
--   (see "Test.Headless.Save.Snapshot") requires to never force a page's
--   edit-log list ELEMENTS, only its top-level shape — a deferred
--   exploding thunk buried in 'pgsEdits' must survive capture/validation
--   and only explode later, at the real encode. Pattern-matching each
--   edit against 'WeSetStructure' would force exactly those elements.
--   This check is therefore called separately, ONLY on the load path
--   ("World.Save.Component"'s 'assembleSnapshot', after every component
--   has already cereal-decoded its bytes into fully concrete values —
--   there is no deferred thunk left to protect there).
structureEditPaletteErrors ∷ SessionSnapshot → [SnapshotError]
structureEditPaletteErrors snap =
    [ StructureEditPaletteIdMissing (pgsPageId page) pid
    | page  ← HM.elems (snapPages snap)
    , edits ← HM.elems (pgsEdits page)
    , WeSetStructure _ _ _ texId faceId _ ← edits
    , pid ← [texId, faceId]
    , not (HM.member pid (tpIdToPath (snapTexPalette snap)))
    ]

-- | A unit sim-state entry with no matching restored unit instance on
--   the same page (#756: "not currently detected" — this is the new
--   check that closes that gap).
orphanedUnitSimStateErrors ∷ SessionSnapshot → [SnapshotError]
orphanedUnitSimStateErrors snap =
    [ OrphanedUnitSimState (pgsPageId page) uid
    | page ← HM.elems (snapPages snap)
    , uid ← HM.keys (pgsUnitSimStates page)
    , not (HM.member uid (usnInstances (pgsUnits page))) ]

-- | Every id reachable from one 'ItemInstance', including its own AND
--   every id nested (recursively) in 'iiContents' — a first-aid kit's
--   own kit-in-kit contents (#760 round 8: the previous version only
--   ever looked at each container's OUTER id, so a nested item's id
--   colliding with the allocator or with another item elsewhere in the
--   session went undetected).
flattenItemInstanceIds ∷ ItemInstance → [Word64]
flattenItemInstanceIds i =
    iiInstanceId i : concatMap flattenItemInstanceIds (iiContents i)

-- | Every item-instance id across the whole session: ground items,
--   unit inventory/equipped/accessories, and building storage/
--   materials-delivered — the full scope 'nextItemInstanceIdRef' (#67)
--   governs. Recurses into 'iiContents' via 'flattenItemInstanceIds'.
allItemInstanceIds ∷ SessionSnapshot → [Word64]
allItemInstanceIds snap = concatMap pageItemIds (HM.elems (snapPages snap))
  where
    pageItemIds page =
        concatMap (flattenItemInstanceIds ∘ giInst)
                  (HM.elems (gisItems (pgsGroundItems page)))
        ⧺ concatMap unitItemIds (HM.elems (usnInstances (pgsUnits page)))
        ⧺ concatMap buildingItemIds
              (HM.elems (bsnInstances (pgsBuildings page)))
    unitItemIds u =
        concatMap flattenItemInstanceIds (uisInventory u)
        ⧺ concatMap flattenItemInstanceIds (HM.elems (uisEquipped u))
        ⧺ concatMap flattenItemInstanceIds (uisAccessories u)
    buildingItemIds b =
        concatMap (concatMap flattenItemInstanceIds)
                  (HM.elems (bisMaterialsDelivered b))
        ⧺ concatMap flattenItemInstanceIds (bisStorage b)

itemAllocatorErrors ∷ SessionSnapshot → [SnapshotError]
itemAllocatorErrors snap =
    [ ItemInstanceIdNotBelowAllocator iid
    | iid ← allItemInstanceIds snap, iid ≥ snapNextItemId snap ]

duplicateItemIdErrors ∷ SessionSnapshot → [SnapshotError]
duplicateItemIdErrors snap =
    let counts = HM.fromListWith (+)
            [ (iid, 1 ∷ Int) | iid ← allItemInstanceIds snap ]
    in [ DuplicateItemInstanceId iid
       | (iid, n) ← HM.toList counts, n > 1 ]

buildingAllocatorErrors ∷ SessionSnapshot → [SnapshotError]
buildingAllocatorErrors snap =
    [ BuildingAllocatorTooLow bid
    | page ← HM.elems (snapPages snap)
    , bid ← HM.keys (bsnInstances (pgsBuildings page))
    , unBuildingId bid ≥ snapNextBuildingId snap ]

unitAllocatorErrors ∷ SessionSnapshot → [SnapshotError]
unitAllocatorErrors snap =
    [ UnitAllocatorTooLow uid
    | page ← HM.elems (snapPages snap)
    , uid ← HM.keys (usnInstances (pgsUnits page))
    , unUnitId uid ≥ snapNextUnitId snap ]

-- | Duplicate page ids in the INPUT list, checked before
--   'buildSessionSnapshot''s fold can silently collapse them.
duplicatePageIdErrors ∷ [PageSnapshot] → [SnapshotError]
duplicatePageIdErrors pages =
    let counts = HM.fromListWith (+)
            [ (pgsPageId p, 1 ∷ Int) | p ← pages ]
        dupes  = HM.keys (HM.filter (> 1) counts)
    in [ DuplicatePageIds dupes | not (null dupes) ]

-- | Build, then fully validate, a session snapshot. All-or-nothing
--   (requirement 9): any failure returns every failure found, and no
--   partial 'SessionSnapshot' is ever handed back for serialization.
captureSessionSnapshot ∷ SessionGlobals → [PageSnapshot]
                       → Either [SnapshotError] SessionSnapshot
captureSessionSnapshot globals pages =
    let snap = buildSessionSnapshot globals pages
        errs = duplicatePageIdErrors pages ⧺ validateSessionSnapshot snap
    in if null errs then Right snap else Left errs

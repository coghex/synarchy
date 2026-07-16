{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The immutable, validated in-memory capture of an entire game
--   session at one coordinated "Engine.Save.Barrier" boundary (#758,
--   save-overhaul A3). This is deliberately NOT 'World.Save.Types'
--   ('SaveData'/'WorldPageSave') — those are the positional cereal WIRE
--   SCHEMA, append-only and version-bumped on every layout change.
--   'SessionSnapshot' has no 'Serialize' instance and no append-only
--   constraint: it is built once, validated once, and handed to a
--   serializer (currently 'World.Save.Snapshot.Adapter', a temporary
--   bridge to the unchanged v88 'SaveData' format) — never itself
--   written to disk.
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
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Structure.Palette (TexPalette)
import World.Generate.Types (WorldGenParams)
import World.Page.Types (WorldPageId, WorldIdentity)
import World.Render.Zoom.Types (ZoomMapMode)
import World.Edit.Types (WorldEdits)
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
    , sgLuaModules     ∷ !(HM.HashMap Text Text)
      -- ^ Per-module opaque blobs, already collected on the Lua thread
      --   at the barrier's snapshot boundary (see
      --   'Engine.Scripting.Lua.API.Save.collectLuaBlobs') — this
      --   module only ever receives the finished map.
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
    , snapLuaModules     ∷ !(HM.HashMap Text Text)
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
    , snapLuaModules     = sgLuaModules globals
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
--   Also deliberately NOT checked: "Lua component capture succeeded."
--   An empty 'snapLuaModules' cannot be distinguished, from this
--   type alone, between "a real Lua thread's collectLuaBlobs silently
--   swallowed an error" (worth flagging) and "there is no Lua thread
--   in this session at all" (a legitimate, exercised scenario —
--   engine-only tests drive 'WorldSave' directly with an empty blob
--   map). Hard-failing on emptiness rejected a real, valid save in
--   exactly that second case; the distinction this would need lives
--   above this module's boundary (did a Lua thread run at all?), not
--   in the snapshot's own data.
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

-- | A unit sim-state entry with no matching restored unit instance on
--   the same page (#756: "not currently detected" — this is the new
--   check that closes that gap).
orphanedUnitSimStateErrors ∷ SessionSnapshot → [SnapshotError]
orphanedUnitSimStateErrors snap =
    [ OrphanedUnitSimState (pgsPageId page) uid
    | page ← HM.elems (snapPages snap)
    , uid ← HM.keys (pgsUnitSimStates page)
    , not (HM.member uid (usnInstances (pgsUnits page))) ]

-- | Every item-instance id across the whole session: ground items,
--   unit inventory/equipped/accessories, and building storage/
--   materials-delivered — the full scope 'nextItemInstanceIdRef' (#67)
--   governs.
allItemInstanceIds ∷ SessionSnapshot → [Word64]
allItemInstanceIds snap = concatMap pageItemIds (HM.elems (snapPages snap))
  where
    pageItemIds page =
        map (iiInstanceId ∘ giInst) (HM.elems (gisItems (pgsGroundItems page)))
        ⧺ concatMap unitItemIds (HM.elems (usnInstances (pgsUnits page)))
        ⧺ concatMap buildingItemIds
              (HM.elems (bsnInstances (pgsBuildings page)))
    unitItemIds u =
        map iiInstanceId (uisInventory u)
        ⧺ map iiInstanceId (HM.elems (uisEquipped u))
        ⧺ map iiInstanceId (uisAccessories u)
    buildingItemIds b =
        concatMap (map iiInstanceId) (HM.elems (bisMaterialsDelivered b))
        ⧺ map iiInstanceId (bisStorage b)

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

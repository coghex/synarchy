{-# LANGUAGE Strict #-}
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
    , allItemInstanceIds
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Structure.Palette (TexPalette(..))
import World.Generate.Types (WorldGenParams)
import World.Page.Types (WorldPageId, WorldIdentity)
import World.Page.GeneratedId (GeneratedWorldId)
import World.Render.Zoom.Types (ZoomMapMode)
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Mine.Types (MineDesignations)
import World.Construct.Attempt (ConstructAttemptId)
import World.Construct.Types (ConstructDesignations)
import Craft.Bills (CraftBills)
import Unit.Transfer.Orders (TransferOrders)
import Building.Knowledge (ContainerKnowledge)
import Power.Types (PowerNodes)
import World.Chop.Types (ChopDesignations, PendingChopDesignations)
import World.Till.Types (TillDesignations)
import World.Plant.Types (SavedPlantDesignations)
import World.Spoil.Types (SpoilPiles)
import World.Flora.Harvest (FloraHarvests, PendingFloraHarvests)
import World.Flora.CropPlot (SavedCropPlots)
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))
import Engine.Graphics.Camera (CameraFacing)
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState)
import World.Save.Types
    ( BuildingSnapshot(..), UnitSnapshot(..)
    , ItemWalkOrder(..), pageItemContainers, flattenItemInstances )

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
    , pgsConstructNextAttempt ∷ !ConstructAttemptId
      -- ^ #1844: this page's construction ATTEMPT allocator. Durable
      --   because a designation admitted after a load must not be able
      --   to collide with one the save already holds; it only ever
      --   advances, so a legacy payload's reconstructed ids are
      --   unreachable by any later allocation.
    , pgsGroundItems  ∷ !GroundItems
    , pgsSpoilPiles   ∷ !SpoilPiles
    , pgsBuildings    ∷ !BuildingSnapshot
    , pgsUnits        ∷ !UnitSnapshot
    , pgsUnitSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    , pgsFloraHarvests ∷ !FloraHarvests
    , pgsChopDesignations ∷ !ChopDesignations
    , pgsPendingChopMigration ∷ !PendingChopDesignations
      -- ^ #1854: pre-identity, tile-keyed chop designations still
      --   waiting for their chunk. Deferred migration state, never a
      --   second authority — see "World.Chop.Types". Captured so a
      --   session that saves before every chunk has been visited cannot
      --   lose them.
    , pgsPendingFloraHarvests ∷ !PendingFloraHarvests
      -- ^ #1854: pre-identity, tile-keyed regrowth timers on the same
      --   terms.
    , pgsPlantedFloraCursor ∷ !Word64
      -- ^ #1854: this page's planted-flora id allocator cursor,
      --   strictly above every planted id its edit log has issued.
    , pgsCraftBills   ∷ !CraftBills
    , pgsPowerNodes   ∷ !PowerNodes
    , pgsContainerKnowledge ∷ !ContainerKnowledge
      -- ^ #1087: the player's last-known view of each container's
      --   contents on this page. Its remembered 'ItemInstance's are
      --   HISTORICAL OBSERVATIONS, deliberately absent from
      --   'allItemInstanceIds' below — see that function's note.
    , pgsTransferOrders ∷ !TransferOrders
      -- ^ #1246: this page's queue of durable transfer orders — the
      --   acting unit, the endpoint pair, and each requested item's own
      --   lifecycle state, plus the page-local id allocator. Its
      --   references (unit, both endpoints, each item instance) are
      --   same-page and validated by
      --   'World.Save.Integrity.sessionIntegrityErrors'; a target absent
      --   from the whole session is the tolerated, non-blocking
      --   diagnostic 'World.Save.Integrity.sessionIntegrityWarnings'
      --   reports.
    , pgsTillDesignations ∷ !TillDesignations
    , pgsCropPlots    ∷ !SavedCropPlots
      -- ^ #2243: species by authored NAME, not by the runtime handle
      --   the live 'World.Flora.CropPlot.CropPlots' carries — see
      --   "World.Flora.Reference" for why a capture may not persist a
      --   handle. Same for 'pgsPlantDesignations' below and for the
      --   planting entries in 'pgsEdits', which
      --   'World.Edit.Types.WePlaceFloraRef' spells.
    , pgsPlantDesignations ∷ !SavedPlantDesignations
    , pgsIdentity     ∷ !(Maybe WorldIdentity)
    , pgsGeneratedId  ∷ !(Maybe GeneratedWorldId)
      -- ^ #2021: which generated FOUNDATION this page descends from.
      --
      --   'Just' for every page captured from a live session — a live
      --   'World.State.Types.WorldState' always holds one
      --   ('wsGeneratedIdRef' is not optional). 'Nothing' occurs on the
      --   LOAD side only, and means exactly one thing: this page was
      --   decoded from a @world-pages@ payload older than v9, which
      --   predates generated-world identity entirely. Requirement 7
      --   then has transactional load staging
      --   ("World.Load.Stage") assign it a FRESH id — the legacy save
      --   carries nothing an id could honestly be derived from, and
      --   deriving one from the seed or the page id would be the
      --   content dedup requirement 3 rejects.
      --
      --   Deliberately NOT an invariant of 'validateSessionSnapshot':
      --   that validator runs on the load path too
      --   ('World.Save.Component.assembleSnapshot'), where 'Nothing' is
      --   the correct, expected value for every legacy save. The
      --   save-side rule — a CAPTURED page always has one — is enforced
      --   where captures happen, by the non-optional live ref.
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
      -- | #1667: a decoded session-global allocator sitting BELOW its
      --   own first valid id. Separate from the three
      --   @...AllocatorTooLow@ constructors above, which report a live
      --   ENTITY whose id has outrun the cursor: these three report the
      --   CURSOR itself, and therefore fire on an empty session too —
      --   the case the per-id comparisons structurally cannot see.
      --   Item/building/unit ids all start at 1
      --   ("Engine.Core.Init", 'Building.Types.emptyBuildingManager',
      --   'Unit.Types.Manager.emptyUnitManager'), so 0 is the one
      --   invalid value each unsigned cursor can carry.
    | ItemAllocatorBelowFloor !Word64
    | BuildingAllocatorBelowFloor !Word32
    | UnitAllocatorBelowFloor !Word32
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
--   reference and a power node's host-building reference being ABSENT
--   from the whole session are NOT checked here, because a demolished
--   station leaving its bills "lingering, visible + cancellable" is
--   documented, tolerated gameplay behaviour (CLAUDE.md's craft-bill
--   notes), not corruption — hard-failing on it would reject otherwise-
--   valid saves. A station/host-building that resolves on a DIFFERENT
--   page than its bill/node IS a hard error, just not this function's —
--   see "World.Save.Integrity".'World.Save.Integrity.sessionIntegrityErrors'
--   (issue #764, save-overhaul C3), which "World.Save.Component"'s
--   'World.Save.Component.assembleSnapshot' runs as an additional
--   cross-component check on the pre-load boundary, and
--   'World.Thread.Command.Save.WriteWorld.handleWorldSaveCommand' runs
--   the same way immediately after a successful 'captureSessionSnapshot'
--   on the pre-save boundary (kept OUTSIDE this module rather than
--   folded into 'captureSessionSnapshot' itself, since
--   "World.Save.Integrity" imports THIS module for 'SessionSnapshot' —
--   folding the call in here would be a cycle).
--   Likewise tile-coordinate bounds are not re-validated HERE (the
--   inventory already records this as a pre-existing, accepted gap, not
--   a #758 requirement) — with one exception that is not this
--   function's either: since #1668 a persisted location instance's
--   stored 'Location.Bounds.AbsBounds' IS checked for axis inversion,
--   at the component boundary by
--   "World.Save.Component.Page" 's @validatePages@, because the save
--   decode path builds that box from unrestricted wire integers rather
--   than downstream of the YAML loader's gate. What IS checked below
--   are invariants that should ALWAYS hold by construction; a violation
--   means real corruption.
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
    , sessionAllocatorFloorErrors snap
    ]

-- | Every texture/facemap palette id a persisted 'WeSetStructure' edit
--   references, across every page (#760). Unlike a craft bill's
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

-- | Every item-instance id across the whole session: ground items,
--   unit inventory/equipped/accessories, and building storage/
--   materials-delivered — the full scope 'nextItemInstanceIdRef' (#67)
--   governs. Enumerates the containers via
--   'World.Save.Types.pageItemContainers' and recurses into
--   'iiContents' via 'World.Save.Types.flattenItemInstances' — the
--   save system's single item walk (#1090), shared with
--   'Engine.Scripting.Lua.API.Save.Integrity.knownEntitiesFromSaveData'
--   and 'World.Save.Types.missingItemDefReferences'. #760: a nested
--   item's id colliding with the allocator, or with another item
--   elsewhere in the session, must be caught too — the pre-#760
--   version only ever looked at each container's OUTER id.
--
--   'pgsContainerKnowledge' (#1087) is deliberately NOT walked here,
--   and must never be added: its remembered instances are historical
--   OBSERVATIONS of items that live (or lived) somewhere else in this
--   very list, not additional live entities. Folding them in would
--   report every remembered item as a duplicate live id, and — worse —
--   would make staleness itself invalid, since the whole feature
--   depends on a remembered id staying meaningful after the live item
--   has moved, changed, or ceased to exist. Same reasoning excludes
--   them from the allocator bound ('itemAllocatorErrors'), the
--   duplicate check ('duplicateItemIdErrors'), and live @item_instance@
--   reference resolution in "World.Save.Integrity". Their DEF NAMES
--   remain ordinary content references, validated by
--   'World.Save.Types.missingItemDefReferences' like any other.
allItemInstanceIds ∷ SessionSnapshot → [Word64]
allItemInstanceIds snap =
    [ iiInstanceId i
    | page       ← HM.elems (snapPages snap)
    , (_, insts) ← pageItemContainers ItemsGroundFirst
                       pgsGroundItems pgsUnits pgsBuildings page
    , inst ← insts
    , i    ← flattenItemInstances inst ]

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

-- | #1667: each session-global allocator's OWN floor, checked
--   independently of the entity maps the three comparisons above walk.
--   Those iterate live ids, so an EMPTY session certified any cursor —
--   including 0, which 'World.Load.Publish' then installs verbatim and
--   the next allocation hands out as a real id, against the "ids start
--   at 1" convention every one of the three establishes
--   ('Engine.Core.Init''s @nextItemInstanceIdRef@,
--   'Building.Types.emptyBuildingManager', 'Unit.Types.Manager.emptyUnitManager').
--   All three cursors are UNSIGNED on the wire ('World.Save.Component.Session.CoreSessionDTO'),
--   so 0 is the single invalid value each can carry.
--
--   Folded into 'validateSessionSnapshot' rather than into
--   @core-session@'s own @csValidate@ deliberately: the legacy v90
--   bridge ("World.Save.Compat.SessionV90") RECONSTRUCTS these three
--   cursors instead of decoding that component, and funnels the
--   assembled snapshot through this validator — so this is the one
--   place both paths share. It runs on the CAPTURE side too, where a
--   live session can never be below the floor.
sessionAllocatorFloorErrors ∷ SessionSnapshot → [SnapshotError]
sessionAllocatorFloorErrors snap = concat
    [ [ ItemAllocatorBelowFloor (snapNextItemId snap)
      | snapNextItemId snap < 1 ]
    , [ BuildingAllocatorBelowFloor (snapNextBuildingId snap)
      | snapNextBuildingId snap < 1 ]
    , [ UnitAllocatorBelowFloor (snapNextUnitId snap)
      | snapNextUnitId snap < 1 ]
    ]

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

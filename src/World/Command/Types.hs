{-# LANGUAGE Strict #-}
module World.Command.Types
    ( WorldTextureType(..)
    , WorldCommand(..)
    , FluidWriteback(..)
    , FluidWritebackBatch(..)
    , FluidAckOutcome(..)
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Concurrent.MVar (MVar)
import Engine.Asset.Handle (TextureHandle(..))
import Location.Instance (LocationInstanceId, LocationLifecycle)
import Structure.Types (StructureStageToken(..), StructureCommitWindow(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Flora.Identity (FloraInstanceId)
import World.Material.Id (MaterialId(..))
import World.Material (MaterialRegistry)
import Building.Types (BuildingId(..))
import Unit.Types (UnitId)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Construct.Attempt (ConstructAttemptId)
import World.Construct.Types
    (ConstructTarget(..), ConstructStatus(..), StructurePiece(..))
import World.Save.Payload (LuaComponentSpec, LuaRefEdge)
import World.Save.Types (SaveData(..), AutosaveRequest(..))
import World.Texture.Types (WorldTextureType(..))
import World.Fluid.Types (FluidType(..), FluidCell(..))

-- | One chunk's simulated fluid result, produced by the sim thread and
--   applied to 'wsTilesRef' by the WORLD thread (the sole writer). The
--   sim derives all four payload fields so the world handler is a dumb
--   inserter — the one thing it does decide is FRESHNESS, from
--   'fwEditGen'.
data FluidWriteback = FluidWriteback
    { fwCoord    ∷ !ChunkCoord
    , fwEditGen  ∷ !Word64
      -- ^ The chunk's live-edit generation this result was derived from
      --   ('Sim.State.Types.scsEditGen'). The world applies the writeback
      --   only when it EQUALS the page's own current generation for this
      --   chunk ('World.State.Types.wsChunkEditGenRef'), which is what
      --   stops a batch computed before a live edit from overwriting it
      --   (#1596). Per chunk, so an edit to one chunk never drops a
      --   writeback for another in the same batch.
    , fwFluid    ∷ !(V.Vector (Maybe FluidCell))
    , fwTerrain  ∷ !(VU.Vector Int)
    , fwSurf     ∷ !(VU.Vector Int)
    , fwSideDeco ∷ !(VU.Vector Word8)
    }

-- | What a batch's acknowledgement reports (#2334).
--
--   The ack used to be a bare @()@, which could only ever mean "the
--   handler reached its last statement". An exception raised anywhere
--   before that statement therefore delivered NOTHING, and the waiter —
--   the sim's fast settle, and through it the whole @--dump@ — blocked
--   forever on a world worker that had already fail-stopped. Carrying
--   the outcome is what lets the handler release its waiter on the way
--   out without pretending the batch was applied.
data FluidAckOutcome
    = FluidAckApplied
      -- ^ The handler ran to completion. That deliberately INCLUDES
      --   every case it has always treated as nothing to do: an empty
      --   batch, a page that is gone, and a batch whose writebacks were
      --   all dropped as stale (#1596).
    | FluidAckFailed !Text
      -- ^ The handler raised, and this is what it raised. The world
      --   worker still fail-stops on the rethrown original, so this
      --   releases the waiter rather than swallowing the error.
    deriving (Eq, Show)

-- | A batch of fluid writebacks for ONE world plus an optional ack
--   'MVar', signalled once the world thread has finished with the batch
--   — with 'FluidAckApplied' when it applied, 'FluidAckFailed' when it
--   raised. The 'WorldPageId' scopes the batch so the world thread
--   applies it only to the world that produced it (not every visible
--   world, #59). Runtime ticks pass 'Nothing' (fire-and-forget); the
--   dump's synchronous fast-settle passes 'Just' and waits on it so the
--   write lands before it reads.
data FluidWritebackBatch =
    FluidWritebackBatch !WorldPageId ![FluidWriteback]
                        !(Maybe (MVar FluidAckOutcome))
instance Show FluidWritebackBatch where
    show (FluidWritebackBatch pid ws _) =
        "FluidWritebackBatch(" <> show pid <> ", " <> show (length ws) <> ")"

data WorldCommand
    = WorldInit WorldPageId Word64 Int Int (Maybe WorldIdentity)
        -- ^ pageId, seed, worldSize (chunks), plateCount, and the
        --   page's optional player-facing identity (#707) — display
        --   name + gloss, already normalized via 'mkWorldIdentity'.
        --   'Nothing' creates an unnamed page (every pre-#707 caller).
    | WorldInitArena WorldPageId
    | WorldInitArenaDone WorldPageId
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    | WorldSetTime WorldPageId Int Int         -- ^ hour minute
    | WorldSetDate WorldPageId Int Int Int     -- ^ year month day
    | WorldSetTimeScale WorldPageId Float      -- ^ game-minutes per real-second
    | WorldSetMapMode WorldPageId ZoomMapMode      -- ^ map mode
    | WorldSetZoomCursorHover WorldPageId Int Int
    | WorldSetZoomCursorSelect WorldPageId
    | WorldSetZoomCursorDeselect WorldPageId
    | WorldSetZoomCursorSelectTexture WorldPageId TextureHandle
    | WorldSetZoomCursorHoverTexture WorldPageId TextureHandle
    | WorldSetWorldCursorSelectTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHoverTexture WorldPageId TextureHandle
    | WorldSetWorldCursorSelectBgTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHoverBgTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHover WorldPageId Int Int
    | WorldSetWorldCursorSelect WorldPageId
    | WorldSetWorldCursorDeselect WorldPageId
    | WorldSelectTileByCoord WorldPageId Int Int (Maybe Int)
        -- ^ Atomically set worldSelectedTile to the column at (gx, gy).
        --   The @Maybe Int@ is the z to select: @Just z@ pins the exact
        --   tile (the live-picked z from a left-click or a right-click
        --   → Info context-menu selection, so a click below the
        --   surface selects the clicked tile, not the column top —
        --   issue #367); @Nothing@ falls back to the loaded chunk's
        --   surface z, a latent API affordance no current UI path
        --   exercises (both live callers always resolve a pick and
        --   pass its z). Bypasses the hover-then-select dance so
        --   the caller doesn't have to fight the continuous mouse-hover
        --   overwrites for one-shot selections.
    | WorldSelectChunkByCoord WorldPageId Int Int
        -- ^ Atomically set zoomSelectedPos to the chunk whose
        --   chunk-aligned grid origin is (gx, gy) — as produced by
        --   'World.Render.Zoom.Cursor.pixelToChunkOrigin' /
        --   @world.pickChunk@ — clearing any zoomed-in tile selection
        --   in the same write (issue #135's newest-selection-owns
        --   rule). This is the chunk-selection analog of
        --   'WorldSelectTileByCoord': the set and the opposing clear
        --   land in ONE atomic write, so a zoom-map left click binds
        --   to the chunk under the click at click time and can't be
        --   retargeted by a later hover update, camera pan/zoom, or
        --   render timing before some later render pass gets around
        --   to resolving it (issue #813).
    | WorldSetToolMode WorldPageId ToolMode
    | WorldSetMineAnchor WorldPageId Int Int
        -- ^ Mine tool: first click anchors the designation rectangle
        --   at (gx, gy). The render pass previews anchor→hover.
    | WorldClearMineAnchor WorldPageId
        -- ^ Mine tool: cancel the pending rectangle (right-click /
        --   Escape / tool switch).
    | WorldDesignateMine WorldPageId Int Int Int Int
        -- ^ Mine tool: second click commits the rectangle
        --   (gx1,gy1)–(gx2,gy2) (corners in either order). Tiles in
        --   loaded chunks land in wsMineDesignationsRef with their
        --   surface z; tiles in unloaded chunks are skipped.
    | WorldSetMineDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed designation markers.
    | WorldSetConstructAnchor WorldPageId Int Int
        -- ^ Construction tool (#95): first click anchors the designation
        --   rectangle at (gx, gy). The render pass previews anchor→hover
        --   — and, since #1846, already previewed the single hovered
        --   tile BEFORE this arrived, given an armed
        --   'WorldSetConstructStructureTarget'.
    | WorldClearConstructAnchor WorldPageId
        -- ^ Construction tool: cancel the pending rectangle (right-click /
        --   Escape / tool switch).
    | WorldDesignateConstruct WorldPageId Int Int Int Int ConstructTarget
                              (Maybe Word64)
        -- ^ …, plus the expected page-SELECTION generation (#1602).
        --   'Nothing' is an unbound designation (every AI caller, and
        --   the two-click structure rectangle) and is never checked.
        --   'Just' is a placement bound to the click that made it: the
        --   world thread re-checks it against 'wmSelectionGen' before
        --   writing anything, which is exact rather than best-effort
        --   because THIS thread is also the one that applies
        --   world.show / world.hide — a selection change enqueued
        --   before this command is therefore already applied when the
        --   check runs, and one enqueued after is genuinely after the
        --   commit.
        -- ^ Construction tool: second click commits the rectangle
        --   (gx1,gy1)–(gx2,gy2) for the given build target. Tiles in
        --   loaded chunks land in wsConstructDesignationsRef with their
        --   surface z; tiles in unloaded chunks are skipped. Per-z-level
        --   like mine designation. Building targets only mark the anchor
        --   tile (a building is one footprint, not a rectangle of them).
    | WorldCancelConstruct WorldPageId Int Int (Maybe ConstructAttemptId)
        -- ^ Remove the construction designation at (gx, gy), if any
        --   (cancel mode / right-click on an existing blueprint).
        --
        --   #1844: 'Just' removes ONLY that exact attempt, so a delayed
        --   cancellation from a job that has already gone cannot remove
        --   a successor designated at the same tile. 'Nothing' is the
        --   player's coordinate-only erase — "remove whatever is here",
        --   which has no attempt to name until it looks.
    | WorldSetConstructStatus WorldPageId Int Int ConstructStatus
                              ConstructAttemptId
                              (Maybe StructureCommitWindow)
        -- ^ Build AI (#96): mark a designation Claimed / Complete. A
        --   Complete designation is removed (the structure/building it
        --   represents now exists).
        --
        --   #1844: the attempt is REQUIRED, not optional. Cancellation
        --   has an honest coordinate-only form — the player's
        --   right-click erases whatever is at a tile — but a status
        --   transition never does: it is always some worker reporting on
        --   the job it observed, and an attempt-less completion that
        --   matched anything would delete a successor at that tile.
        --   Making it unrepresentable is cheaper than checking for it.
        --
        --   #1844: a COMPLETION may additionally carry the
        --   'StructureCommitWindow' of the placement it is completing —
        --   @structure.place@ returning true means STAGED AND QUEUED,
        --   not committed, and the world thread can still decline the
        --   queued placement when the target chunk has evicted. Given a
        --   window, the handler completes only if nothing in it was
        --   declined; otherwise it cancels that same attempt and refunds
        --   its receipt, because the alternative is a paid job with
        --   neither a structure nor its materials. Exactly the protocol
        --   'WorldMarkLocationStamped' uses for the same hazard (#2051).
    | WorldAddConstructProgress WorldPageId Int Int Float
                                ConstructAttemptId
        -- ^ #1844: required for the same reason — an attempt-less pour
        --   would advance, and visibly stamp progress onto, a job its
        --   sender never claimed.
        -- ^ Build AI (#96): add build progress to the designation at
        --   (gx, gy). Deltas are pre-normalised to the job's total
        --   work (1.0 = done) and the sum is clamped to [0, 1].
        --
        --   NEITHER ghost ramps with it, and only a STRUCTURE job
        --   sends this at all. A structure site is already invisible by
        --   the time any progress is poured (#1846): D-15/D-16 make it
        --   vanish when its materials are PAID for and show nothing
        --   until the finished piece appears. A BUILDING designation
        --   accrues no progress here whatsoever (#1845) — it is staked
        --   into a real 'BuildingInstance' and the work accumulates on
        --   THAT, so its ghost holds a fixed 60 % until the staked
        --   building enters its own build-progress frames. The alpha
        --   ramp this used to describe was a dead 0.45 + 0.55 * 0 and
        --   is deleted.
        --
        --   Completion (placing the piece + removing the designation)
        --   stays Lua-side — the AI resolves art/materials, so it owns
        --   final placement.
    | WorldRevalidateConstructAll
        -- ^ #1844: the CATALOGUE-reconciliation sweep. A terminal
        --   structure-art failure makes a whole pack resolve nothing
        --   (#1842's all-or-nothing rule), which can invalidate
        --   designations on ANY page — including ones over already
        --   resident chunks, where no later terrain edit or chunk
        --   publication would ever re-check them. It carries no page
        --   because the catalogue is keyed by pack NAME and is global;
        --   this is requirement 9's bounded page-level sweep, and it is
        --   enqueued only when a failure actually CHANGED the catalogue.
    | WorldSetConstructStructureTarget WorldPageId (Maybe StructurePiece)
        -- ^ The structure piece the build tool has ARMED (#1846), or
        --   'Nothing' on leaving placement. Drives the pre-anchor hover
        --   preview, which has no designation to read a descriptor from.
    | WorldSetConstructLineMode WorldPageId Bool
        -- ^ Wire path tool (#359): while true, the anchor→hover preview
        --   snaps to a straight 1-wide line (the build tool's commit
        --   snaps identically), instead of the default filled rectangle.
    | WorldDesignateChopInstances WorldPageId [FloraInstanceId] Text
        -- ^ Chop tool (#97, re-shaped by #1856): designate EXACTLY these
        --   plants. The gesture that produced the list is screen-space
        --   (a click on a tree sprite, or a drag box over their rendered
        --   ground-contact anchors — "World.Flora.HitTest"), so the
        --   selection rule lives entirely on the sending side and no
        --   tile rectangle crosses the queue. The world thread still
        --   re-checks eligibility against live state — the unchanged
        --   Chop predicate, a harvestable species carrying the given
        --   tag (the tool passes "wood") with no live regrowth timer —
        --   because a tree can be felled between the gesture and the
        --   drain. An id naming no resident plant is dropped, and the
        --   F4 record reports it as such.
    | WorldEraseChopInstances WorldPageId [FloraInstanceId]
        -- ^ The symmetric erase (#1856 requirement 4): clear exactly
        --   these plants' designations. Filtered by what is DESIGNATED
        --   rather than by add-eligibility, so a standing designation
        --   stays clearable even after its tree stops qualifying.
    | WorldCancelChop WorldPageId Int Int (Maybe FloraInstanceId)
        -- ^ Remove a chop designation — the chop AI's, not the
        --   player's. #1854: with an instance id, EXACTLY that plant's
        --   designation goes, so the felling acolyte cancels the tree it
        --   claimed and leaves its co-tenants designated. Without one it
        --   clears every designation standing on (gx, gy), pending legacy
        --   entries included; since #1856 that shape serves only the AI's
        --   restored-job fallback (which knows its tile but not which of
        --   its plants it had claimed) and the legacy migration drain.
        --   The PLAYER's erase is 'WorldEraseChopInstances'.
    | WorldSetChopDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed chop-designation markers.
    | WorldSetTillAnchor WorldPageId Int Int
        -- ^ Till tool (#333): first click anchors the designation
        --   rectangle at (gx, gy). The render pass previews anchor→hover.
    | WorldClearTillAnchor WorldPageId
        -- ^ Till tool: cancel the pending rectangle (right-click /
        --   Escape / tool switch).
    | WorldDesignateTill WorldPageId Int Int Int Int
        -- ^ Till tool: second click commits the rectangle
        --   (gx1,gy1)–(gx2,gy2). Only tiles in loaded chunks at the
        --   ANCHOR's surface z, with no fluid on top, no flora instance,
        --   and not already tilled, are designated — a farmed field is
        --   flat ground, like a construction footprint.
    | WorldCancelTill WorldPageId Int Int
        -- ^ Remove the till designation at (gx, gy), if any (the till
        --   AI's completion, or a player cancel).
    | WorldSetTillDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed till-designation markers.
    | WorldDesignatePlant WorldPageId Int Int Text
        -- ^ Plant tool (#335): single-tile designation (no anchor — the
        --   planting screen already scopes the player to one tile
        --   before a crop is chosen). gx gy cropName. Refused unless
        --   the tile is tilled soil (world.isPlantable) and cropName
        --   names a registered plantable-crop species (row_crop or
        --   groundcover_crop worldGen category).
        --
        --   #1858: admission is no longer the LAST soil check. Tilled
        --   soil is a continuous requirement — 'World.Plant.Validate'
        --   re-runs the same 'World.Vegetation.isTilledSoil' test after
        --   every write that can move a designated tile's resolved
        --   surface and whenever terrain becomes resident, and REMOVES
        --   the record when the soil is resident and lost. So this
        --   command is where a designation starts, not where its
        --   eligibility is settled for good.
    | WorldCancelPlant WorldPageId Int Int
        -- ^ Remove the plant designation at (gx, gy), if any (the farm
        --   AI's completion, or a player cancel). NOT the only removal
        --   path since #1858 — see 'World.Plant.Validate', whose
        --   invalidation sweep is the farm AI's cancellation signal by
        --   way of the record simply being gone on its next tick.
    | WorldSetPlantDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed plant-designation markers.
    | WorldSetVeg WorldPageId Int Int Int Word8
        -- ^ Set the vegetation id of the tile at (gx,gy,z) via the
        --   WeSetVeg edit path (world.setVegAt) — the till AI's
        --   completion primitive (#333), same shape as world.setSlope.
    | WorldPlantRowCropAt WorldPageId Int Int Text
        -- ^ worldId, gx, gy, cropName. Plant a single row-crop
        --   FloraInstance at (gx,gy) via the WePlaceFlora edit path
        --   (world.plantRowCropAt) — the farm AI's (#336) row-crop
        --   planting completion, the FloraInstance counterpart to
        --   world.plantCropAt's CropPlot for groundcover crops. Refused
        --   world-thread-side unless the tile is tilled soil and
        --   cropName names a registered row_crop species.
    | WorldDigTile WorldPageId Int Int Float Float Float Float Float
        -- ^ Apply dig progress to the designated tile at (gx, gy):
        --   pageId gx gy uxPos uyPos amount minerSkill perception.
        --   perception scales the gem-find roll when the tile
        --   completes (World.Gem). The digger's
        --   tile-space position picks which corners drain first
        --   (digger-side); amount is pre-scaled by tool × material
        --   speed. minerSkill is the CURRENT digger's mining skill —
        --   it scales the per-tick chunk-yield fill, so a mid-dig
        --   handoff uses the new digger's rate. Corners at zero →
        --   the tile drops one z via the WeDeleteTile path and the
        --   designation is removed.
    | WorldAddTile WorldPageId Int Int MaterialId
        -- ^ Raise the column at (gx, gy) one z of the given material
        --   via the WeAddTile edit path (debug terrain placement —
        --   same machinery spoil promotion uses, so it persists).
    | WorldSave WorldPageId Text Text [LuaComponentSpec]
        [LuaRefEdge] (Maybe AutosaveRequest)
        -- ^ pageId, save-name, request-timestamp (ISO 8601 microsecond
        --   precision, monotonically clamped), every currently-
        --   registered Lua save component (bare registry name, schema
        --   version, required flag, already-encoded payload — issue
        --   #761, save-overhaul B3), and
        --   every reference edge those components' @references()@
        --   hooks reported on the SAME live snapshot (component id,
        --   kind, target id, optional owning-unit id, source field
        --   path, and (#915) the edge's own declared world page for the
        --   one kind whose id is page-local — issue #764,
        --   save-overhaul C3, the same record the load path's
        --   'prepareLuaLoad' returns; the path replaces a
        --   synthetic "kind#id" diagnostic path with the actual field
        --   the edge came from). Both payloads are the canonical
        --   records from the LEAF module "World.Save.Payload" (issue
        --   #1103) — naming them costs this module no dependency on
        --   the Save/Envelope module graph, which is why they are no
        --   longer re-spelled here as anonymous tuples.
        --   The Lua side captures the
        --   timestamp at request time (so two saves queued close
        --   together get distinct timestamps reflecting when the
        --   player asked, not whenever the world thread happened to
        --   process them) and calls @saveModules.snapshotAll()@ before
        --   queueing this command, aborting the save entirely rather
        --   than enqueueing it if any REQUIRED Lua component failed to
        --   snapshot. The trailing 'AutosaveRequest' (#913) is 'Just'
        --   exactly for a save the interval autosave scheduler asked
        --   for: it carries the durable autosave classification into
        --   the save's metadata AND the pre-request pause / visible-page
        --   time scale / player-intent generation the world thread
        --   restores from once the transaction succeeds. 'Nothing' for
        --   every manual save, which is classified manual and never
        --   touches the pause the save path imposed.
    | WorldLoadTransaction Int SaveData MaterialRegistry
        -- ^ requestId, decoded + content-validated 'SaveData', and the
        --   'MaterialRegistry' 'Engine.Scripting.Lua.API.Save.continueLoad'
        --   already built (and validated the save's material references
        --   against) off to the side, WITHOUT touching the live
        --   'Engine.Core.State.materialRegistryRef' (issue #763: the
        --   registry is otherwise only populated by
        --   @world.init@, so validating against a freshly-built one is
        --   required — but writing it to the live ref before the load
        --   is even known to succeed would discard any runtime/custom
        --   material registrations the OLD, still-paused session had,
        --   violating "no live mutation before commit" same as every
        --   other piece of session state). Stages the complete
        --   replacement session ("World.Load.Stage") without touching
        --   any live ref (requirement 6) — the staged result lands in
        --   'Engine.Core.State.pendingLoadRef', keyed by requestId, and
        --   a 'LuaStagingComplete' message is posted so the Lua thread
        --   can drive the publish barrier
        --   ("Engine.Scripting.Lua.Thread.Dispatch").
    | WorldLoadPublish Int
        -- ^ requestId. Atomically publishes the 'World.Load.Types.StagedSession'
        --   the matching 'WorldLoadTransaction' produced
        --   ("World.Load.Publish") — issued ONLY while the save
        --   barrier's capture lock is held (mirrors 'WorldSave''s
        --   authorized-command handling in "World.Thread").
    | WorldDeleteTile WorldPageId Int Int      -- ^ worldId, gx, gy
    | WorldSetFluidTile WorldPageId Int Int FluidType
        -- ^ worldId, gx, gy, fluid kind. Sets one tile of fluid at
        --   surfaceZ + 1 on the given column. Idempotent; replaces any
        --   existing fluid cell. Currently a debug-tool affordance.
    | WorldSetSlope WorldPageId Int Int Int Word8
        -- ^ worldId, gx, gy, z, slope bitmask (0=N 1=E 2=S 3=W). Sets the
        --   walkable-ramp slope bits of an existing tile via the WeSetSlope
        --   edit path. No generator emits this — it exists for movement
        --   test harnesses, since addTile only ever produces flat tops.
    | WorldSetCell WorldPageId Int Int Int MaterialId
        -- ^ worldId, gx, gy, z, material (id 0 = air). Sets a single 3D
        --   cell via the WeSetCell edit path — the locations primitive for
        --   carving interior air, walls, ceilings, staircases. Grows the
        --   column up to reach z; z below the column floor is a no-op.
    | WorldSetStructure WorldPageId Int Int Word8 Int Int Int StructureStageToken
        -- ^ worldId, gx, gy, slot-tag, texture palette id, facemap palette
        --   id, z, staging token. Places a structure piece via the
        --   WeSetStructure edit path (per-chunk overlay; persists). The
        --   palette ids are interned Lua-side before queueing; the resolved
        --   cap variant is already baked into facePaletteId (the BUILDER
        --   picks it, not this handler).
        --
        --   The token (#1674) names the ONE 'structure.place' attempt that
        --   staged this piece in 'wsStructureStageRef'. It is what lets the
        --   handler retract exactly that staged entry when it declines the
        --   commit (target chunk not loaded) — the tuple above cannot, since
        --   a later placement at the same tile and slot can carry an
        --   identical one.
    | WorldClearStructure WorldPageId Int Int Word8
        -- ^ worldId, gx, gy, slot-tag. Removes a structure piece.
    | WorldClearAllStructures WorldPageId
        -- ^ worldId. Removes EVERY structure piece in the world: clears the
        --   live per-chunk overlays AND strips all WeSetStructure/
        --   WeClearStructure edits from the log so they don't replay on
        --   eviction/reload. The authoritative "wipe all structures".
    | WorldDestroy !WorldPageId
    | WorldDestroyAll
        -- ^ Tear down EVERY world (Exit to Menu): clears wmWorlds/wmVisible,
        --   sim-deactivates each page, and resets the global unit/building
        --   managers. Without this a hidden world (e.g. a leftover arena)
        --   stays in wmWorlds and resolveActiveWorld's head-fallback keeps
        --   resolving it as the implicit active world behind the menu (#58).
    | WorldApplyFluids !FluidWritebackBatch
        -- ^ Sim → World: apply the sim's settled/active fluid results to
        --   the visible world's 'wsTilesRef'. The world thread is the
        --   SOLE writer of 'wsTilesRef'; the sim never touches it.
    | WorldMarkLocationContentsSpawned WorldPageId LocationInstanceId
        -- ^ worldId, location instance id. One-time content-spawn flag
        --   (#90), per INSTANCE since #911: marks that placed location as
        --   having had its `contents` spawned
        --   ('Location.Instance.liContentsSpawned') — so a later chunk
        --   (re)load never respawns them. Marking one instance never
        --   touches another, including a second instance anchored in the
        --   same chunk. Deliberately independent of the instance's
        --   lifecycle and of the chunk-keyed stamp flag (#424). The world
        --   thread is the sole owner of WorldGenParams; Lua queues this
        --   rather than mutating wsGenParamsRef directly.
    | WorldRegisterLocationEncounterOccupants
        !WorldPageId !LocationInstanceId ![(UnitId, (Float, Float))]
        -- ^ Persist the exact unit ids and spawn homes allocated for one
        --   placed location's ranged encounter. The world queue orders this
        --   before the matching contents-spawn marker, and the instance
        --   mutation is idempotent once the complete roster is installed.
    | WorldSetLocationEncounterOccupantState
        !WorldPageId !LocationInstanceId !UnitId !Bool !Bool
        -- ^ worldId, location instance id, occupant id, engaged, returning.
        --   Mirrors the guard's durable participation/return state into the
        --   placed instance independently of runtime-unit resolution.
    | WorldSetLocationEncounterEpisodeState
        !WorldPageId !LocationInstanceId !Bool !Bool !Bool
        -- ^ worldId, location instance id, episode active,
        --   aggression-announced, disengage-announced. The notice flags are
        --   encounter-wide so several guards joining one episode cannot
        --   produce one event each; activation also drives visible lifecycle.
    | WorldSetLocationLifecycle WorldPageId LocationInstanceId LocationLifecycle
        -- ^ worldId, location instance id, requested lifecycle state
        --   (#911). Applied only when it moves the instance STRICTLY
        --   forward along @unknown → hinted → discovered → active →
        --   cleared → depleted@ ('Location.Instance.promoteLifecycle');
        --   a backward or same-state request is silently refused, so
        --   the one-way discovery guarantee holds no matter who asks.
    | WorldSpawnBoundBuilding !BuildingId !Text !Int !Int !Int !WorldPageId
                              !Word64
        -- ^ A PAGE-BOUND building placement (#1602): pre-allocated id,
        --   defName, canonical anchor gx/gy, floor z, target page, and
        --   the page-SELECTION generation the click that produced it was
        --   hit-tested under.
        --
        --   It lands HERE rather than straight on the building queue for
        --   one reason: this thread is the sole mutator of 'wmVisible'
        --   and therefore of 'wmSelectionGen'
        --   (@handleWorldShow/Hide/InitArenaDone/Destroy/DestroyAll@ and
        --   @publishStagedSession@ all run on it), so a check performed
        --   here cannot be interleaved with a selection change — a
        --   change enqueued before this command is already applied, and
        --   one enqueued after is genuinely after the decision. Checking
        --   on the Lua thread, or on the building drain the unit thread
        --   runs, could only ever be best-effort against a counter
        --   another thread is free to move.
        --
        --   A live binding forwards an ordinary 'BuildingSpawn' to the
        --   building queue (its binding already discharged); a stale one
        --   forwards nothing at all, so the placement lands on neither
        --   the captured page nor the newly selected one.
    | WorldMarkLocationStamped WorldPageId Int Int
                               (Maybe StructureCommitWindow)
        -- ^ worldId, gx, gy, and the span of placement attempts the stamp
        --   invocation made. One-time geometry-stamp flag (#424): marks the
        --   chunk containing (gx, gy) as having had its placed location's
        --   builder COMPLETE — every placement it attempted succeeded
        --   (#1719), not merely that the builder ran — in
        --   'WorldGenParams.wgpLocationStamped', so a later chunk (re)load
        --   never re-stamps it, even if the player has since cleared the
        --   anchor floor tile the old structure.hasAt guard relied on. A
        --   partial stamp deliberately never reaches this command: the Lua
        --   stamper leaves the chunk unmarked so the every-load dispatch in
        --   "World.Thread.ChunkLoading" retries it. The world thread is the
        --   sole owner of WorldGenParams; Lua queues this rather than
        --   mutating wsGenParamsRef directly.
        --
        --   __The Lua-side answer is only half of it (#2051).__ Every
        --   @structure.place@ returning true means the placement was
        --   ACCEPTED — staged and queued — not that it committed; the
        --   target chunk can still evict before the world thread's own
        --   residency check, which declines the commit and appends no
        --   edit. So the command carries the invocation's
        --   'StructureCommitWindow', and the handler withholds the marker
        --   when any attempt in it was declined. 'Nothing' means no window
        --   was supplied (the console verb, and a caller that placed
        --   through no window at all) and marks unconditionally, exactly
        --   as this command always did.
    deriving (Show)
